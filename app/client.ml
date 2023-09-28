let ( let* ) = Result.bind

let connect (host, port) =
  let* s =
    try
      let sockaddr = Unix.ADDR_INET (host, port) in
      let s = Unix.(socket PF_INET SOCK_STREAM 0) in
      try
        Unix.(connect s sockaddr);
        Ok s
      with e -> Unix.close s ; raise e
    with
    | Unix.Unix_error (err, f, _) ->
      Error (`Msg (Fmt.str "connect unix error in %s: %s" f (Unix.error_message err)))
  in
  let* () = Builder.(write_cmd s (Client_hello (`Client, client_version))) in
  match Builder.read_cmd s with
  | Ok Builder.Server_hello -> Ok s
  | Ok cmd ->
    Error (`Msg (Fmt.str "expected Server Hello, got %a" Builder.pp_cmd cmd))
  | Error _ as e -> e

let teardown s =
  let r = match Builder.read_cmd s with
    | Ok Success -> Ok ()
    | Ok Failure msg ->
      Error (`Msg msg)
    | Ok cmd ->
      Error (`Msg (Fmt.str "expected success or failure, received %a" Builder.pp_cmd cmd))
    | Error `Msg msg ->
      Error (`Msg msg)
  in
  Unix.close s;
  r

let observe_uuid uuid remote =
  let* s = connect remote in
  let* () = Builder.write_cmd s (Builder.Observe uuid) in
  let rec read () =
    let* cmd = Builder.read_cmd s in
    match cmd with
    | Output_timestamped (_uuid, ts, data) ->
      Logs.app (fun m -> m "%a %s" Duration.pp ts data);
      read ()
    | _ ->
      if cmd <> Builder.Success then
        Logs.warn (fun m -> m "expected output, got %a" Builder.pp_cmd cmd);
      Unix.close s;
      Ok ()
  in
  read ()

let observe_latest () remote platform job_name =
  let* reply =
    let* s = connect remote in
    let* () = Builder.write_cmd s Builder.Info in
    let r = Builder.read_cmd s in
    ignore (teardown s);
    r
  in
  let is_platform p = match platform with None -> true | Some p' -> String.equal p p' in
  let is_job p = match job_name with None -> true | Some p' -> String.equal p p' in
  match reply with
  | Info_reply { Builder.running ; _ } ->
    (match List.fold_left
             (fun acc ((start, _, _) as x) ->
                match acc with
                | None -> Some x
                | Some (start', _, job) ->
                  if is_platform job.Builder.platform
                  && is_job job.Builder.name
                  && Ptime.is_later start ~than:start'
                  then Some x else acc)
             None running
     with
     | None -> Error (`Msg "No running jobs")
     | Some (_start, uuid, job) ->
       Logs.app (fun m -> m "Observing %s on %s (%a)"
                    job.Builder.name job.Builder.platform Uuidm.pp uuid);
       observe_uuid uuid remote)
  | cmd -> Error (`Msg (Fmt.str "Unexpected reply to 'info': %a" Builder.pp_cmd cmd))

let observe () remote id =
  match Uuidm.of_string id with
  | None -> Error (`Msg "error parsing uuid")
  | Some uuid -> observe_uuid uuid remote

let info_ () remote =
  let* s = connect remote in
  let* () = Builder.write_cmd s Builder.Info in
  let* cmd = Builder.read_cmd s in
  Logs.app (fun m -> m "%a" Builder.pp_cmd cmd);
  teardown s

let unschedule () remote name =
  let* s = connect remote in
  let* () = Builder.write_cmd s (Builder.Unschedule name) in
  teardown s

let execute () remote name platform =
  let* s = connect remote in
  let* () = Builder.write_cmd s (Builder.Execute (name, platform)) in
  teardown s

let schedule () remote name platform script period =
  let* script = Bos.OS.File.read (Fpath.v script) in
  let job = Builder.{ name ; platform ; script } in
  let* s = connect remote in
  let* () = Builder.write_cmd s (Builder.Schedule (period, job)) in
  teardown s

let schedule_orb_build () remote name opam_package period =
  let job = Builder.{ name ; opam_package } in
  let* s = connect remote in
  let* () = Builder.write_cmd s (Builder.Schedule_orb_build (period, job)) in
  teardown s

let reschedule () remote name next period =
  let* s = connect remote in
  let* () = Builder.write_cmd s (Builder.Reschedule (name, next, period)) in
  teardown s

let drop_platform () remote name =
  let* s = connect remote in
  let* () = Builder.write_cmd s (Builder.Drop_platform name) in
  teardown s

let help () man_format cmds = function
  | None -> `Help (`Pager, None)
  | Some t when List.mem t cmds -> `Help (man_format, Some t)
  | Some x ->
    print_endline ("unknown command '" ^ x ^ "', available commands:");
    List.iter print_endline cmds;
    `Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let host_port =
  let parse s =
    match String.split_on_char ':' s with
    | [ hostname ;  port ] ->
      begin try
          Ok (Unix.inet_addr_of_string hostname, int_of_string port)
        with
          Not_found -> Error (`Msg "failed to parse IP:port")
      end
    | _ -> Error (`Msg "broken: no port specified")
  and pp ppf (h, p) =
    Format.fprintf ppf "%s:%d" (Unix.string_of_inet_addr h) p
  in
  Arg.conv (parse, pp)

let remote =
  let doc = "The remote host:port to connect to" in
  let env = Cmd.Env.info "BUILDER_REMOTE" in
  Arg.(value & opt host_port (Unix.inet_addr_loopback, 1234) &
       info [ "r" ; "remote" ] ~env ~doc ~docv:"IP:PORT")

let nam =
  let doc = "The job name" in
  Arg.(required & pos 0 (some string) None & info [ ] ~doc ~docv:"NAME")

let id =
  let doc = "The job ID" in
  Arg.(required & pos 0 (some string) None & info [ ] ~doc ~docv:"ID")

let p =
  Arg.enum [
    "hourly", Builder.Hourly ;
    "daily", Builder.Daily ;
    "weekly", Builder.Weekly ;
    "never", Builder.Never ;
  ]

let period =
  let doc = "The periodic execution interval" in
  Arg.(value & opt p Builder.Daily & info [ "period" ] ~doc ~docv:"PERIOD")

let period_opt =
  let doc = "The periodic execution interval" in
  Arg.(value & opt (some p) None & info [ "period" ] ~doc ~docv:"PERIOD")

let next =
  let ptime =
    let parse s = match Ptime.of_rfc3339 s with
      | Ok (ptime, (None | Some 0), _) ->  Ok ptime
      | Ok (_, _, _) ->  Error (`Msg "I don't like the timezone :(") (* FIXME *)
      | Error (`RFC3339 (_, e)) ->
        Error (`Msg (Fmt.str "bad RFC3339 date: %a" Ptime.pp_rfc3339_error e))
    in
    Arg.conv (parse, Ptime.pp_rfc3339 ())
  in
  let doc = "The next execution time (RFC3339 date)" in
  Arg.(required & pos 1 (some ptime) None & info [ ] ~doc ~docv:"NEXT")

let script =
  let doc = "The script to execute" in
  Arg.(required & pos 2 (some file) None & info [ ] ~doc ~docv:"FILE")

let opam_package =
  let doc = "The opam package to build" in
  Arg.(required & pos 1 (some string) None & info [ ] ~doc ~docv:"OPAM")

let platform_opt =
  let doc = "The platform to observe" in
  Arg.(value & opt (some string) None & info [ "platform" ] ~doc ~docv:"PLATFORM")

let job_name_opt =
  let doc = "The job name to observe" in
  Arg.(value & opt (some string) None & info [ "job" ] ~doc ~docv:"JOB")

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let observe_latest_cmd =
  let term =
    Term.(term_result (const observe_latest $ setup_log $ remote $ platform_opt $ job_name_opt))
  and info = Cmd.info "observe-latest"
  in
  Cmd.v info term

let observe_cmd =
  let term =
    Term.(term_result (const observe $ setup_log $ remote $ id))
  and info = Cmd.info "observe"
  in
  Cmd.v info term

let info_cmd =
  let term =
    Term.(term_result (const info_ $ setup_log $ remote))
  and info = Cmd.info "info"
  in
  Cmd.v info term

let unschedule_cmd =
  let term =
    Term.(term_result (const unschedule $ setup_log $ remote $ nam))
  and info = Cmd.info "unschedule"
  in
  Cmd.v info term

let schedule_cmd =
  let platform =
    let doc = "The platform to schedule the job on" in
    Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"PLATFORM")
  in
  let term =
    Term.(term_result (const schedule $ setup_log $ remote $ nam $ platform $ script $ period))
  and info = Cmd.info "schedule"
  in
  Cmd.v info term

let schedule_orb_build_cmd =
  let term =
    Term.(term_result (const schedule_orb_build $ setup_log $ remote $ nam $ opam_package $ period))
  and info = Cmd.info "orb-build"
  in
  Cmd.v info term

let reschedule_cmd =
  let term =
    Term.(term_result (const reschedule $ setup_log $ remote $ nam $ next $ period_opt))
  and info = Cmd.info "reschedule"
  in
  Cmd.v info term

let execute_cmd =
  let term =
    Term.(term_result (const execute $ setup_log $ remote $ nam $ platform_opt))
  and info = Cmd.info "execute"
  in
  Cmd.v info term

let drop_platform_cmd =
  let platform =
    let doc = "The platform to drop" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"PLATFORM")
  in
  let term =
    Term.(term_result (const drop_platform $ setup_log $ remote $ platform))
  and info = Cmd.info "drop-platform"
  in
  Cmd.v info term

let help_cmd =
  Term.(ret (const help $ setup_log $ Arg.man_format $ Term.choice_names $ Term.const None))

let cmds = [ schedule_cmd ; unschedule_cmd ; info_cmd ; observe_latest_cmd ; observe_cmd ; execute_cmd ; schedule_orb_build_cmd ; reschedule_cmd ; drop_platform_cmd ]

let () =
  let doc = "Builder client" in
  let info = Cmd.info "builder-client" ~version:Builder.version ~doc in
  let group = Cmd.group ~default:help_cmd info cmds in
  exit (Cmd.eval group)
