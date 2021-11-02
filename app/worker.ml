let ( let* ) = Result.bind

let sh = "script.sh"

let prng = Random.State.make_self_init ()

let rec tmp_dirname () =
  let rnd = Random.State.bits prng land 0xFFFFFF in
  let name =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "builder-%06x" rnd)
  in
  try
    let _stat = Unix.lstat name
    in
    tmp_dirname ()
  with
    _ -> name

let read_console_write_network s fd uuid =
  let ch = Unix.in_channel_of_descr fd in
  let rec read_write () =
    let line = input_line ch in
    Builder.write_cmd s (Builder.Output (uuid, line)) |> ignore; (* TODO *)
    read_write ()
  in
  try read_write () with End_of_file -> exit 0

let prepare_fs job =
  let tmpdir = Fpath.v (tmp_dirname ()) in
  let* did_not_exist = Bos.OS.Dir.create tmpdir in
  if not did_not_exist then
    Error (`Msg "path already existed")
  else
    let* () = Bos.OS.Dir.set_current tmpdir in
    let* () = Bos.OS.File.write ~mode:500 Fpath.(tmpdir / sh) job.Builder.script in
    Ok tmpdir

let collect_output tmpdir =
  let all_files =
    let dirs = [ tmpdir ] in
    let collect path acc = path :: acc in
    match Bos.OS.Path.fold ~elements:`Files collect [] dirs with
    | Ok files -> files
    | Error `Msg msg ->
      Logs.warn (fun m -> m "folding resulted in an error %s" msg);
      []
  in
  List.fold_left (fun acc f ->
      match Fpath.rem_prefix tmpdir f with
      | None ->
        Logs.warn (fun m -> m "couldn't remove tmpdir prefix from %a"
                      Fpath.pp f);
        acc
      | Some name when Fpath.to_string name = sh ->
        (* ignoring the script.sh itself *)
        acc
      | Some name ->
        match Bos.OS.File.read f with
        | Ok data -> (name, data) :: acc
        | Error `Msg e ->
          Logs.err (fun m -> m "error reading %a: %s" Fpath.pp f e);
          acc)
    [] all_files

let execute_job s uuid job =
  match prepare_fs job with
  | Error `Msg msg -> Builder.Msg msg, []
  | Ok tmpdir ->
    let to_read, out = Unix.pipe () in
    let f = Unix.fork () in
    if f = 0 then begin
      (* child *)
      Unix.close out;
      read_console_write_network s to_read uuid
    end else (* parent *)
      let toexec = Fpath.(to_string (tmpdir / sh)) in
      let pid =
        Unix.create_process "/bin/sh" [| "-e" ; toexec |] Unix.stdin out out
      in
      let r = Unix.waitpid [] pid in
      Unix.close out;
      let _ = Unix.waitpid [] f in
      (* Unix.kill f 9; *)
      let res = match snd r with
        | Unix.WEXITED 0 -> Builder.Exited 0, collect_output tmpdir
        | Unix.WEXITED c -> Builder.Exited c, []
        | Unix.WSIGNALED s -> Builder.Signalled s, []
        | Unix.WSTOPPED s -> Builder.Stopped s, []
      in
      ignore (Bos.OS.Dir.delete ~recurse:true tmpdir);
      res

let jump () platform (host, port) =
  (* client semantics:
     - 1 connect to server
     - 2 send client hello
     - 3 await server hello, check version agreement
     - 4 send job request
     - 5 read from server until job is received
     - 6 dump files, execute job (pipe + fork to send output to server)
     - 7 send job result to server

     if while in 1-5 server communication fails, start from 1
     if in 6 server communication fails, drop data (for now) [and retry]
     if in 7 server communication fails, retry until publishing result is done
  *)
  let connect () =
    try
      let sockaddr = Unix.ADDR_INET (host, port) in
      let s = Unix.(socket PF_INET SOCK_STREAM 0) in
      Unix.(connect s sockaddr);
      Ok s
    with
    | Unix.Unix_error (err, f, _) ->
      Logs.err (fun m -> m "unix error in %s: %s" f (Unix.error_message err));
      Error (`Msg "connect failure")
  and disconnect s =
    Unix.close s
  and timeout () = Unix.sleepf 2.
  in
  let disc_on_err s = function Ok x -> Ok x | Error _ as e -> disconnect s; e in
  let init () =
    let* s = connect () in
    let hello = Builder.(Client_hello (`Worker, worker_version)) in
    let* () = disc_on_err s (Builder.write_cmd s hello) in
    let* cmd = disc_on_err s (Builder.read_cmd s) in
    Ok (s, cmd)
  in
  let rec establish () =
    match init () with
    | Error `Msg e ->
      Logs.warn (fun m -> m "error %s connecting, trying again in a bit" e);
      timeout ();
      establish ()
    | Ok cmd -> Ok cmd
  in
  let good_server_hello s = function
    | Builder.Server_hello -> Ok ()
    | cmd ->
      Logs.err (fun m -> m "expected Server Hello with matching version, got %a"
                   Builder.pp_cmd cmd);
      disconnect s;
      Error (`Msg "bad communication")
  in
  let rec hs () =
    let* s, cmd = establish () in
    let* () = good_server_hello s cmd in
    match
      disc_on_err s (
        let* () = Builder.write_cmd s (Builder.Job_requested platform) in
        Builder.read_cmd s)
    with
    | Ok cmd -> Ok (s, cmd)
    | Error `Msg msg ->
      Logs.warn (fun m -> m "received error %s while waiting for job, retry" msg);
      hs ()
  in
  let submit_success s fini =
    match Builder.write_cmd s fini with
    | Ok () -> Ok ()
    | Error `Msg msg ->
      Logs.err (fun m -> m "error %s while submitting result" msg);
      Error (`Msg msg)
  in
  let* s, cmd = hs () in
  let* () =
    match cmd with
    | Builder.Job_schedule (uuid, job) ->
      Logs.app (fun m -> m "received job uuid %a: %a" Uuidm.pp uuid
                   Builder.pp_script_job job);
      let r, data = execute_job s uuid job in
      let fini = Builder.Job_finished (uuid, r, data) in
      submit_success s fini
    | cmd ->
      Logs.err (fun m -> m "expected Job, got %a" Builder.pp_cmd cmd);
      disconnect s;
      Error (`Msg "bad communication")
  in
  disconnect s;
  Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let host_port : (Unix.inet_addr * int) Arg.converter =
  let parse s =
    match String.split_on_char ':' s with
    | [ hostname ;  port ] ->
      begin try
          `Ok (Unix.inet_addr_of_string hostname, int_of_string port)
        with
          Not_found -> `Error "failed to parse IP:port"
      end
    | _ -> `Error "broken: no port specified"
  in
  parse, fun ppf (h, p) -> Format.fprintf ppf "%s:%d"
      (Unix.string_of_inet_addr h) p

let remote =
  let doc = "The remote host:port to connect to" in
  Arg.(value & opt host_port (Unix.inet_addr_loopback, 1234) &
       info [ "r" ; "remote" ] ~doc ~docv:"IP:PORT")

let platform =
  let doc = "The platform this worker is executed on" in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"PLATFORM")

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let cmd =
  Term.(term_result (const jump $ setup_log $ platform $ remote)),
  Term.info "builder-worker" ~version:Builder.version

let () = match Term.eval cmd with `Ok () -> exit 0 | _ -> exit 1
