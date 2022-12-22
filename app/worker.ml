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
    match Builder.write_cmd s (Builder.Output (uuid, line)) with
    | Ok () -> read_write ()
    | Error `Msg msg ->
      Logs.err (fun m -> m "communication with server failed %s" msg);
      Unix.kill 0 9;
      exit 1
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
        Unix.create_process "/bin/sh" [| "-ec" ; "timeout 1h " ^ toexec |] Unix.stdin out out
      in
      let r = Unix.waitpid [] pid in
      (try
         Unix.close out;
         ignore (Unix.waitpid [] f)
       with Unix.Unix_error(_, _, _) as e ->
         Logs.warn (fun m -> m "exception during close or waitpid %s"
                      (Printexc.to_string e)));
      let res = match snd r with
        | Unix.WEXITED c -> Builder.Exited c, collect_output tmpdir
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

     if server communication fails, exit
  *)
  let* s =
    try
      let sockaddr = Unix.ADDR_INET (host, port) in
      let s = Unix.(socket PF_INET SOCK_STREAM 0) in
      try
        Unix.(connect s sockaddr);
        Ok s
      with e -> Unix.close s; raise e
    with
    | Unix.Unix_error (err, f, _) ->
      Logs.err (fun m -> m "unix error in %s: %s" f (Unix.error_message err));
      Error (`Msg "connect failure")
  in
  let r =
    let* () = Builder.(write_cmd s (Client_hello (`Worker, worker_version))) in
    Logs.debug (fun m -> m "waiting for server hello");
    let* cmd = Builder.read_cmd s in
    let* () = if cmd = Builder.Server_hello then Ok () else Error (`Msg "bad command (expected server hello") in
    let* () = Builder.write_cmd s (Builder.Job_requested platform) in
    Logs.debug (fun m -> m "waiting for job");
    let* cmd = Builder.read_cmd s in
    match cmd with
    | Builder.Job_schedule (uuid, job) ->
      Logs.app (fun m -> m "%d received job uuid %a: %a" (Unix.getpid ())
                  Uuidm.pp uuid Builder.pp_script_job job);
      let r, data = execute_job s uuid job in
      Logs.debug (fun m -> m "executed job");
      Builder.(write_cmd s (Job_finished (uuid, r, data)))
    | cmd ->
      Logs.err (fun m -> m "expected Job, got %a" Builder.pp_cmd cmd);
      Error (`Msg "bad communication")
  in
  Unix.close s;
  Result.iter_error (function `Msg msg -> Logs.err (fun m -> m "error %s" msg)) r;
  r

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
  let term = Term.(term_result (const jump $ setup_log $ platform $ remote))
  and info = Cmd.info "builder-worker" ~version:Builder.version
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
