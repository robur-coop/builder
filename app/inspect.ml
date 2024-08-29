let prepare_console out started now res =
  let cons =
    List.map (fun (delta, txt) ->
        Printf.sprintf "%dms: %S" (Duration.to_ms (Int64.of_int delta)) txt)
      (List.rev out)
  in
  let started = "started at " ^ Ptime.to_rfc3339 started
  and stopped = "stopped at " ^ Ptime.to_rfc3339 now
  and exited = Fmt.to_to_string Builder.pp_execution_result res
  in
  started :: cons @ [ exited ; stopped ]

let jump () file console script output print_job =
  let ( let* ) = Result.bind in
  let* data = Bos.OS.File.read (Fpath.v file) in
  let* job, _uuid, out, started, now, res, data =
    Builder.Asn.exec_of_str data
  in
  if print_job then
    Logs.app (fun m -> m "%s on %s" job.Builder.name job.Builder.platform);
  if console then begin
    let lines = prepare_console out started now res in
    List.iter (fun l -> Logs.app (fun m -> m "%s" l)) lines
  end;
  if script then Logs.app (fun m -> m "%s" job.Builder.script);
  (match output with
   | None -> ()
   | Some dir ->
     let t = Fpath.v dir in
     List.iter (fun (path, value) ->
         let p = Fpath.append t path in
         ignore (Bos.OS.Dir.create (Fpath.parent p));
         ignore (Bos.OS.File.write p value))
       data);
  Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let console =
  let doc = "Print console output" in
  Arg.(value & flag & info [ "console" ] ~doc)

let script =
  let doc = "Print script" in
  Arg.(value & flag & info [ "script" ] ~doc)

let output =
  let doc = "Output files into a directory" in
  Arg.(value & opt (some dir) None & info [ "output" ] ~doc ~docv:"DIR")

let job =
  let doc = "Print job information" in
  Arg.(value & flag & info [ "job" ] ~doc)

let file =
  let doc = "The file name to inspect" in
  Arg.(required & pos 0 (some file) None & info [ ] ~doc ~docv:"FILE")

let cmd =
  let term =
    Term.(term_result (const jump $ setup_log $ file $ console $ script $ output $ job))
  and info = Cmd.info "builder-inspect" ~version:Builder.version
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
