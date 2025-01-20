open Lwt.Infix

let ( let* ) = Result.bind

let read fd =
  Lwt.catch (fun () ->
      let rec r b ?(off = 0) l =
        if l = 0 then
          Lwt.return (Ok ())
        else
          Lwt_unix.read fd b off l >>= fun read ->
          if read = 0 then
            Lwt.return (Error (`Msg "end of file"))
          else
            r b ~off:(read + off) (l - read)
      in
      let open Lwt_result.Infix in
      let bl = Bytes.create 8 in
      r bl 8 >>= fun () ->
      let l = Bytes.get_int64_be bl 0 in
      let l_int = Int64.to_int l in (* TODO *)
      let b = Bytes.create l_int in
      r b l_int >|= fun () ->
      Bytes.unsafe_to_string b)
    (function
      | Lwt.Canceled as e -> Lwt.reraise e
      | e ->
       Logs.err (fun m -> m "Error while reading: %s" (Printexc.to_string e));
       Lwt.return (Error (`Msg "error in read")))

let read_cmd fd =
  let open Lwt_result.Infix in
  read fd >>= fun data ->
  Lwt.return (Builder.Asn.cmd_of_str data)

let write fd data =
  Lwt.catch (fun () ->
      let rec w b ?(off = 0) l =
        if l = 0 then
          Lwt.return_unit
        else
          Lwt_unix.write fd b off l >>= fun written ->
          w b ~off:(written + off) (l - written)
      in
      let csl = Bytes.create 8 in
      Bytes.set_int64_be csl 0 (Int64.of_int (String.length data));
      w csl 8 >>= fun () ->
      w (Bytes.unsafe_of_string data) (String.length data) >|= fun () ->
      Ok ())
    (fun e ->
       Logs.err (fun m -> m "Error while writing: %s" (Printexc.to_string e));
       Lwt.return (Error (`Msg "unix error in write")))

let write_cmd fd cmd =
  let data = Builder.Asn.cmd_to_str cmd in
  write fd data

let pp_sockaddr ppf = function
  | Lwt_unix.ADDR_UNIX s -> Fmt.pf ppf "unix://%s" s
  | Lwt_unix.ADDR_INET (ip, port) ->
    Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr ip) port

module UM = Map.Make(Uuidm)

module S = Binary_heap.Make (struct
    type t = Builder.schedule_item
    let compare { Builder.next = n1 ; _ } { Builder.next = n2 ; _ } =
      Ptime.compare n1 n2
  end)

let dummy =
  let job = Builder.(Script_job { name = "dummy" ; platform = "dummy-platform" ; script = "#nothing" }) in
  Builder.{ next = Ptime.epoch ; period = Daily ; job }

module SM = Map.Make(String)
module SSet = Set.Make(String)

type output_data = [ `Data of int64 * string | `End of int64 * string ]

type t = {
  mutable queues : Builder.job Queue.t SM.t ;
  mutable schedule : S.t ;
  mutable running : (Ptime.t * Builder.script_job * output_data Lwt_condition.t * (int64 * string) list) UM.t ;
  mutable dropped_platforms : SSet.t;
  waiter : [`New_job | `Platform_removed of string] Lwt_condition.t ;
  dbdir : Fpath.t ;
  upload : string option ;
  he : Happy_eyeballs_lwt.t ;
  cfgdir : Fpath.t ;
}

let template_file dir platform = Fpath.(dir / "orb-build.template" + platform)

let template_exists dir platform =
  template_file dir platform |> Bos.OS.File.must_exist |> Result.is_ok

let find_queue t p =
  match SM.find_opt p t.queues with
  | Some q -> q
  | None ->
    let template_present = template_exists t.cfgdir p in
    let q = Queue.create () in
    if not (SSet.mem p t.dropped_platforms) then
      S.iter (fun { job ; _ } ->
          match Builder.job_platform job with
          | Some plat when String.equal plat p -> Queue.add job q
          | None when template_present -> Queue.add job q
          | _ -> ())
        t.schedule;
    if not (Queue.is_empty q) then begin
      t.queues <- SM.add p q t.queues;
      Lwt_condition.broadcast t.waiter `New_job;
    end;
    q

let p_to_span p =
  let one_hour = 60 * 60
  and to_span = Ptime.Span.of_int_s
  in
  match p with
  | Builder.Hourly -> to_span one_hour
  | Builder.Daily -> to_span (24 * one_hour)
  | Builder.Weekly -> to_span (7 * 24 * one_hour)
  | Builder.Never -> Ptime.(to_span max)

let add_to_queue t platform job =
  match SM.find_opt platform t.queues with
  | Some q ->
    if Queue.fold (fun acc i ->
        if acc then not (Builder.job_equal i job) else acc)
        true q
    then Queue.add job q;
    Lwt_condition.broadcast t.waiter `New_job
  | None -> ()

let move_to_front q q' j =
  Queue.add j q';
  Queue.iter
    (fun job -> if not (Builder.job_equal j job) then Queue.add job q')
    q

let add_to_front_of_queues t name platform =
  let find_job schedule name =
    S.fold (fun { job; _ } (acc : Builder.job option) ->
        let curr = if Builder.job_name job = name then Some job else None in
        match curr, acc with
        | None, None -> None
        | Some job, None | None, Some job -> Some job
        | Some _, Some _ -> assert false) (* XXX: assumption: job names are unique *)
      schedule None
  in
  let recreate_queue j q =
    let q' = Queue.create () in
    move_to_front q q' j;
    q'
  in
  let find_queue platform =
    SM.find_opt platform t.queues
    |> Option.to_result ~none:(`Msg "no such platform")
  in
  let* job = Option.to_result ~none:(`Msg "job not found") (find_job t.schedule name) in
  match Builder.job_platform job, platform with
  | None, None ->
    t.queues <- SM.map (recreate_queue job) t.queues;
    Ok ()
  | Some platform, None | None, Some platform ->
    let* q = find_queue platform in
    t.queues <- SM.add platform (recreate_queue job q) t.queues;
    Ok ()
  | Some platform, Some p when String.equal platform p ->
    let* q = find_queue platform in
    t.queues <- SM.add platform (recreate_queue job q) t.queues;
    Ok ()
  | Some platform, Some _ ->
    Error (`Msg ("job is for a different platform: " ^ platform))

let queue_contains j =
  Queue.fold (fun acc job -> acc || Builder.job_equal j job) false

let add_to_queues t = function
  | Builder.Orb_build_job _ as job ->
    SM.iter (fun _ q ->
      if not (queue_contains job q) then Queue.add job q)
      t.queues;
    Lwt_condition.broadcast t.waiter `New_job
  | Builder.Script_job { platform ; _ } as job ->
    let q = find_queue t platform in
    if not (queue_contains job q) then Queue.add job q;
    Lwt_condition.broadcast t.waiter `New_job

let schedule_job t now period job =
  let next =
    Option.value ~default:Ptime.max
      (Ptime.add_span now (p_to_span period))
  in
  S.add t.schedule Builder.{ next ; period ; job }

let schedule t =
  let now = Ptime_clock.now () in
  let rec s_next modified =
    match S.minimum t.schedule with
    | exception Binary_heap.Empty -> modified
    | Builder.{ next ; period ; job } when Ptime.is_later ~than:next now && period <> Never ->
      S.remove t.schedule;
      schedule_job t now period job;
      add_to_queues t job;
      s_next true
    | _ -> modified
  in
  s_next false

let dump, restore =
  let file = "state" in
  (fun t ->
     let state =
       let queues =
         SM.fold (fun platform q acc ->
           Builder.Queue (platform, Queue.to_seq q |> List.of_seq) :: acc)
           t.queues []
       in
       let and_schedules =
         S.fold (fun s acc -> (Builder.Schedule s) :: acc) t.schedule queues
       in
       SSet.fold (fun platform acc ->
           (Builder.Dropped_platform platform) :: acc)
         t.dropped_platforms
         and_schedules
     in
     let data = Builder.Asn.state_to_str state in
     let bak = Fpath.(t.dbdir / file + "tmp") in
     let* () = Bos.OS.File.write bak data in
     Bos.OS.U.(error_to_msg (rename bak Fpath.(t.dbdir / file)))),
  (fun upload dbdir cfgdir ->
     let* _ = Bos.OS.Dir.create dbdir in
     let to_read = Fpath.(dbdir / file) in
     let queues = SM.empty
     and schedule = S.create ~dummy 13
     and dropped_platforms = SSet.empty
     and waiter = Lwt_condition.create ()
     and he = Happy_eyeballs_lwt.create ()
     in
     let* t_r_exists = Bos.OS.File.exists to_read in
     if not t_r_exists then begin
       Logs.warn (fun m -> m "state file does not exist, using empty");
       Ok { queues ; schedule ; dropped_platforms ; running = UM.empty ; waiter ; dbdir ; upload ; he ; cfgdir }
     end else
       let* data = Bos.OS.File.read to_read in
       let* items = Builder.Asn.state_of_str data in
       let queues, schedule, dropped_platforms =
         List.fold_left (fun (queues, schedule, dropped_platforms) -> function
             | Builder.Queue (platform, jobs) ->
               let q = List.to_seq jobs |> Queue.of_seq in
               (SM.add platform q queues, schedule, dropped_platforms)
             | Builder.Schedule s ->
               S.add schedule s;
               (queues, schedule, dropped_platforms)
             | Builder.Dropped_platform platform ->
               let dropped_platforms = SSet.add platform dropped_platforms in
               (queues, schedule, dropped_platforms))
           (queues, schedule, dropped_platforms) items
       in
       Ok { queues ; schedule ; dropped_platforms ; running = UM.empty ; waiter ; dbdir ; upload ; he ; cfgdir })

let uuid_gen = Uuidm.v4_gen (Random.State.make_self_init ())

let save_to_disk dir (((job : Builder.script_job), uuid, _, _, _, _, _) as full) =
  let out_dir = Fpath.(dir / job.Builder.name / Uuidm.to_string uuid) in
  Logs.info (fun m -> m "saving result to %a" Fpath.pp out_dir);
  let* _ = Bos.OS.Dir.create out_dir in
  let full = Builder.Asn.exec_to_str full in
  Bos.OS.File.write Fpath.(out_dir / "full") full

let upload happy_eyeballs url dir full =
  let body = Builder.Asn.exec_to_str full in
  let body_f _ acc data = Lwt.return (acc ^ data) in
  Http_lwt_client.request ~happy_eyeballs ~meth:`POST ~body url body_f "" >|= function
  | Ok (resp, body) ->
    if Http_lwt_client.Status.is_successful resp.Http_lwt_client.status then begin
      Logs.info (fun m -> m "successful upload (HTTP %s)"
                    (Http_lwt_client.Status.to_string resp.Http_lwt_client.status));
      Ok ()
    end else begin
      Logs.err (fun m -> m "upload failed (HTTP %s, body: %s), saving to %a"
                   (Http_lwt_client.Status.to_string resp.Http_lwt_client.status)
                   body Fpath.pp dir);
      save_to_disk dir full
    end
  | Error `Msg e ->
    Logs.err (fun m -> m "upload failed %s, saving to %a" e Fpath.pp dir);
    save_to_disk dir full

let job_finished state uuid res data =
  let r = UM.find_opt uuid state.running in
  state.running <- UM.remove uuid state.running;
  match r with
  | None ->
    Logs.err (fun m -> m "no job found for uuid %a" Uuidm.pp uuid);
    Lwt.return_unit
  | Some (started, job, cond, out) ->
    let now = Ptime_clock.now () in
    let res_ts =
      let delta = Ptime.diff now started in
      Duration.of_f (Ptime.Span.to_float_s delta)
    in
    let res_str = Fmt.to_to_string Builder.pp_execution_result res in
    Lwt_condition.broadcast cond (`End (res_ts, res_str));
    let full =
      let out = List.rev_map (fun (d, d') -> Int64.to_int d, d') out in
      let out = List.rev out in
      job, uuid, out, started, now, res, data
    in
    (match state.upload with
     | None -> Lwt.return (save_to_disk state.dbdir full)
     | Some url -> upload state.he url state.dbdir full) >|= function
    | Ok () -> ()
    | Error (`Msg msg) ->
      Logs.err (fun m -> m "error saving %s (%a) to disk: %s"
                   job.Builder.name Uuidm.pp uuid msg)

let read_template dir platform =
  Bos.OS.File.read (template_file dir platform)

let job_to_script_job dir platform = function
  | Builder.Script_job j ->
    if j.Builder.platform = platform then
      Ok j
    else
      Error (`Msg (Fmt.str "script platform %S does not match worker platform %S"
                     j.Builder.platform platform))
  | Builder.Orb_build_job { name ; opam_package } ->
    let* template = read_template dir platform in
    let script =
      Astring.String.(concat ~sep:opam_package (cuts ~sep:"%%OPAM_PACKAGE%%" template))
    in
    let script =
      Astring.String.(concat ~sep:platform (cuts ~sep:"%%PLATFORM%%" script))
    in
    Ok { Builder.name ; platform ; script }

let reschedule_job t name f =
  let s = S.create ~dummy 13 in
  (* first we look through the schedule to find <name> *)
  match S.fold (fun ({ Builder.job ; period ; next } as si) j ->
      match j, String.equal (Builder.job_name job) name with
      | None, true -> Some (job, period, next)
      | Some _, true ->
        (* violates our policy: there ain't multiple jobs with same name *)
        assert false
      | _, false -> S.add s si; j)
      t.schedule None
  with
  | None -> Error (`Msg ("couldn't find job " ^ name))
  | Some (job, period, _next) ->
    t.schedule <- s;
    let period, next = f period in
    schedule_job t next period job;
    Ok job

let worker_loop t addr fd =
  read_cmd fd >>= function
  | Ok Builder.Job_requested platform ->
    begin
      Logs.app (fun m -> m "job requested for %S" platform);
      let rec find_job = function
        | `Platform_removed p ->
          if String.equal p platform then
            Logs.warn (fun m -> m "Platform %s removed; remember to shut down worker at %a"
                          platform pp_sockaddr addr);
          (* FIXME: wait forever-ish *)
          Lwt_condition.wait t.waiter >>= find_job
        | `New_job ->
          let queue = find_queue t platform in
          match Queue.take_opt queue with
          | None ->
            if not (template_exists t.cfgdir platform) then
              Logs.warn (fun m -> m "no template for %S" platform);
            Lwt_condition.wait t.waiter >>= find_job
          | Some job -> Lwt.return job
      in
      find_job `New_job >>= fun job ->
      ignore (dump t);
      let uuid = uuid_gen () in
      let put_back_on_err f =
        f >|= function
        | Ok a -> Ok a
        | Error `Msg err ->
          Logs.warn (fun m -> m "%a communication failure %s with %a, job %a put back"
                        Uuidm.pp uuid
                        err Builder.pp_job job pp_sockaddr addr);
          t.running <- UM.remove uuid t.running;
          add_to_queue t platform job;
          ignore (dump t);
          Error (`Msg err)
      in
      Lwt_result.lift (job_to_script_job t.cfgdir platform job) >>= function
      | Error `Msg msg ->
        Logs.warn (fun m -> m "%a error converting job %a to script job: %s"
                      Uuidm.pp uuid Builder.pp_job job msg);
        Lwt.return_unit
      | Ok script_job ->
        Logs.app (fun m -> m "%a on platform %S job scheduled %a for %a"
                     Uuidm.pp uuid platform Builder.pp_job job pp_sockaddr addr);
        put_back_on_err (write_cmd fd (Builder.Job_schedule (uuid, script_job))) >>= function
        | Error _ -> Lwt.return_unit
        | Ok () ->
          t.running <- UM.add uuid (Ptime_clock.now (), script_job, Lwt_condition.create (), []) t.running;
          (* await output *)
          let timeout () =
            (* an hour should be enough for a job *)
            let timeout = Duration.(to_f (of_hour 1)) in
            Lwt_unix.sleep timeout >|= fun () ->
            Logs.warn (fun m -> m "%a timeout after %f seconds" Uuidm.pp uuid timeout);
            `Timeout
          in
          let read_worker () =
            let rec read_and_process_data () =
              put_back_on_err (read_cmd fd) >>= function
              | Ok Builder.Output (uuid', data) ->
                Logs.debug (fun m -> m "%a output %S" Uuidm.pp uuid data);
                if not (Uuidm.equal uuid uuid') then
                  Logs.warn (fun m -> m "%a got uuid %a" Uuidm.pp uuid Uuidm.pp uuid');
                (match UM.find_opt uuid t.running with
                 | None ->
                   Logs.err (fun m -> m "%a unknown in uuid map, discarding %S"
                                Uuidm.pp uuid data)
                 | Some (created, job, cond, out) ->
                   let ts =
                     let delta = Ptime.diff (Ptime_clock.now ()) created in
                     Duration.of_f (Ptime.Span.to_float_s delta)
                   in
                   Lwt_condition.broadcast cond (`Data (ts, data));
                   let value = created, job, cond, (ts, data) :: out in
                   t.running <- UM.add uuid value t.running);
                read_and_process_data ()
              | Ok Builder.Job_finished (uuid', r, data) ->
                if not (Uuidm.equal uuid uuid') then
                  Logs.warn (fun m -> m "%a got uuid %a" Uuidm.pp uuid Uuidm.pp uuid');
                Logs.app (fun m -> m "%a finished with %a" Uuidm.pp uuid
                             Builder.pp_execution_result r);
                job_finished t uuid r data
              | Ok cmd ->
                Logs.err (fun m -> m "%a expected output or job finished, got %a"
                             Uuidm.pp uuid Builder.pp_cmd cmd);
                Lwt.return_unit
              | Error _ -> Lwt.return_unit
            in
            read_and_process_data () >|= fun () ->
            `Done
          in
          Lwt.pick [ timeout () ; read_worker () ] >>= function
          | `Timeout ->
            Logs.warn (fun m -> m "%a timed out" Uuidm.pp uuid);
            Lwt_unix.close fd >>= fun () ->
            job_finished t uuid (Builder.Msg "timeout") [] >|= fun () ->
            add_to_queue t platform job;
            ignore (dump t)
          | `Done -> Lwt_unix.close fd
    end
  | Ok cmd ->
    Logs.err (fun m -> m "unexpected %a" Builder.pp_cmd cmd);
    Lwt.return_unit
  | Error `Msg msg ->
    Logs.err (fun m -> m "error %s while reading from worker" msg);
    Lwt.return_unit

let maybe_schedule_job t p j =
  if
    S.fold
      (fun { Builder.job ; _ } acc ->
         if acc then not (Builder.job_equal job j) else acc)
      t.schedule true
  then
    let now = Ptime_clock.now () in
    schedule_job t now p j;
    add_to_queues t j;
    ignore (dump t);
    Lwt.return (Ok ())
  else
    Lwt.return (Error (`Msg "job name already used"))

let client_loop t fd =
  let open Lwt_result.Infix in
  read_cmd fd >>= function
  | Builder.Schedule (_, {platform; _}) when SSet.mem platform t.dropped_platforms ->
    Lwt.return (Error (`Msg "platform dropped; can't schedule jobs"))
  | Builder.Schedule (p, j) ->
    Logs.app (fun m -> m "%a schedule %a" Builder.pp_period p
                 Builder.pp_script_job j);
    maybe_schedule_job t p (Builder.Script_job j)
  | Builder.Schedule_orb_build (p, j) ->
    Logs.app (fun m -> m "%a schedule orb build %a" Builder.pp_period p
                 Builder.pp_orb_build_job j);
    maybe_schedule_job t p (Builder.Orb_build_job j)
  | Builder.Unschedule name ->
    Logs.app (fun m -> m "unschedule %s" name);
    let changed = ref false in
    let schedule =
      let s = S.create ~dummy 13 in
      S.iter (fun ({ Builder.job ; _ } as si) ->
          if not (String.equal (Builder.job_name job) name) then
            S.add s si
          else changed := true)
        t.schedule;
      s
    in
    if !changed then begin
      let queues =
        SM.map (fun queue ->
          let q = Queue.create () in
          Queue.iter (fun job ->
              if not (String.equal (Builder.job_name job) name) then
                Queue.add job q
              else
                ())
            queue;
          q)
        t.queues
      in
      t.schedule <- schedule;
      t.queues <- queues;
      ignore (dump t);
      Lwt.return (Ok ())
    end else
      Lwt.return (Error (`Msg ("unknown job " ^ name)))
  | Builder.Execute (name, platform) as cmd ->
    Logs.app (fun m -> m "%a" Builder.pp_cmd cmd);
    let r = add_to_front_of_queues t name platform in
    Result.fold r
      ~ok:(fun () ->
        ignore (dump t);
        Lwt_condition.broadcast t.waiter `New_job;
        Lwt.return (Ok ()))
      ~error:(fun (`Msg m) ->
        Lwt.return (Error (`Msg ("execute failed: " ^ m))))
  | Builder.Reschedule (name, next, period) ->
    Logs.app (fun m -> m "reschedule %s: %a" name (Ptime.pp_rfc3339 ()) next);
    begin match
        reschedule_job t name (fun orig_period ->
          Option.value ~default:orig_period period, next)
      with
      | Ok j -> add_to_queues t j; ignore (dump t); Lwt.return (Ok ())
      | Error e -> Lwt.return (Error e)
    end
  | Builder.Info ->
    Logs.app (fun m -> m "info");
    let reply =
      let schedule = S.fold (fun s acc -> s :: acc) t.schedule []
      and queues = SM.fold (fun platform q acc ->
        (platform, List.rev (Queue.fold (fun acc j -> j :: acc) [] q)) :: acc)
        t.queues []
      and running =
        UM.fold (fun uuid (started, job, _, _) acc ->
            (started, uuid, job) :: acc)
          t.running []
      in
      Builder.{ schedule ; queues ; running ; dropped_platforms = SSet.elements t.dropped_platforms }
    in
    write_cmd fd (Builder.Info_reply reply)
  | Builder.Observe id ->
    (* two cases: still running or already done *)
    begin match UM.find_opt id t.running with
      | Some (_, _, cond, out) ->
        let open Lwt.Infix in
        let output id ts data = Builder.Output_timestamped (id, ts, data) in
        Lwt_list.iter_s (fun (ts, l) ->
            write_cmd fd (output id ts l) >|= ignore)
          (List.rev out) >>= fun () ->
        let q = Queue.create () in
        let q_cond = Lwt_condition.create () in
        let rec more () =
          Lwt_condition.wait cond >>= function
          | `End _ as ev ->
            Queue.add ev q;
            Lwt_condition.signal q_cond ();
            Lwt.return_unit
          | `Data _ as ev ->
            if Queue.length q < 100 then begin
              Queue.add ev q;
              Lwt_condition.signal q_cond ();
              more ()
            end else
              (* Drop data messages if the client can't keep up *)
              more ()
        in
        let rec send () =
          if Queue.is_empty q then
            Lwt_condition.wait q_cond >>= fun () ->
            send ()
          else
            match Queue.take q with
            | `End (ts, data) ->
              write_cmd fd (output id ts data)
            | `Data (ts, data) ->
              write_cmd fd (output id ts data) >>= function
              | Ok () -> send ()
              | Error _ -> Lwt.return (Ok ())
        in
        let more = more () in
        send () >>= fun r -> Lwt.cancel more; Lwt.return r
      | None -> Lwt.return (Error (`Msg "uuid not found"))
    end
  | Builder.Drop_platform p ->
    if SM.mem p t.queues then begin
      t.queues <- SM.remove p t.queues;
      t.dropped_platforms <- SSet.add p t.dropped_platforms;
      let s = S.create ~dummy 13 in
      S.iter (fun ({ job ; _ } as si) ->
        match Builder.job_platform job with
        | Some plat when String.equal plat p -> ()
        | _ -> S.add s si)
      t.schedule;
      t.schedule <- s;
      ignore (dump t);
      Lwt_condition.broadcast t.waiter (`Platform_removed p);
      Lwt.return (Ok ())
    end else
      Lwt.return (Error (`Msg ("unknown platform " ^ p)))
  | Builder.Undrop_platform p ->
    if SSet.mem p t.dropped_platforms then begin
      t.dropped_platforms <- SSet.remove p t.dropped_platforms;
      ignore (dump t);
      Lwt_condition.broadcast t.waiter `New_job;
      Lwt.return (Ok ())
    end else
      Lwt.return (Error (`Msg ("unknown platform " ^ p)))
  | cmd ->
    Logs.err (fun m -> m "unexpected %a" Builder.pp_cmd cmd);
    Lwt_result.lift (Error (`Msg "bad communication"))

let handle t fd addr =
  (* -- client connection:
     (1) read client hello (or client hello 2)
     (2) send server hello
     -- now there are different paths:
     (3) read request job
     (4) send job
     (5) await job done
     -- or scheduling a job
     (3) read schedule
     -- or unscheduling a job
     (3) read unschedule
     -- or info
     (3) read info
     (4) send info_reply
     -- or observing
     (3) read observe
     (4) send outputs and job done
  *)
  Logs.app (fun m -> m "client connection from %a" pp_sockaddr addr);
  read_cmd fd >>= function
  | Ok (Builder.Client_hello (`Client, n)) when n = Builder.client_version ->
    begin
      write_cmd fd Builder.Server_hello >>= function
      | Error _ -> Lwt.return_unit
      | Ok () ->
        client_loop t fd >>= fun r ->
        let to_client = match r with
          | Ok _ -> Builder.Success
          | Error `Msg msg ->
            Logs.info (fun m -> m "error while processing client command %s" msg);
            Builder.Failure msg
         in
         write_cmd fd to_client >|= fun _ ->
         ()
    end
  | Ok (Builder.Client_hello (`Worker, n)) when n = Builder.worker_version ->
    begin
      write_cmd fd Builder.Server_hello >>= function
      | Error _ -> Lwt.return_unit
      | Ok () -> worker_loop t addr fd
    end
  | Ok cmd ->
    Logs.err (fun m -> m "expected client hello, got %a" Builder.pp_cmd cmd);
    Lwt.return_unit
  | Error `Msg msg ->
    Logs.err (fun m -> m "error %s while reading client hello" msg);
    Lwt.return_unit

let jump () ip port dbdir cfgdir url =
  Lwt_main.run
    (Sys.(set_signal sigpipe Signal_ignore);
     let d = Fpath.v dbdir
     and cfg = Fpath.v cfgdir
     in
     Lwt_result.lift (restore url d cfg) >>= fun state ->
     (match state with
      | Ok s -> Lwt.return s | Error `Msg m -> Lwt.fail_with m) >>= fun state ->
     let modified = schedule state in
     ignore (if modified then dump state else Ok ());
     let s = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
     Lwt_unix.(setsockopt s SO_REUSEADDR true);
     Lwt_unix.(bind s (ADDR_INET (Unix.inet_addr_of_string ip, port))) >>= fun () ->
     Lwt_unix.listen s 10;
     let _ev =
       Lwt_engine.on_timer 60. true (fun _ev ->
           let modified = schedule state in
           ignore (if modified then dump state else Ok ()))
     in
     Lwt.catch (fun () ->
         let rec loop () =
           Lwt_unix.accept s >>= fun (fd, addr) ->
           Lwt.async (fun () ->
             handle state fd addr >>= fun () ->
             Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit));
           loop ()
         in
         loop ())
       (fun e ->
          Lwt.return (Logs.err (fun m -> m "exception %s, shutting down"
                                   (Printexc.to_string e)))));
  Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

let uname =
  let cmd = Bos.Cmd.(v "uname" % "-s") in
  lazy (match Bos.OS.Cmd.(run_out cmd |> out_string |> success) with
      | Ok s when s = "FreeBSD" -> `FreeBSD
      | Ok s when s = "Linux" -> `Linux
      | Ok s -> invalid_arg (Printf.sprintf "OS %s not supported" s)
      | Error (`Msg m) -> invalid_arg m)

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let port =
  let doc = "TCP listen port" in
  Arg.(value & opt int 1234 & info [ "port" ] ~doc)

let ip =
  let doc = "Listen IP" in
  Arg.(value & opt string "127.0.0.1" & info [ "ip" ] ~doc)

let default_dbdir =
  match Lazy.force uname with
  | `FreeBSD -> "/var/db/builder"
  | `Linux -> "/var/lib/builder"

let dbdir =
  let doc = "Directory for persistent data" in
  Arg.(value & opt dir default_dbdir & info [ "dir" ] ~doc)

let default_cfgdir =
  match Lazy.force uname with
  | `FreeBSD -> "/usr/local/etc/builder"
  | `Linux -> "/etc/builder"

let cfgdir =
  let doc = "Directory for configuration" in
  Arg.(value & opt dir default_cfgdir & info [ "config-dir" ] ~doc)

let upload =
  let doc = "Upload artifacts to URL (instead of local storage)" in
  Arg.(value & opt (some string) None & info [ "upload" ] ~doc)

let cmd =
  let term =
    Term.(term_result (const jump $ setup_log $ ip $ port $ dbdir $ cfgdir $ upload))
  and info = Cmd.info "builder-server" ~version:Builder.version
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
