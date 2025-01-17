let src = Logs.Src.create "builder" ~doc:"Builder"
module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Result.bind

type data = (Fpath.t * string) list

let pp_data ppf xs =
  Fmt.(list ~sep:(any "@.")
         (pair ~sep:(any ": ") Fpath.pp int))
    ppf
    (List.map (fun (f, s) -> f, String.length s) xs)

type script_job = {
  name : string ;
  platform : string ;
  script : string ;
}

let pp_script_job ppf { name ; platform ; _ } =
  Fmt.pf ppf "name %s on %s" name platform

type orb_build_job = {
  name : string ;
  opam_package : string ;
}

let pp_orb_build_job ppf { name ; opam_package } =
  Fmt.pf ppf "name %s, opam %s" name opam_package

type execution_result =
  | Exited of int
  | Signalled of int
  | Stopped of int
  | Msg of string

let pp_execution_result ppf = function
  | Exited i -> Fmt.pf ppf "exited %d" i
  | Signalled i -> Fmt.pf ppf "signalled %d" i
  | Stopped i -> Fmt.pf ppf "stopped %d" i
  | Msg m -> Fmt.pf ppf "execution aborted: %s" m

type period = Hourly | Daily | Weekly | Never

let pp_period ppf = function
  | Hourly -> Fmt.string ppf "hourly"
  | Daily -> Fmt.string ppf "daily"
  | Weekly -> Fmt.string ppf "weekly"
  | Never -> Fmt.string ppf "never"

type job =
  | Script_job of script_job
  | Orb_build_job of orb_build_job

let pp_job ppf = function
  | Script_job j -> pp_script_job ppf j
  | Orb_build_job j -> pp_orb_build_job ppf j

 let job_name = function
   | Script_job { name ; _ } -> name
   | Orb_build_job { name ; _ } -> name

let job_equal a b =
   String.equal (job_name a) (job_name b)

let job_platform = function
  | Script_job { platform; _ } -> Some platform
  | Orb_build_job _ -> None

type schedule_item = {
  next : Ptime.t ;
  period : period ;
  job : job ;
}

let pp_schedule_item ppf { next ; period ; job } =
  Fmt.pf ppf "%a next %a, scheduled %a" pp_job job
    (Ptime.pp_rfc3339 ()) next
    pp_period period

type info = {
  schedule : schedule_item list ;
  queues : (string * job list) list ;
  running : (Ptime.t * Uuidm.t * script_job) list ;
  dropped_platforms : string list ;
}

let triple ~sep pc pb pa ppf (va, vb, vc)=
  Fmt.pair ~sep pc (Fmt.pair ~sep pb pa) ppf
    (vc, (vb, va))

let pp_info ppf { schedule ; queues ; running ; dropped_platforms } =
  let pp_time = Ptime.pp_rfc3339 () in
  Fmt.pf ppf "dropped platforms: %a@.schedule:@.%a@.queues:@.%a@.running:@.%a@."
    Fmt.(list ~sep:(any "@ ") string) dropped_platforms
    Fmt.(list ~sep:(any ";@.") pp_schedule_item) schedule
    Fmt.(list ~sep:(any "@.")
      (pair ~sep:(any ":@ ") string (list ~sep:(any ";@ ") pp_job))) queues
    Fmt.(list ~sep:(any ";@.")
           (triple ~sep:(any ",@,") pp_script_job Uuidm.pp pp_time)) running

type cmd =
  | Job_requested of string (* worker *)
  | Job_schedule of Uuidm.t * script_job (* worker *)
  | Job_finished of Uuidm.t * execution_result * data (* worker *)
  | Output of Uuidm.t * string (* worker *)
  | Output_timestamped of Uuidm.t * int64 * string (* client *)
  | Schedule of period * script_job (* client *)
  | Unschedule of string (* client *)
  | Info (* client *)
  | Info_reply of info (* client *)
  | Observe of Uuidm.t (* client *)
  | Execute of string * string option (* client *)
  | Schedule_orb_build of period * orb_build_job (* client *)
  | Reschedule of string * Ptime.t * period option (* client *)
  | Drop_platform of string (* client *)
  | Undrop_platform of string (* client *)
  | Client_hello of [ `Client | `Worker ] * int
  | Server_hello
  | Success (* client *)
  | Failure of string (* client *)

let client_version = 13
let worker_version = 5

let version =
  Fmt.str "version %%VERSION%% protocol: client %d worker %d"
    client_version worker_version

let pp_cmd ppf = function
  | Job_requested platform -> Fmt.pf ppf "job request on %s" platform
  | Job_schedule (uuid, job) ->
    Fmt.pf ppf "[%a] job schedule %a" Uuidm.pp uuid pp_script_job job
  | Job_finished (uuid, result, data) ->
    Fmt.pf ppf "[%a] job finished with %a: %a" Uuidm.pp uuid
      pp_execution_result result pp_data data
  | Output (uuid, data) -> Fmt.pf ppf "[%a] %S" Uuidm.pp uuid data
  | Output_timestamped (uuid, ts, data) -> Fmt.pf ppf "[%a] %a: %S" Uuidm.pp uuid Duration.pp ts data
  | Schedule (period, job) ->
    Fmt.pf ppf "schedule at %a: %a" pp_period period pp_script_job job
  | Unschedule job_name -> Fmt.pf ppf "unschedule %s" job_name
  | Info -> Fmt.string ppf "info"
  | Info_reply info -> Fmt.pf ppf "info:@.%a" pp_info info
  | Observe id -> Fmt.pf ppf "observe %a" Uuidm.pp id
  | Execute (name, platform) ->
    Fmt.pf ppf "execute %s on %a" name Fmt.(option ~none:(any "any platform") string) platform
  | Schedule_orb_build (period, orb_job) ->
    Fmt.pf ppf "schedule orb build at %a: %a" pp_period period pp_orb_build_job orb_job
  | Reschedule (name, next, None) ->
    Fmt.pf ppf "reschedule %s: %a" name (Ptime.pp_rfc3339 ()) next
  | Reschedule (name, next, Some period) ->
    Fmt.pf ppf "reschedule %s at %a: %a" name pp_period period (Ptime.pp_rfc3339 ()) next
  | Drop_platform platform ->
    Fmt.pf ppf "drop platform %s" platform
  | Undrop_platform platform ->
    Fmt.pf ppf "undrop platform %s" platform
  | Client_hello (t, num) ->
    Fmt.pf ppf "client hello %s %d"
      (match t with `Client -> "client" | `Worker -> "worker") num
  | Server_hello -> Fmt.string ppf "server hello"
  | Success -> Fmt.string ppf "success"
  | Failure msg -> Fmt.pf ppf "failure %s" msg

type state_item =
  | Queue of string * job list
  | Schedule of schedule_item
  | Dropped_platform of string

let pp_state_item ppf = function
  | Queue (platform, j) ->
    Fmt.pf ppf "queue platform %s jobs %a"
      platform Fmt.(list ~sep:(any ", ") pp_job) j
  | Schedule s -> Fmt.pf ppf "schedule %a" pp_schedule_item s
  | Dropped_platform platform ->
    Fmt.pf ppf "dropped platform %s" platform

type state = state_item list

let pp_state = Fmt.(list ~sep:(any ";@ ") pp_state_item)

module Asn = struct
  let decode_strict codec buf =
    match Asn.decode codec buf with
    | Ok (a, rest) ->
      if String.length rest = 0 then
        Ok a
      else
        Error (`Msg "trailing bytes")
    | Error (`Parse msg) -> Error (`Msg msg)

  let projections_of asn =
    let c = Asn.codec Asn.der asn in
    (decode_strict c, Asn.encode c)

  let data =
    let f (path, value) =
      match Fpath.of_string path with
      | Ok p -> p, value
      | Error `Msg msg -> Asn.S.error (`Parse msg)
    and g (path, value) =
      Fpath.to_string path, value
    in
    Asn.S.(sequence_of
             (map f g
                (sequence2
                   (required ~label:"path" utf8_string)
                   (required ~label:"data" utf8_string))))

  let script_job =
    let f (name, script, _files, platform) =
      let platform = Option.value ~default:"no-platform" platform in
      { name ; platform ; script }
    and g { name ; platform ; script } =
      name, script, [], Some platform
    in
    Asn.S.(map f g (sequence4
                      (required ~label:"name" utf8_string)
                      (required ~label:"script" utf8_string)
                      (required ~label:"files" data)
                      (optional ~label:"platform" utf8_string)))

  let orb_build_job =
    let f (name, opam_package) =
      { name ; opam_package }
    and g { name ; opam_package } =
      name, opam_package
    in
    Asn.S.(map f g (sequence2
                      (required ~label:"name" utf8_string)
                      (required ~label:"opam_package" utf8_string)))

  let job =
    let f = function
     | `C1 j -> Script_job j
     | `C2 j -> Orb_build_job j
   and g = function
     | Script_job j -> `C1 j
     | Orb_build_job j -> `C2 j
   in
   Asn.S.(map f g
     (choice2
       (explicit 0 script_job)
       (explicit 1 orb_build_job)))

  let period =
    let f = function
      | `C1 () -> Hourly
      | `C2 () -> Daily
      | `C3 () -> Weekly
      | `C4 () -> Never
    and g = function
      | Hourly -> `C1 ()
      | Daily -> `C2 ()
      | Weekly -> `C3 ()
      | Never -> `C4 ()
    in
    Asn.S.(map f g
             (choice4 (explicit 0 null) (explicit 1 null) (explicit 2 null) (explicit 3 null)))

  let old_schedule =
    let f (next, period, job) = {next; period; job = Script_job job}
    and g _ = assert false
    in
    Asn.S.(map f g (sequence3
                      (required ~label:"next" utc_time)
                      (required ~label:"period" period)
                      (required ~label:"job" script_job)))

  let schedule =
    let f (next, period, job) =
      let next = match next with `C1 n | `C2 n -> n in
      {next; period; job}
    and g {next; period; job} = (`C2 next, period, job)
    in
    Asn.S.(map f g (sequence3
                      (required ~label:"next"
                         (choice2 utc_time generalized_time))
                      (required ~label:"period" period)
                      (required ~label:"job" job)))

  let state_item =
    let f = function
      | `C1 _ -> Log.warn (fun m -> m "script job for queue no longer supported, ignoring"); None
      | `C2 s -> Some (Schedule s)
      | `C3 _ -> Log.warn (fun m -> m "job for queue no longer supported, ignoring"); None
      | `C4 e -> Some (Schedule e)
      | `C5 (platform, jobs) -> Some (Queue (platform, jobs))
      | `C6 platform -> Some (Dropped_platform platform)
    and g = function
      | Some (Schedule s) -> `C4 s
      | Some (Queue (platform, jobs)) -> `C5 (platform, jobs)
      | Some Dropped_platform platform -> `C6 platform
      | None -> assert false
    in
    Asn.S.(map f g (choice6
                      (explicit 0 script_job)
                      (explicit 1 old_schedule)
                      (explicit 2 job)
                      (explicit 3 schedule)
                      (explicit 4
                        (sequence2
                          (required ~label:"platform" utf8_string)
                          (required ~label:"jobs" (sequence_of job))))
                      (explicit 5 utf8_string)))

  let state_of_str, state_to_str =
    let of_str, to_str = projections_of (Asn.S.sequence_of state_item) in
    (fun buf ->
       let* items = of_str buf in
       Ok (List.filter_map Fun.id items)),
    (fun s -> List.map (fun x -> Some x) s |> to_str)

  let uuid =
    let f s =
      match Uuidm.of_binary_string s with
      | None -> Asn.S.error (`Parse "couldn't decode UUID")
      | Some s -> s
    and g uuid = Uuidm.to_binary_string uuid
    in
    Asn.S.(map f g utf8_string)

  let res =
    let f = function
      | `C1 i -> Exited i
      | `C2 i -> Signalled i
      | `C3 i -> Stopped i
      | `C4 s -> Msg s
    and g = function
      | Exited i -> `C1 i
      | Signalled i -> `C2 i
      | Stopped i -> `C3 i
      | Msg s -> `C4 s
    in
    Asn.S.(map f g
             (choice4
                (explicit 0 int)
                (explicit 1 int)
                (explicit 2 int)
                (explicit 3 utf8_string)))

  let exec =
    (* please note that this is used by builder-web as well, so a change needs
       careful thought in respect to potentially an old builder-web trying to
       decode the binary *)
    let f = function
      | `C1 (job, uuid, out, (created, finished), res, data) ->
        let created = match created with `C1 n | `C2 n -> n
        and finished = match finished with `C1 n | `C2 n -> n
        in
        job, uuid, out, created, finished, res, data
      | `C2 () -> assert false
    and g (job, uuid, out, created, finished, res, data) =
      `C1 (job, uuid, out, (`C2 created, `C2 finished), res, data)
    in
    Asn.S.(map f g
             (choice2
                (explicit 0
                   (sequence6
                      (required ~label:"job" script_job)
                      (required ~label:"uuid" uuid)
                      (required ~label:"console"
                         (sequence_of (sequence2
                                         (required ~label:"delta" int)
                                         (required ~label:"data" utf8_string))))
                      (required ~label:"timestamps"
                         (sequence2
                            (required ~label:"started"
                               (choice2 utc_time generalized_time))
                            (required ~label:"finished"
                               (choice2 utc_time generalized_time))))
                      (required ~label:"result" res)
                      (required ~label:"output" data)))
                (explicit 1 null)))

  let exec_of_str, exec_to_str = projections_of exec

  let client_or_worker =
    let f = function
      | `C1 () -> `Client
      | `C2 () -> `Worker
    and g = function
      | `Client -> `C1 ()
      | `Worker -> `C2 ()
    in
    Asn.S.(map f g @@ choice2
             (explicit 0 null)
             (explicit 1 null))

  let cmd =
    let f = function
      | `C1 `C1 _max -> assert false
      | `C1 `C2 _max -> assert false
      | `C1 `C3 (uuid, job) -> Job_schedule (uuid, job)
      | `C1 `C4 (uuid, res, data) -> Job_finished (uuid, res, data)
      | `C1 `C5 (uuid, out, timestamp) ->
        begin match timestamp with
          | None -> Output (uuid, out)
          | Some ts -> Output_timestamped (uuid, Int64.of_int ts, out)
        end
      | `C1 `C6 (period, job) -> Schedule (period, job)
      | `C2 `C1 () -> Info
      | `C2 `C2 (schedule, queues, running) ->
        let running =
          List.map (fun (started, uuid, job) ->
              let started = match started with `C1 n | `C2 n -> n in
              started, uuid, job)
            running
        in
        Info_reply { schedule ; queues ; running ; dropped_platforms = [] }
      | `C2 `C3 () -> assert false
      | `C2 `C4 jn -> Unschedule jn
      | `C2 `C5 id -> Observe id
      | `C2 `C6 jn -> Execute (jn, None)
      | `C3 `C1 (period, orb_job) -> Schedule_orb_build (period, orb_job)
      | `C3 `C2 (name, next, period) ->
        let next = match next with `C1 n | `C2 n -> n in
        Reschedule (name, next, period)
      | `C3 `C3 (t, n) -> Client_hello (t, n)
      | `C3 `C4 () -> Server_hello
      | `C3 `C5 platform -> Job_requested platform
      | `C3 `C6 platform -> Drop_platform platform
      | `C4 `C1 () -> Success
      | `C4 `C2 msg -> Failure msg
      | `C4 `C3 (name, platform) -> Execute (name, platform)
      | `C4 `C4 platform -> Undrop_platform platform
      | `C4 `C5 (schedule, queues, running, dropped_platforms) ->
        let running =
          List.map (fun (started, uuid, job) ->
              let started = match started with `C1 n | `C2 n -> n in
              started, uuid, job)
            running
        in
        Info_reply { schedule ; queues ; running ; dropped_platforms }
    and g = function
      | Job_schedule (uuid, job) -> `C1 (`C3 (uuid, job))
      | Job_finished (uuid, res, data) -> `C1 (`C4 (uuid, res, data))
      | Output (uuid, out) -> `C1 (`C5 (uuid, out, None))
      | Output_timestamped (uuid, ts, out) -> `C1 (`C5 (uuid, out, Some (Int64.to_int ts)))
      | Schedule (period, job) -> `C1 (`C6 (period, job))
      | Info -> `C2 (`C1 ())
      | Info_reply { schedule ; queues ; running ; dropped_platforms } ->
        let running =
          List.map (fun (started, uuid, job) -> `C2 started, uuid, job) running
        in
        `C4 (`C5 (schedule, queues, running, dropped_platforms))
      | Unschedule jn -> `C2 (`C4 jn)
      | Observe id -> `C2 (`C5 id)
      | Schedule_orb_build (period, orb_job) -> `C3 (`C1 (period, orb_job))
      | Reschedule (name, next, period) -> `C3 (`C2 (name, `C2 next, period))
      | Client_hello (t, n) -> `C3 (`C3 (t, n))
      | Server_hello -> `C3 (`C4 ())
      | Job_requested platform -> `C3 (`C5 platform)
      | Drop_platform platform -> `C3 (`C6 platform)
      | Success -> `C4 (`C1 ())
      | Failure msg -> `C4 (`C2 msg)
      | Execute (job, platform) -> `C4 (`C3 (job, platform))
      | Undrop_platform platform -> `C4 (`C4 platform)
    in
    Asn.S.(map f g
             (choice4
                (choice6
                   (explicit 0 int)
                   (explicit 1 int)
                   (explicit 2 (sequence2
                                  (required ~label:"uuid" uuid)
                                  (required ~label:"job" script_job)))
                   (explicit 3 (sequence3
                                  (required ~label:"uuid" uuid)
                                  (required ~label:"result" res)
                                  (required ~label:"data" data)))
                   (explicit 4 (sequence3
                                  (required ~label:"uuid" uuid)
                                  (required ~label:"output" utf8_string)
                                  (optional ~label:"timestamp" int)))
                   (explicit 5 (sequence2
                                  (required ~label:"period" period)
                                  (required ~label:"job" script_job))))
                (choice6
                   (explicit 6 null)
                   (explicit 7 (sequence3
                                  (required ~label:"schedule" (sequence_of schedule))
                                  (required ~label:"queues"
                                    (sequence_of
                                      (sequence2
                                        (required ~label:"platform" utf8_string)
                                        (required ~label:"jobs" (sequence_of job)))))
                                  (required ~label:"running"
                                     (sequence_of
                                        (sequence3
                                           (required ~label:"started"
                                              (choice2 utc_time generalized_time))
                                           (required ~label:"uuid" uuid)
                                           (required ~label:"job" script_job))))))
                   (explicit 8 null)
                   (explicit 9 utf8_string)
                   (explicit 10 uuid)
                   (explicit 11 utf8_string))
                (choice6
                   (explicit 12 (sequence2
                     (required ~label:"period" period)
                     (required ~label:"orb_build_job" orb_build_job)))
                   (explicit 13 (sequence3
                     (required ~label:"name" utf8_string)
                     (required ~label:"next" (choice2 utc_time generalized_time))
                     (optional ~label:"period" period)))
                   (explicit 14 (sequence2
                     (required ~label:"typ" client_or_worker)
                     (required ~label:"version" int)))
                   (explicit 15 null)
                   (explicit 16 utf8_string)
                   (explicit 17 utf8_string))
                (choice5
                   (explicit 18 null)
                   (explicit 19 utf8_string)
                   (explicit 20 (sequence2
                     (required ~label:"job_name" utf8_string)
                     (optional ~label:"platform" utf8_string)))
                   (explicit 21 utf8_string)
                   (explicit 22 (sequence4
                                  (required ~label:"schedule" (sequence_of schedule))
                                  (required ~label:"queues"
                                    (sequence_of
                                      (sequence2
                                        (required ~label:"platform" utf8_string)
                                        (required ~label:"jobs" (sequence_of job)))))
                                  (required ~label:"running"
                                     (sequence_of
                                        (sequence3
                                           (required ~label:"started"
                                              (choice2 utc_time generalized_time))
                                           (required ~label:"uuid" uuid)
                                           (required ~label:"job" script_job))))
                                  (required ~label:"dropped-platforms" (sequence_of utf8_string))))
                )))

  let cmd_of_str, cmd_to_str = projections_of cmd
end

let rec ign_intr f v =
  try f v with Unix.Unix_error (Unix.EINTR, _, _) -> ign_intr f v

let read fd =
  try
    let rec r b ?(off = 0) l =
      if l = 0 then
        Ok ()
      else
        let read = ign_intr (Unix.read fd b off) l in
        if read = 0 then
          Error (`Msg "end of file")
        else
          r b ~off:(read + off) (l - read)
    in
    let bl = Bytes.create 8 in
    let* () = r bl 8 in
    let l = Bytes.get_int64_be bl 0 in
    let l_int = Int64.to_int l in (* TODO *)
    let b = Bytes.create l_int in
    let* () = r b l_int in
    Ok (Bytes.unsafe_to_string b)
  with
    Unix.Unix_error (err, f, _) ->
    Log.err (fun m -> m "Unix error in %s: %s" f (Unix.error_message err));
    Error (`Msg "unix error in read")

let read_cmd fd =
  let* data = read fd in
  Asn.cmd_of_str data

let write fd data =
  try
    let rec w b ?(off = 0) l =
      if l = 0 then
        ()
      else
        let written = ign_intr (Unix.write fd b off) l in
        w b ~off:(written + off) (l - written)
    in
    let csl = Bytes.create 8 in
    Bytes.set_int64_be csl 0 (Int64.of_int (String.length data));
    w csl 8;
    w (Bytes.unsafe_of_string data) (String.length data);
    Ok ()
  with
    Unix.Unix_error (err, f, _) ->
    Log.err (fun m -> m "Unix error in %s: %s" f (Unix.error_message err));
    Error (`Msg "unix error in write")

let write_cmd fd cmd =
  let data = Asn.cmd_to_str cmd in
  write fd data
