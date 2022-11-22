open Prometheus

module Metrics = struct
  let namespace = "prometheus"

  let subsystem = "logs"

  let inc_messages =
    let help = "Total number of messages logged" in
    let c =
      Counter.v_labels ~label_names:[ "level"; "src" ] ~help ~namespace
        ~subsystem "messages_total"
    in
    fun lvl src ->
      let lvl = Logs.level_to_string (Some lvl) in
      Counter.inc_one @@ Counter.labels c [ lvl; src ]
end

module Unix_runtime = struct
  let start_time = Unix.gettimeofday ()

  let simple_metric ~metric_type ~help name fn =
    let info = {
      MetricInfo.
      name = MetricName.v name;
      help;
      metric_type;
      label_names = [];
    }
    in
    let collect () =
      LabelSetMap.singleton [] [Sample_set.sample (fn ())]
    in
    info, collect

  let process_start_time_seconds =
    simple_metric ~metric_type:Counter "process_start_time_seconds" (fun () -> start_time)
      ~help:"Start time of the process since unix epoch in seconds."

  let metrics = [
    process_start_time_seconds;
  ]
end

type config = string option

module Server = Prometheus_app.Cohttp(Cohttp_lwt_unix.Server)

let bind addr port =
    let open! Unix in
    let [@ocaml.warning "-partial-match"] addrinfo :: _ =
      getaddrinfo addr port [AI_SOCKTYPE SOCK_STREAM] in
    let socket = socket ~cloexec:true addrinfo.ai_family addrinfo.ai_socktype addrinfo.ai_protocol in
    let () = setsockopt socket SO_REUSEADDR true in
    let callback = Server.callback in
    let () = bind socket addrinfo.ai_addr in
    let () = listen socket 20 in
    let mode = `TCP (`Socket (Lwt_unix.of_unix_file_descr socket)) in
    let thread = Cohttp_lwt_unix.Server.create ~mode (Cohttp_lwt_unix.Server.make ~callback ()) in
    [thread]

let serve config =
  let addr = "0.0.0.0" in
  let port = "9090" in
  match config with
  | None -> []
  | Some config_s ->
    try
      match (String.split_on_char ':' config_s) with
      | [] -> bind addr port
      | port :: [] -> bind addr port
      | addr :: port :: [] -> bind addr port
    with
      | Match_failure _ -> Printf.printf "ERROR: Incorrect addr:port pair specified, prometheus listener not starting.\n"; flush_all (); []
      [@@ocaml.warning "-partial-match"]

let listen_prometheus =
  let open! Cmdliner in
  let doc =
    Arg.info ~docs:"MONITORING OPTIONS" ~docv:"ADDR_PORT" ~doc:
      "Address and port on which to provide Prometheus metrics over HTTP."
      ["listen-prometheus"]
  in
  Arg.(value @@ opt (some string) None doc)

let opts = listen_prometheus

let () =
  let add (info, collector) =
    CollectorRegistry.(register default) info collector in
  List.iter add Unix_runtime.metrics

module Logging = struct
  let inc_counter = Metrics.inc_messages

  let pp_timestamp f x =
    let open Unix in
    let tm = localtime x in
    Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
      tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

  let reporter formatter =
    let report src level ~over k msgf =
      let k _ = over (); k () in
      let src = Logs.Src.name src in
      Metrics.inc_messages level src;
      msgf @@ fun ?header ?tags:_ fmt ->
      Fmt.kpf k formatter ("%a %a %a @[" ^^ fmt ^^ "@]@.")
        pp_timestamp (Unix.gettimeofday ())
        Fmt.(styled `Magenta string) (Printf.sprintf "%14s" src)
        Logs_fmt.pp_header (level, header)
    in
    { Logs.report = report }

  let set_level (src, level) =
    let rec aux = function
      | [] -> Logs.warn (fun f -> f "set_level: logger %S not registered; ignoring" src)
      | x :: _ when Logs.Src.name x = src -> Logs.Src.set_level x (Some level)
      | _ :: xs -> aux xs
    in
    aux (Logs.Src.list ())

  let init ?(default_level=Logs.Info) ?(levels=[]) ?(formatter=Fmt.stderr) () =
    Fmt_tty.setup_std_outputs ();
    Logs.set_reporter (reporter formatter);
    Logs.set_level (Some default_level);
    List.iter set_level levels
end
