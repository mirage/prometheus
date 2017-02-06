open Prometheus

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
      LabelSetMap.singleton [] ["", fn ()]
    in
    info, collect

  let process_start_time_seconds =
    simple_metric ~metric_type:Counter "process_start_time_seconds" (fun () -> start_time)
      ~help:"Start time of the process since unix epoch in seconds."

  let metrics = [
    process_start_time_seconds;
  ]
end

type config = int option

module Server = Prometheus_app.Cohttp(Cohttp_lwt_unix.Server)

let serve = function
  | None -> []
  | Some port ->
    let mode = `TCP (`Port port) in
    let callback = Server.callback in
    let thread = Cohttp_lwt_unix.Server.create ~mode (Cohttp_lwt_unix.Server.make ~callback ()) in
    [thread]

let listen_prometheus =
  let open Cmdliner in
  let doc =
    Arg.info ~doc:
      "Port on which to provide Prometheus metrics over HTTP, \
       of the form port or host:port"
      ["listen-prometheus"]
  in
  Arg.(value @@ opt (some int) None doc)

let opts = listen_prometheus

let () =
  let add (info, collector) =
    CollectorRegistry.(register default) info collector in
  List.iter add Unix_runtime.metrics
