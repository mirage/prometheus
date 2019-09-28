open Prometheus

module Runtime = struct
  let current = ref (Gc.stat ())
  let update () =
    current := Gc.stat ()

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

  let ocaml_gc_allocated_bytes =
    simple_metric ~metric_type:Counter "ocaml_gc_allocated_bytes" Gc.allocated_bytes
      ~help:"Total number of bytes allocated since the program was started."

  let ocaml_gc_major_words =
    simple_metric ~metric_type:Counter "ocaml_gc_major_words" (fun () -> (!current).Gc.major_words)
      ~help:"Number of words allocated in the major heap since the program was started."

  let ocaml_gc_minor_collections =
    simple_metric ~metric_type:Counter "ocaml_gc_minor_collections" (fun () -> float_of_int (!current).Gc.minor_collections)
      ~help:"Number of minor collection cycles completed since the program was started."

  let ocaml_gc_major_collections =
    simple_metric ~metric_type:Counter "ocaml_gc_major_collections" (fun () -> float_of_int (!current).Gc.major_collections)
      ~help:"Number of major collection cycles completed since the program was started."

  let ocaml_gc_heap_words =
    simple_metric ~metric_type:Gauge "ocaml_gc_heap_words" (fun () -> float_of_int (!current).Gc.heap_words)
      ~help:"Total size of the major heap, in words."

  let ocaml_gc_compactions =
    simple_metric ~metric_type:Counter "ocaml_gc_compactions" (fun () -> float_of_int (!current).Gc.compactions)
      ~help:"Number of heap compactions since the program was started."

  let ocaml_gc_top_heap_words =
    simple_metric ~metric_type:Counter "ocaml_gc_top_heap_words" (fun () -> float_of_int (!current).Gc.top_heap_words)
      ~help:"Maximum size reached by the major heap, in words."

  let process_cpu_seconds_total =
    simple_metric ~metric_type:Counter "process_cpu_seconds_total" Sys.time
      ~help:"Total user and system CPU time spent in seconds."

  let metrics = [
    ocaml_gc_allocated_bytes;
    ocaml_gc_major_words;
    ocaml_gc_minor_collections;
    ocaml_gc_major_collections;
    ocaml_gc_heap_words;
    ocaml_gc_compactions;
    ocaml_gc_top_heap_words;
    process_cpu_seconds_total;
  ]
end


module Cohttp(Server : Cohttp_lwt.S.Server) = struct
  let callback _conn req _body =
    let open Cohttp in
    let uri = Request.uri req in
    match Request.meth req, Uri.path uri with
    | `GET, "/metrics" ->
      let data = Prometheus.CollectorRegistry.(collect default) in
      let body = Fmt.to_to_string CollectorRegistry.pp_snapshot data in
      let headers = Header.init_with "Content-Type" "text/plain; version=0.0.4" in
      Server.respond_string ~status:`OK ~headers ~body ()
    | _ -> Server.respond_error ~status:`Bad_request ~body:"Bad request" ()
end

let () =
  CollectorRegistry.(register_pre_collect default) Runtime.update;
  let add (info, collector) =
    CollectorRegistry.(register default) info collector in
  List.iter add Runtime.metrics
