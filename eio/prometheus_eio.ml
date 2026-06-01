(* Eio support for Prometheus. *)

let collect = Prometheus.CollectorRegistry.collect

module C = Prometheus_cohttp.Make (Cohttp_eio.Server)

let callback conn req body =
  C.callback ~collect:(fun () -> Prometheus.CollectorRegistry.(collect default))
    conn req body
