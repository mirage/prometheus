(** Eio support for Prometheus: a cohttp-eio [/metrics] handler.

    Recording (Counter.inc, Gauge.set, ...) and {!Prometheus.CollectorRegistry}
    are in the backend-free [prometheus] core. A collector that performs Eio I/O
    needs no special registration: it just does so, and {!collect}, called from
    a fiber, suspends and resumes around it.

    The core's timing helpers ({!Prometheus.Gauge.time},
    {!Prometheus.Gauge.track_inprogress}, {!Prometheus.Summary.time}) are already
    direct-style, so use them as-is under Eio — passing [(fun () -> Eio.Time.now clock)]
    where a time source is required. *)

val collect : Prometheus.CollectorRegistry.t -> Prometheus.CollectorRegistry.snapshot
(** [collect t] reads every registered metric. Call it from a fiber: a collector
    may perform Eio I/O while it runs. This is {!Prometheus.CollectorRegistry.collect}. *)

val callback :
  Cohttp_eio.Server.conn ->
  Http.Request.t ->
  Cohttp_eio.Server.body ->
  Cohttp_eio.Server.response
(** A cohttp-eio request handler that serves {!Prometheus.CollectorRegistry.default}
    at [/metrics]. Build and run a server with:
    {[
    let server = Cohttp_eio.Server.make ~callback:Prometheus_eio.callback () in
    Cohttp_eio.Server.run socket server ~on_error:log_error
    ]} *)
