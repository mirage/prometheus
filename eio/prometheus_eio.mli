(** Eio support for Prometheus. *)

val callback :
  ?registry:Prometheus.CollectorRegistry.t ->
  unit ->
  Cohttp_eio.Server.conn ->
  Http.Request.t ->
  Cohttp_eio.Server.body ->
  Cohttp_eio.Server.response
(** [callback ()] is a cohttp-eio request handler that serves
    {!Prometheus.CollectorRegistry.default} at [/metrics] in the text format
    and answers anything else with [400 Bad Request]. Build and run a server
    with
    {[
    let server = Cohttp_eio.Server.make ~callback:Prometheus_eio.callback () in
    Cohttp_eio.Server.run socket server ~on_error:log_error
    ]} *)
