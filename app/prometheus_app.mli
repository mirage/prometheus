(** Report metrics for Prometheus.

    See: {{:https://prometheus.io/}https://prometheus.io/}

    Notes:

    - This module is intended to be used by applications that export Prometheus metrics.
      Libraries should only link against the `Prometheus` module.

    - This module automatically initialises itself and registers some standard collectors relating to
      GC statistics, as recommended by Prometheus.

    - This module does not depend on [Unix], and so can be used in unikernels.

    - This module calls {!Prometheus.init} automatically when linked.

    - The renderer and the standard collectors live in [Prometheus_reporter],
      which this module re-exports. Applications that serve metrics with a web
      server other than cohttp can use that package directly.
 *)

module TextFormat_0_0_4 = Prometheus_reporter.TextFormat_0_0_4
(** Format a snapshot in Prometheus's text format, version 0.0.4. *)

module Cohttp (S : Cohttp_lwt.S.Server) : sig
  val callback :
    ?registry:Prometheus_lwt.CollectorRegistry.t ->
    S.conn ->
    Cohttp.Request.t ->
    Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
end
(** A Cohttp callback for a web-server that exposes the Prometheus metrics. *)
