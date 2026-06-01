(** Report metrics for Prometheus.

    See: {{:https://prometheus.io/}https://prometheus.io/}

    Notes:

    - This module is intended to be used by applications that export Prometheus metrics.
      Libraries should only link against the `Prometheus` module.

    - This module automatically initialises itself and registers some standard collectors relating to
      GC statistics, as recommended by Prometheus.

    - This module does not depend on [Unix], and so can be used in unikernels.
 *)

module TextFormat_0_0_4 : sig
  val output : Prometheus.CollectorRegistry.snapshot Fmt.t
end
(** Format a snapshot in Prometheus's text format, version 0.0.4. *)

(** This package is backend-agnostic: it renders snapshots and registers the
    standard GC collectors, but does not serve them over HTTP. To expose metrics,
    use [prometheus-lwt] (Lwt + cohttp-lwt) or [prometheus-eio] (Eio + cohttp-eio). *)
