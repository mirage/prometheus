(** Lwt support for Prometheus.

    For Lwt applications this provides collectors that may suspend
    ([register_lwt]), an Lwt-returning {!CollectorRegistry.collect}, the Lwt
    timing helpers, and a cohttp callback to serve metrics.

    Metric {e recording} (Counter.inc, Gauge.set, ...) lives in the backend-free
    [prometheus] core; only collection and timing are Lwt-flavoured here. *)

module CollectorRegistry : sig
  type t
  (** An Lwt view over a core {!Prometheus.CollectorRegistry.t}: it adds
      collectors that may return an [Lwt.t]. Synchronous metrics registered on
      the underlying core registry (counters, gauges, the GC collectors, ...)
      are still collected. *)

  val of_registry : Prometheus.CollectorRegistry.t -> t
  (** [of_registry core] wraps an existing core registry. *)

  val default : t
  (** Wraps {!Prometheus.CollectorRegistry.default}. *)

  val core : t -> Prometheus.CollectorRegistry.t
  (** The underlying core registry (register synchronous metrics here). *)

  val collect : t -> Prometheus.CollectorRegistry.snapshot Lwt.t
  (** Read every metric: the core's synchronous collectors plus this view's
      Lwt collectors, with the Lwt collectors run concurrently. *)

  val register_lwt :
    t -> Prometheus.MetricInfo.t ->
    (unit -> Prometheus.Sample_set.t Prometheus.LabelSetMap.t Lwt.t) -> unit
  (** [register_lwt t info collector] registers a collector that may suspend
      (perform I/O) before producing its samples. *)

  val register_pre_collect_lwt : t -> (unit -> unit Lwt.t) -> unit
  (** Like {!Prometheus.CollectorRegistry.register_pre_collect} but the hook
      returns an [Lwt.t]. Run at the start of each collection. *)
end

module Gauge : sig
  val track_inprogress : Prometheus.Gauge.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [track_inprogress t f] increases [t] by one while [f ()] is running. *)

  val time : Prometheus.Gauge.t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [time t gettime f] adds the duration of [f ()] to [t]. *)
end

module Summary : sig
  val time : Prometheus.Summary.t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [time t gettime f] observes the duration of [f ()]. *)
end

module Cohttp (S : Cohttp_lwt.S.Server) : sig
  val callback :
    S.conn ->
    Cohttp.Request.t ->
    Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
end
(** A Cohttp callback that serves {!CollectorRegistry.default} at [/metrics]. *)
