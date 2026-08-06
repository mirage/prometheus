(** Lwt support for Prometheus.

    Provide collectors that may suspend before producing their metrics. *)

module CollectorRegistry : sig
  type t

  val of_registry : Prometheus.CollectorRegistry.t -> t
  (** [of_registry core] is the Lwt view over the existing registry [core]. *)

  val default : t
  (** [default] is the view over {!Prometheus.CollectorRegistry.default}. *)

  val core : t -> Prometheus.CollectorRegistry.t
  (** [core t] is the underlying core registry. Use it to register
      synchronous metrics. *)

  val collect : t -> Prometheus.CollectorRegistry.snapshot Lwt.t
  (** [collect t] reads the current value of every metric. *)

  val register :
    t -> Prometheus.MetricInfo.t ->
    (unit -> Prometheus.Sample_set.t Prometheus.LabelSetMap.t Lwt.t) -> unit
  (** [register t info collector] registers a collector that may suspend
      before producing its samples. *)

  val register_pre_collect : t -> (unit -> unit Lwt.t) -> unit
  (** [register_pre_collect t f] arranges for [f ()] to run at the start
      of each collection. *)
end

module Gauge : sig
  val track_in_progress : Prometheus.Gauge.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [track_in_progress t f] increases [t] by one while [f ()] is running. *)

  val set_time : Prometheus.Gauge.t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [set_time t gettime f] calls [gettime ()] before and after executing [f ()] and
      sets [t] to the difference. *)
end

module Summary : sig
  val observe_time : Prometheus.Summary.t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [observe_time t gettime f] observes the duration of [f ()]. *)
end

module Histogram (H : Prometheus.HISTOGRAM) : sig
  val observe_time : H.t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [observe_time t gettime f] observes the duration of [f ()]. *)
end
