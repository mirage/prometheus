open Prometheus

module Gauge : sig
  include module type of Gauge
  val track_inprogress : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [track_inprogress t f] increases the value of the gauge by one while [f ()] is running. *)

  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      increases the metric by the difference. *)
end

module Summary : sig
  include module type of Summary

  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      observes the difference. *)
end

module type HISTOGRAM = sig
  include HISTOGRAM

  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      observes the difference. *)
end

module Histogram (Buckets : BUCKETS) : HISTOGRAM

module DefaultHistogram : HISTOGRAM
(** A histogram configured with reasonable defaults for measuring network request times in seconds. *)
