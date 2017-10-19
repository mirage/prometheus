(** Collect metrics for Prometheus.
    See: https://prometheus.io/

    Notes:

    - The Prometheus docs require that client libraries are thread-safe. We interpret this to mean safe
      with Lwt threads, NOT with native threading.

    - This library is intended to be a dependency of any library that might need to report metrics,
      even though many applications will not enable it. Therefore it should have minimal dependencies.
*)

open Asetmap

type metric_type =
  | Counter
  | Gauge
  | Summary

module type NAME = sig
  type t = private string

  val v : string -> t
  (** Raises an exception if the name is not valid. *)

  val pp : t Fmt.t

  val compare : t -> t -> int
end
(** A string that meets some additional requirements. *)

module MetricName : NAME
(** A valid name for a metric. *)

module LabelName  : NAME
(** A valid name for a label. *)

module MetricInfo : sig
  type t = {
    name : MetricName.t;
    metric_type : metric_type;
    help : string;
    label_names : LabelName.t list;
  }
end
(** Metadata about a metric. *)

module LabelSetMap : Map.S with type key = string list
(** A map indexed by a set of labels. *)

module MetricFamilyMap : Map.S with type key = MetricInfo.t
(** A map indexed by metric families. *)

module CollectorRegistry : sig
  type t
  (** A collection of metrics to be monitored. *)

  type snapshot = (string * float) list LabelSetMap.t MetricFamilyMap.t
  (** The result of reading a set of metrics. *)

  val create : unit -> t
  (** [create ()] is a fresh registry. This is mostly useful for testing. *)

  val default : t
  (** The default registry. *)

  val collect : t -> snapshot
  (** Read the current value of each metric. *)

  val register : t -> MetricInfo.t -> (unit -> (string * float) list LabelSetMap.t) -> unit
  (** [register t metric collector] adds [metric] to the set of metrics being collected.
      It will call [collector ()] to collect the values each time [collect] is called. *)

  val register_pre_collect : t -> (unit -> unit) -> unit
  (** [register_pre_collect t fn] arranges for [fn ()] to be called at the start
      of each collection. This is useful if one expensive call provides
      information about multiple metrics. *)
end
(** A collection of metric reporters. Usually, only {!CollectorRegistry.default} is used. *)

module type METRIC = sig
  type family
  (** A collection of metrics that are the same except for their labels.
      e.g. "Number of HTTP responses" *)

  type t
  (** A particular metric.
      e.g. "Number of HTTP responses with code=404" *)

  val v_labels : label_names:string list -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> family
  (** [v_labels ~label_names ~help ~namespace ~subsystem name] is a family of metrics with full name
      [namespace_subsystem_name] and documentation string [help]. Each metric in the family will provide
      a value for each of the labels.
      The new family is registered with [registry] (default: {!CollectorRegistry.default}). *)

  val labels : family -> string list -> t
  (** [labels family label_values] is the metric in [family] with these values for the labels.
      The order of the values must be the same as the order of the [label_names] passed to [v_labels];
      you may wish to write a wrapper function with labelled arguments to avoid mistakes.
      If this is called multiple times with the same set of values, the existing metric will be returned. *)

  val v_label : label_name:string -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> (string -> t)
  (** [v_label] is a convenience wrapper around [v_labels] for the case where there is a single label.
      The result is a function from the single label's value to the metric. *)

  val v : ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> t
  (** [v] is a convenience wrapper around [v_labels] for the case where there are no labels. *)
end
(** Operations common to all types of metric. *)

module Counter : sig
  include METRIC
  val inc_one : t -> unit
  val inc : t -> float -> unit
  (** [inc t v] increases [t] by [v], which must be non-negative. *)

  val get : t -> float
  (** [get t] returns the current value of the counter. *)
end
(** A counter is a cumulative metric that represents a single numerical value that only ever goes up. *)

module Gauge : sig
  include METRIC

  val inc_one : t -> unit
  val inc : t -> float -> unit
  (** [inc t v] increases the current value of the guage by [v]. *)

  val dec_one : t -> unit
  val dec : t -> float -> unit
  (** [dec t v] decreases the current value of the guage by [v]. *)

  val set : t -> float -> unit
  (** [set t v] sets the current value of the guage to [v]. *)

  val track_inprogress : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [track_inprogress t f] increases the value of the gauge by one while [f ()] is running. *)

  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      increases the metric by the difference.
  *)

  val get : t -> float
  (** [get t] returns the current value of the gauge. *)
end
(** A gauge is a metric that represents a single numerical value that can arbitrarily go up and down. *)

module Summary : sig
  include METRIC

  val observe : t -> float -> unit
  (** [observe t v] increases the total by [v] and the count by one. *)

  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      observes the difference. *)

  val get : t -> float * float
  (** [get t] returns the sum and count of the summary. *)

  val get_average : t -> float
  (** [get_average t] returns the average of the counter. *)
end
(** A summary is a metric that records both the number of readings and their total.
    This allows calculating the average. *)
