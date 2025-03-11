(** Collect metrics for Prometheus.

    See: {{:https://prometheus.io/}https://prometheus.io/}

    Notes:

    - The Prometheus docs require that client libraries are thread-safe. We interpret this to mean safe
      with cooperatively-scheduled threads (e.g. Lwt), NOT with native threading.

    - This library is intended to be a dependency of any library that might need to report metrics,
      even though many applications will not enable it. Therefore it should have minimal dependencies.

    - Lwt applications should use the [prometheus-lwt] package to collect
      metrics and to register collectors that suspend.
*)

val init : gettime:(unit -> int64) -> unit -> unit
(** [init ~gettime ()] sets the global function for recording the current time
    (in nanosecods from an arbitrary base).
    Typically this will be [Mirage_mtime.elapsed_ns].

    {!Prometheus_app} calls this at start-up, but if you're collecting metrics
    manually then you MUST call [init] yourself before using any time-based
    functions.

    Only the main application code that collects metrics should call this;
    libraries that merely report metrics should not call this function.
    It is safe for a library to use a time-based function in an application
    that never collects metrics (the metrics will be wrong, but it doesn't
    matter because they're never read). *)

type metric_type =
  | Counter
  | Gauge
  | Summary
  | Histogram

module type NAME = sig
  type t = private string

  val v : string -> t
  (** Raises an exception if the name is not valid. *)

  val pp : Format.formatter -> t -> unit

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

module LabelSetMap : sig
  include Map.S with type key = string list
  val pp :
    ?sep:(Format.formatter -> unit -> unit) ->
    (Format.formatter -> string list * 'a -> unit) ->
    Format.formatter -> 'a t -> unit
end
(** A map indexed by a set of labels. *)

module MetricFamilyMap : Map.S with type key = MetricInfo.t
(** A map indexed by metric families. *)

module Sample_set : sig
  type sample = {
    ext : string;               (** An extension to append to the base metric name. *)
    value : float;
    bucket : (LabelName.t * float) option;   (** The "le" or "quantile" label and value, if any. *)
  }

  type t = sample list
  (** A collection of values that together represent a single sample.
      For a counter, each reading is just a single value, but more complex types
      require multiple values.
      For example, a "summary" sample set contains "_sum" and "_count" values.
   *)

  val sample : ?ext:string -> ?bucket:(LabelName.t * float) -> float -> sample
end
(** A collection of values that together represent a single sample. *)

module CollectorRegistry : sig
  type t
  (** A collection of metrics to be monitored. *)

  type snapshot = Sample_set.t LabelSetMap.t MetricFamilyMap.t
  (** The result of reading a set of metrics. *)

  val create : unit -> t
  (** [create ()] is a fresh registry. This is mostly useful for testing. *)

  val default : t
  (** The default registry. *)

  val collect : t -> snapshot
  (** Read the current value of each metric.

      @raise Invalid_argument if {!init} has not been called *)

  val register : t -> MetricInfo.t -> (unit -> Sample_set.t LabelSetMap.t) -> unit
  (** [register t metric collector] adds [metric] to the set of metrics being collected.
      It will call [collector ()] to collect the values each time [collect] is called. *)

  val unregister : t -> MetricInfo.t -> unit
  (** [unregister t metric] removes [metric] from the set of metrics being collected. *)

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

  val unregister_labels : family -> string list -> unit
  (** [unregister_labels family label_values] unregisters the metric in [family] with these values
      for the labels. The order of the values must be the same as the order of the [label_names]
      passed to [v_labels]; you may wish to write a wrapper function with labelled arguments to
      avoid mistakes.

      Note that any metric value previously obtained from {!labels} for these label values becomes
      detached by this call. Updates to such a stale value are silently ignored, and later calls
      to {!labels} with the same label values will create a fresh metric starting from zero. *)

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
end
(** A counter is a cumulative metric that represents a single numerical value that only ever goes up. *)

module Gauge : sig
  include METRIC

  val inc_one : t -> unit
  val inc : t -> float -> unit
  (** [inc t v] increases the current value of the gauge by [v]. *)

  val dec_one : t -> unit
  val dec : t -> float -> unit
  (** [dec t v] decreases the current value of the gauge by [v]. *)

  val set : t -> float -> unit
  (** [set t v] sets the current value of the gauge to [v]. *)

  val track_in_progress : t -> (unit -> 'a) -> 'a
  (** [track_in_progress t f] increases the value of the gauge by one while [f ()] runs.
      Use [Prometheus_lwt.Gauge.track_in_progress] to track an Lwt thread. *)

  val set_time : t -> (unit -> 'a) -> 'a
  (** [set_time t f] records the time (in seconds) before and after executing [f ()] and
      sets [t] to the difference.
      Use [Prometheus_lwt.Gauge.set_time] to time an Lwt thread. *)
end
(** A gauge is a metric that represents a single numerical value that can arbitrarily go up and down. *)

module Summary : sig
  include METRIC

  val observe : t -> float -> unit
  (** [observe t v] increases the total by [v] and the count by one. *)

  val observe_time : t -> (unit -> 'a) -> 'a
  (** [observe_time t f] records the time (in seconds) before and after executing [f ()] and
      observes the difference.
      Use [Prometheus_lwt.Summary.observe_time] to time an Lwt thread. *)
end
(** A summary is a metric that records both the number of readings and their total.
    This allows calculating the average. *)

module Histogram_spec : sig
  type t

  val of_linear : float -> float -> int -> t
  (** [of_linear start interval count] will return a histogram type with
      [count] buckets with values starting at [start] and [interval] apart:
      [(start, start+interval, start + (2 * interval), ... start + ((count-1) * interval), infinity)].
      [count] does not include the infinity bucket.
  *)

  val of_exponential : float -> float -> int -> t
  (** [of_exponential start factor count] will return a histogram type with
      [count] buckets with values starting at [start] and every next item [previous*factor].
      [count] does not include the infinity bucket.
  *)

  val of_list : float list -> t
  (** [of_list [0.5; 1.]] will return a histogram with buckets [0.5;1.;infinity]. *)
end

module type HISTOGRAM = sig
  include METRIC

  val observe : t -> float -> unit
  (** [observe t v] adds one to the appropriate bucket for v and adds v to the sum. *)

  val observe_time : t -> (unit -> 'a) -> 'a
  (** [observe_time t f] records the time (in seconds) before and after executing [f ()] and
      observes the difference.
      Use [Prometheus_lwt.Histogram.observe_time] to time an Lwt thread. *)
end

module Histogram (Buckets : sig val spec : Histogram_spec.t end) : HISTOGRAM

module DefaultHistogram : HISTOGRAM
(** A histogram configured with reasonable defaults for measuring network request times in seconds. *)

val get_gettime : unit -> (unit -> int64)
(** Get the time function passed to {!init}.

    If [init] has not been called yet, the returned function always returns 0. *)
