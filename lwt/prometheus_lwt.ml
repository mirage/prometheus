[@@@alert "-deprecated"]

(* This transition module reexports the core operations under their
   new names to allow the core package to drop the Lwt dependency
   in a future release. This interface will not change. *)
module CollectorRegistry = struct
  type t = Prometheus.CollectorRegistry.t

  let of_registry core = core

  let default = Prometheus.CollectorRegistry.default

  let core t = t

  let collect = Prometheus.CollectorRegistry.collect

  let register = Prometheus.CollectorRegistry.register_lwt

  let register_pre_collect = Prometheus.CollectorRegistry.register_pre_collect_lwt
end

module Gauge = struct
  let track_in_progress = Prometheus.Gauge.track_inprogress

  let set_time = Prometheus.Gauge.time
end

module Summary = struct
  let observe_time = Prometheus.Summary.time
end

module Histogram (H : Prometheus.HISTOGRAM) = struct
  let observe_time = H.time
end
