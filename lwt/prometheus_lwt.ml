[@@@alert "-deprecated"]

let time_delta t0 t1 =
  Int64.to_float (Int64.sub t1 t0) /. 1e9

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

  let set_time t fn =
    let gettime = Prometheus.get_gettime () in
    let start = gettime () in
    Lwt.finalize fn
      (fun () ->
         let finish = gettime () in
         Prometheus.Gauge.set t (time_delta start finish);
         Lwt.return_unit
      )
end

module Summary = struct
  let observe_time t fn =
    let gettime = Prometheus.get_gettime () in
    let start = gettime () in
    Lwt.finalize fn
      (fun () ->
         let finish = gettime () in
         Prometheus.Summary.observe t (time_delta start finish);
         Lwt.return_unit
      )
end

module Histogram (H : Prometheus.HISTOGRAM) = struct
  let observe_time t fn =
    let gettime = Prometheus.get_gettime () in
    let start = gettime () in
    Lwt.finalize fn
      (fun () ->
         let finish = gettime () in
         H.observe t (time_delta start finish);
         Lwt.return_unit
      )
end
