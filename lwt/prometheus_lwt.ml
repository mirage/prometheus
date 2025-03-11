(* The Lwt collectors for Prometheus metrics. *)

open Prometheus

let time_delta t0 t1 =
  Int64.to_float (Int64.sub t1 t0) /. 1e9

module CollectorRegistry = struct
  type t = {
    core : Prometheus.CollectorRegistry.t;
    mutable metrics_lwt : (unit -> Sample_set.t LabelSetMap.t Lwt.t) MetricFamilyMap.t;
    mutable pre_collect_lwt : (unit -> unit Lwt.t) list;
  }

  let of_registry core = {
    core;
    metrics_lwt = MetricFamilyMap.empty;
    pre_collect_lwt = [];
  }

  let default = of_registry Prometheus.CollectorRegistry.default

  let core t = t.core

  let register_pre_collect t f = t.pre_collect_lwt <- f :: t.pre_collect_lwt

  let register t info collector =
    if MetricFamilyMap.mem info t.metrics_lwt then
      failwith (Format.asprintf "%a already registered" MetricName.pp info.MetricInfo.name);
    t.metrics_lwt <- MetricFamilyMap.add info collector t.metrics_lwt

  let unregister t info =
    t.metrics_lwt <- MetricFamilyMap.remove info t.metrics_lwt

  open Lwt.Infix

  let map_p m =
    MetricFamilyMap.fold (fun k f acc -> (k, f ()) :: acc) m []
    |> Lwt_list.fold_left_s
      (fun acc (k, v) -> v >|= fun v -> MetricFamilyMap.add k v acc)
      MetricFamilyMap.empty

  let collect t =
    Lwt_list.iter_p (fun f -> f ()) t.pre_collect_lwt >>= fun () ->
    let sync = Prometheus.CollectorRegistry.collect t.core in
    map_p t.metrics_lwt >|= fun metrics_lwt ->
    MetricFamilyMap.merge
      (fun _ v1 v2 ->
         match v1 with
         | Some v1 -> Some v1
         | None -> v2)
      sync metrics_lwt
end

module Gauge = struct
  let track_in_progress t fn =
    Prometheus.Gauge.inc_one t;
    Lwt.finalize fn (fun () -> Prometheus.Gauge.dec_one t; Lwt.return_unit)

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
