open Prometheus

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

  let register_pre_collect_lwt t f = t.pre_collect_lwt <- f :: t.pre_collect_lwt

  let register_lwt t info collector =
    if MetricFamilyMap.mem info t.metrics_lwt then
      failwith (Format.asprintf "%a already registered" MetricName.pp info.MetricInfo.name);
    t.metrics_lwt <- MetricFamilyMap.add info collector t.metrics_lwt

  open Lwt.Infix

  let map_p m =
    MetricFamilyMap.fold (fun k f acc -> (k, f ()) :: acc) m []
    |> Lwt_list.fold_left_s
      (fun acc (k, v) -> v >|= fun v -> MetricFamilyMap.add k v acc)
      MetricFamilyMap.empty

  let collect t =
    (* Run the Lwt pre-collect hooks, then the core's synchronous collection
       (which runs its own pre-collect hooks and reads the in-memory metrics),
       then this view's Lwt collectors concurrently, and merge — synchronous
       values winning on a name clash. *)
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
  let track_inprogress t fn =
    Prometheus.Gauge.inc_one t;
    Lwt.finalize fn (fun () -> Prometheus.Gauge.dec_one t; Lwt.return_unit)

  let time t gettimeofday fn =
    let start = gettimeofday () in
    Lwt.finalize fn
      (fun () ->
         let finish = gettimeofday () in
         Prometheus.Gauge.inc t (finish -. start);
         Lwt.return_unit)
end

module Summary = struct
  let time t gettimeofday fn =
    let start = gettimeofday () in
    Lwt.finalize fn
      (fun () ->
         let finish = gettimeofday () in
         Prometheus.Summary.observe t (finish -. start);
         Lwt.return_unit)
end

module Cohttp(Server : Cohttp_lwt.S.Server) = struct
  module M = Prometheus_cohttp.Make(Server)

  let callback conn req body =
    M.callback ~collect:(fun () -> CollectorRegistry.collect CollectorRegistry.default)
      conn req body
end
