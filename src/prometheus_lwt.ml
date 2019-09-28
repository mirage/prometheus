open Prometheus

module Gauge = struct
  include Gauge
  let track_inprogress t fn =
    inc_one t;
    Lwt.finalize fn (fun () -> dec_one t; Lwt.return_unit)

  let time t gettimeofday fn =
    let start = gettimeofday () in
    Lwt.finalize fn
      (fun () ->
         let finish = gettimeofday () in
         inc t (finish -. start);
         Lwt.return_unit
      )
end

module Summary = struct
  include Summary

  let time t gettimeofday fn =
    let start = gettimeofday () in
    Lwt.finalize fn
      (fun () ->
         let finish = gettimeofday () in
         observe t (finish -. start);
         Lwt.return_unit
      )
end

module type HISTOGRAM = sig
  include METRIC
  include HISTOGRAM
  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

module Histogram (Buckets : BUCKETS) = struct
  include Histogram(Buckets)

  let time t gettimeofday fn =
    let start = gettimeofday () in
    Lwt.finalize fn
      (fun () ->
         let finish = gettimeofday () in
         observe t (finish -. start);
         Lwt.return_unit
      )
end

module DefaultHistogram = Histogram (
  struct
    let spec =
      Histogram_spec.of_list [0.005;  0.01; 0.025; 0.05;
                              0.075;  0.1 ; 0.25 ; 0.5;
                              0.75 ;  1.  ; 2.5  ; 5.;
                              7.5  ; 10.  ]
  end)
