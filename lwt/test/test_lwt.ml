open Prometheus
open Prometheus_app

open Lwt.Infix

(* Exercises the Lwt collection layer: an [register_lwt] collector that suspends,
   collected together with the core's synchronous metrics. *)
let test_lwt_collectors () =
  let registry = Prometheus_lwt.CollectorRegistry.of_registry (CollectorRegistry.create ()) in
  (* A synchronous counter on the underlying core registry... *)
  let sync_counter =
    Counter.v ~registry:(Prometheus_lwt.CollectorRegistry.core registry)
      ~help:"A synchronous counter" "counter_0"
  in
  Counter.inc_one sync_counter;
  (* ...and two suspending Lwt collectors. *)
  let register_counter ~name ~help value =
    let metric_info = {
      MetricInfo.name = MetricName.v name;
      metric_type = Counter;
      help;
      label_names = []
    }
    in
    let collector () =
      Lwt.pause () >|= fun () ->
      LabelSetMap.singleton [] [Prometheus.Sample_set.sample value]
    in
    Prometheus_lwt.CollectorRegistry.register_lwt registry metric_info collector
  in
  register_counter ~name:"counter_1" ~help:"The first counter" 1.0;
  register_counter ~name:"counter_2" ~help:"The second counter" 2.0;
  Prometheus_lwt.CollectorRegistry.collect registry >|= fun collected ->
  let output = Fmt.to_to_string TextFormat_0_0_4.output collected in
  Alcotest.(check string) "Text output"
    "# HELP counter_0 A synchronous counter\n\
     # TYPE counter_0 counter\n\
     counter_0 1\n\
     # HELP counter_1 The first counter\n\
     # TYPE counter_1 counter\n\
     counter_1 1\n\
     # HELP counter_2 The second counter\n\
     # TYPE counter_2 counter\n\
     counter_2 2\n"
    output

let () =
  Lwt_main.run @@ Alcotest_lwt.run "prometheus-lwt" [
    "main", [
      "Lwt collectors", `Quick, test_lwt_collectors;
    ];
  ]
