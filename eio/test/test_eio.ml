open Prometheus
open Prometheus_reporter

let test_eio_collectors () =
  let registry = CollectorRegistry.create () in
  let register_counter ~name ~help value =
    let metric_info = {
      MetricInfo.name = MetricName.v name;
      metric_type = Counter;
      help;
      label_names = []
    }
    in
    let collector () =
      Eio.Fiber.yield ();
      LabelSetMap.singleton [] [Sample_set.sample value]
    in
    CollectorRegistry.register registry metric_info collector
  in
  register_counter ~name:"counter_1" ~help:"The first counter" 1.0;
  register_counter ~name:"counter_2" ~help:"The second counter" 2.0;
  let collected = Prometheus.CollectorRegistry.collect registry in
  let output = Fmt.to_to_string TextFormat_0_0_4.output collected in
  Alcotest.(check string) "Text output"
    "# HELP counter_1 The first counter\n\
     # TYPE counter_1 counter\n\
     counter_1 1\n\
     # HELP counter_2 The second counter\n\
     # TYPE counter_2 counter\n\
     counter_2 2\n"
    output

let _server = Cohttp_eio.Server.make ~callback:Prometheus_eio.callback ()

let () =
  Eio_main.run @@ fun _env ->
  Alcotest.run "prometheus-eio" [
    "main", [
      Alcotest.test_case "Eio collectors" `Quick test_eio_collectors;
    ];
  ]
