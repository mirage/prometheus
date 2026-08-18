open Eio.Std
open Prometheus
open Prometheus_reporter

let create_registry () =
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
  registry

let expected_output =
  "# HELP counter_1 The first counter\n\
   # TYPE counter_1 counter\n\
   counter_1 1\n\
   # HELP counter_2 The second counter\n\
   # TYPE counter_2 counter\n\
   counter_2 2\n"

let test_eio_collectors () =
  let registry = create_registry () in
  let collected = Prometheus.CollectorRegistry.collect registry in
  let output = Fmt.to_to_string TextFormat_0_0_4.output collected in
  Alcotest.(check string) "Text output" expected_output output

let test_eio_server ~net () =
  (* Test HTTP server *)
  if Sys.os_type <> "Unix" then Alcotest.skip ();
  let registry = create_registry () in
  Switch.run @@ fun sw ->
  let socket = Eio.Net.listen net ~sw ~backlog:128 ~reuse_addr:true (`Unix "./socket") in
  let addr = Uri.make ~scheme:"httpunix" ~host:"./socket" ~path:"/metrics" () in
  let callback = Prometheus_eio.callback ~registry () in
  let server = Cohttp_eio.Server.make ~callback () in
  Fiber.fork_daemon ~sw (fun () -> Cohttp_eio.Server.run socket server ~on_error:raise);
  let client = Cohttp_eio.Client.make net ~https:None in
  let headers, body = Cohttp_eio.Client.get ~sw client addr in
  Alcotest.(check bool) "HTTP status" true (Http.Response.status headers = `OK);
  Alcotest.(check string) "HTTP body" expected_output (Eio.Flow.read_all body)

let () =
  Eio_main.run @@ fun env ->
  Alcotest.run "prometheus-eio" [
    "main", [
      Alcotest.test_case "Eio collectors" `Quick test_eio_collectors;
      Alcotest.test_case "Eio server" `Quick (test_eio_server ~net:env#net);
    ];
  ]
