(** Run this with [example.exe --listen-prometheus=9090].
    View the metrics with:

    curl http://localhost:9090/metrics
   *)

module Metrics = struct
  open Prometheus

  let namespace = "MyProg"
  let subsystem = "main"

  let ticks_counted_total =
    let help = "Total number of ticks counted" in
    Counter.v ~help ~namespace ~subsystem "ticks_counted_total"
end

let counter ~clock () =
  while true do
    Eio.Time.sleep clock 1.0;
    print_endline "Tick!";
    Prometheus.Counter.inc_one Metrics.ticks_counted_total
  done

let main env prometheus_config =
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  Eio.Fiber.all
    ((fun () -> counter ~clock ()) :: Prometheus_unix.serve ~net prometheus_config)

open Cmdliner

(* Optional: configure logging *)
let () =
  Prometheus_unix.Logging.init ()
    ~default_level:Logs.Debug
    ~levels:[
      "cohttp.eio.io", Logs.Info;
    ]

let () =
  Logs.info (fun f -> f "Logging initialised.");
  print_endline "If run with the option --listen-prometheus=9090, this program serves metrics at\n\
                 http://localhost:9090/metrics";
  Eio_main.run @@ fun eio_env ->
  let info = Cmd.info "example" in
  let cmd = Cmd.v info Term.(const (main eio_env) $ Prometheus_unix.opts) in
  exit @@ Cmd.eval cmd
