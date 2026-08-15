module Logging = Prometheus_reporter_unix.Logging

type config = int option

module Server = Prometheus_app.Cohttp(Cohttp_lwt_unix.Server)

let serve = function
  | None -> []
  | Some port ->
    let mode = `TCP (`Port port) in
    let callback = Server.callback in
    let thread = Cohttp_lwt_unix.Server.create ~mode (Cohttp_lwt_unix.Server.make ~callback ()) in
    [thread]

let listen_prometheus =
  let open! Cmdliner in
  let doc =
    Arg.info ~docs:"MONITORING OPTIONS" ~docv:"PORT" ~doc:
      "Port on which to provide Prometheus metrics over HTTP."
      ["listen-prometheus"]
  in
  Arg.(value @@ opt (some int) None doc)

let opts = listen_prometheus
