include Prometheus_reporter_unix

module Server = Prometheus_app.Cohttp(Cohttp_lwt_unix.Server)

let serve = function
  | None -> []
  | Some port ->
    let mode = `TCP (`Port port) in
    let callback = Server.callback in
    let thread = Cohttp_lwt_unix.Server.create ~mode (Cohttp_lwt_unix.Server.make ~callback ()) in
    [thread]
