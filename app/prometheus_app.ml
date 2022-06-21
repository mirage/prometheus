include Prometheus_reporter

open Lwt.Infix

module Cohttp(Server : Cohttp_lwt.S.Server) = struct
  let callback _conn req _body =
    let open Cohttp in
    let uri = Request.uri req in
    match Request.meth req, Uri.path uri with
    | `GET, "/metrics" ->
      Prometheus.CollectorRegistry.(collect default) >>= fun data ->
      let body = Fmt.to_to_string TextFormat_0_0_4.output data in
      let headers = Header.init_with "Content-Type" "text/plain; version=0.0.4" in
      Server.respond_string ~status:`OK ~headers ~body ()
    | _ -> Server.respond_error ~status:`Bad_request ~body:"Bad request" ()
end
