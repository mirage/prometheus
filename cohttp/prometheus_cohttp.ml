module Make (Server : Cohttp.Generic.Server.S) = struct
  open Server.IO

  let callback ~collect _conn req _body =
    match Http.Request.meth req, Http.Request.resource req with
    | `GET, "/metrics" ->
      collect () >>= fun data ->
      let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
      let headers =
        Http.Header.of_list [ "content-type", "text/plain; version=0.0.4" ]
      in
      Server.respond_string ~headers ~status:`OK ~body ()
    | _ ->
      Server.respond_string ~status:`Bad_request ~body:"Bad request" ()
end
