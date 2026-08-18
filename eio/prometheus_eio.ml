(* Eio support for Prometheus. *)

let callback ?(registry=Prometheus.CollectorRegistry.default) () _conn req _body =
  match Http.Request.meth req, Http.Request.resource req with
  | `GET, "/metrics" ->
    let data = Prometheus.CollectorRegistry.collect registry in
    let body = Fmt.to_to_string Prometheus_reporter.TextFormat_0_0_4.output data in
    let headers =
      Http.Header.of_list [ "content-type", "text/plain; version=0.0.4" ]
    in
    Cohttp_eio.Server.respond_string ~headers ~status:`OK ~body ()
  | _ ->
    Cohttp_eio.Server.respond_string ~status:`Bad_request ~body:"Bad request" ()
