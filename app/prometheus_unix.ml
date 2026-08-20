module Logging = Prometheus_reporter_unix.Logging
module Server = Prometheus_app.Cohttp(Cohttp_lwt_unix.Server)

module Listen_address = struct
  type t =
    [ `TCP of [ `Host of string ] * [ `Port of int ]
    | `Unix_domain_socket of [ `File of string ] ]

  let default_host = "0.0.0.0"
  let default_port = 9090

  let syntax = "expected PORT, tcp:HOST[:PORT] or unix:PATH"

  let pp f = function
    | `TCP (`Host host, `Port port) ->
      let host = if String.contains host ':' then "[" ^ host ^ "]" else host in
      Fmt.pf f "tcp:%s:%d" host port
    | `Unix_domain_socket (`File path) -> Fmt.pf f "unix:%s" path

  let error = Fmt.error_msg

  let valid_port port = port > 0 && port <= 65535

  let parse_tcp s host_port =
    let uri = Uri.of_string ("tcp://" ^ host_port) in
    match Uri.host uri, Uri.port uri, Uri.path uri with
    | Some host, port, ("" | "/") when host <> "" ->
      let port = Option.value port ~default:default_port in
      if valid_port port then Ok (`TCP (`Host host, `Port port))
      else error "Port %d is out of range in %S" port s
    | _ -> error "Invalid address %S, expected tcp:HOST[:PORT]" s

  let of_string s =
    match int_of_string_opt s with
    | Some port when valid_port port ->
      Ok (`TCP (`Host default_host, `Port port))
    | Some _ -> error "Port %S is out of range" s
    | None ->
      match String.index_opt s ':' with
      | None | Some 0 -> error "Missing scheme in %S, %s" s syntax
      | Some i ->
        let rest = String.sub s (i + 1) (String.length s - i - 1) in
        match String.sub s 0 i with
        | "tcp" -> parse_tcp s rest
        | "unix" when rest = "" -> error "Missing path in %S, expected unix:PATH" s
        | "unix" -> Ok (`Unix_domain_socket (`File rest))
        | scheme -> error "Unsupported scheme %S in %S, %s" scheme s syntax
end

type config = Listen_address.t option

let sockaddr = function
  | `Unix_domain_socket (`File path) -> Unix.ADDR_UNIX path
  | `TCP (`Host host, `Port port) ->
    let hints = Unix.[ AI_SOCKTYPE SOCK_STREAM; AI_PASSIVE ] in
    match Unix.getaddrinfo host (string_of_int port) hints with
    | [] -> Fmt.failwith "Unable to resolve listen address %S" host
    | ai :: _ -> ai.Unix.ai_addr

let listen sockaddr =
  let open Unix in
  let socket = socket ~cloexec:true (domain_of_sockaddr sockaddr) SOCK_STREAM 0 in
  (try
     setsockopt socket SO_REUSEADDR true;
     bind socket sockaddr;
     listen socket 128
   with exn -> close socket; raise exn);
  `TCP (`Socket (Lwt_unix.of_unix_file_descr socket))

let serve = function
  | None -> []
  | Some conf ->
    let mode = listen (sockaddr conf) in
    let callback = Server.callback in
    let thread = Cohttp_lwt_unix.Server.create ~mode (Cohttp_lwt_unix.Server.make ~callback ()) in
    [thread]

let listen_prometheus =
  let open! Cmdliner in
  let addr = Arg.conv ~docv:"ADDR" Listen_address.(of_string, pp) in
  let doc =
    Arg.info ~docs:"MONITORING OPTIONS" ~docv:"ADDR" ~doc:
      "Address on which to provide Prometheus metrics over HTTP. This is \
       either a port number, $(b,tcp:HOST[:PORT]) or $(b,unix:PATH), for \
       example $(b,9090), $(b,tcp:127.0.0.1:9090), $(b,tcp:[::1]:9090), \
       $(b,tcp:localhost) or $(b,unix:/run/metrics.sock). The default \
       listen address and port are 0.0.0.0:9090."
      ["listen-prometheus"]
  in
  Arg.(value @@ opt (some addr) None doc)

let opts = listen_prometheus
