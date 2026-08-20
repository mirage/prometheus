module Listen_address = Prometheus_unix.Listen_address

let config = Alcotest.testable Listen_address.pp ( = )

let tcp host port = `TCP (`Host host, `Port port)
let unix_socket path = `Unix_domain_socket (`File path)

let mentions sub s =
  let n = String.length sub and len = String.length s in
  let rec scan i = i + n <= len && (String.sub s i n = sub || scan (i + 1)) in
  scan 0

let parses s expected () =
  Alcotest.(check (result config string)) s (Ok expected)
    (Result.map_error (fun (`Msg m) -> m) (Listen_address.of_string s))

let rejects s expected () =
  match Listen_address.of_string s with
  | Ok c -> Alcotest.failf "Expected %S to be rejected, got %a" s Listen_address.pp c
  | Error (`Msg m) ->
    if not (mentions expected m) then
      Alcotest.failf "Rejected %S with %S, expected it to mention %S" s m expected

let round_trips s () =
  match Listen_address.of_string s with
  | Error (`Msg m) -> Alcotest.failf "%s" m
  | Ok c -> parses (Format.asprintf "%a" Listen_address.pp c) c ()

let accepted = [
  "1", tcp "0.0.0.0" 1;
  "9090", tcp "0.0.0.0" 9090;
  "65535", tcp "0.0.0.0" 65535;
  "tcp:127.0.0.1:9090", tcp "127.0.0.1" 9090;
  "tcp:0.0.0.0:1", tcp "0.0.0.0" 1;
  "tcp:[::1]:9090", tcp "::1" 9090;
  "tcp:[::]:9090", tcp "::" 9090;
  "tcp:[fe80::1]:80", tcp "fe80::1" 80;
  "tcp:localhost:9090", tcp "localhost" 9090;
  "tcp:example.com:65535", tcp "example.com" 65535;
  "tcp:localhost", tcp "localhost" 9090;
  "tcp:127.0.0.1", tcp "127.0.0.1" 9090;
  "tcp:[::1]", tcp "::1" 9090;
  "tcp:localhost:", tcp "localhost" 9090;
  "unix:/run/metrics.sock", unix_socket "/run/metrics.sock";
  "unix:metrics.sock", unix_socket "metrics.sock";
  "unix:./metrics.sock", unix_socket "./metrics.sock";
  "unix:/run/my metrics.sock", unix_socket "/run/my metrics.sock";
  "unix:/run/a:b.sock", unix_socket "/run/a:b.sock";
]

let rejected = [
  "", "Missing scheme";
  " ", "Missing scheme";
  "::1", "Missing scheme";
  "localhost", "Missing scheme";
  "unix", "Missing scheme";
  ":9090", "Missing scheme";
  "0", "out of range";
  "-1", "out of range";
  "65536", "out of range";
  "tcp:localhost:0", "out of range";
  "tcp:localhost:65536", "out of range";
  "localhost:9090", "Unsupported scheme";
  "http:localhost:9090", "Unsupported scheme";
  "TCP:localhost", "Unsupported scheme";
  "UNIX:/run/metrics.sock", "Unsupported scheme";
  "tcp://127.0.0.1:9090", "Invalid address";
  "tcp:", "Invalid address";
  "tcp::9090", "Invalid address";
  "tcp:localhost:abc", "Invalid address";
  "tcp:[::1]:abc", "Invalid address";
  "tcp:[nonsense]:80", "Invalid address";
  "tcp:localhost:9090:9091", "Invalid address";
  "tcp:localhost:9090/metrics", "Invalid address";
  "unix:", "Missing path";
]

let case (s, expected) = Alcotest.test_case (Printf.sprintf "%S" s) `Quick (parses s expected)
let error_case (s, expected) = Alcotest.test_case (Printf.sprintf "%S" s) `Quick (rejects s expected)
let round_trip_case (s, _) = Alcotest.test_case (Printf.sprintf "%S" s) `Quick (round_trips s)

let printing = [
  Alcotest.test_case "ipv6 brackets" `Quick (fun () ->
    Alcotest.(check string) "tcp:[::1]:9090" "tcp:[::1]:9090"
      (Format.asprintf "%a" Listen_address.pp (tcp "::1" 9090)));
]

let () =
  Alcotest.run "prometheus-app" [
    "parse", List.map case accepted;
    "reject", List.map error_case rejected;
    "round-trip", List.map round_trip_case accepted;
    "print", printing;
  ]
