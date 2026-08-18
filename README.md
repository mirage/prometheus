## OCaml client library for Prometheus monitoring

To run services reliably, it is useful if they can report various metrics
(for example, heap size, queue lengths, number of warnings logged, etc).

A monitoring service can be configured to collect this data regularly.
The data can be graphed to help understand the performance of the service over time,
or to help debug problems quickly.
It can also be used to send alerts if a service is down or behaving poorly.

This repository contains code to report metrics to a [Prometheus][] monitoring server.

### Use by libraries

Library authors should define a set of metrics that may be useful. For example, the DataKitCI
cache module defines several metrics like this:

```ocaml
module Metrics = struct
  open Prometheus

  let namespace = "DataKitCI"
  let subsystem = "cache"

  let builds_started_total =
    let help = "Total number of builds started" in
    Counter.v_label ~help ~label_name:"name" ~namespace ~subsystem "builds_started_total"

  let builds_succeeded_total =
    let help = "Total number of builds that succeeded" in
    Counter.v_label ~help ~label_name:"name" ~namespace ~subsystem "builds_succeeded_total"

  let builds_failed_total =
    let help = "Total number of builds that failed" in
    Counter.v_label ~help ~label_name:"name" ~namespace ~subsystem "builds_failed_total"

  [...]
end
```

Each of these metrics has a `name` label, which allows the reports to be further broken down
by the type of thing being built.

When (for example) a build succeeds, the CI does:

```ocaml
Prometheus.Counter.inc_one (Metrics.builds_succeeded_total build_type)
```

### Use by applications

Applications can enable metric reporting using the `prometheus-app` opam package.
This depends on cohttp and can serve the metrics collected above over HTTP.

The `prometheus-app.unix` ocamlfind library provides the `Prometheus_unix` module,
which includes a cmdliner option and pre-configured web-server.
See the `examples/example.ml` program for an example, which can be run as:

```shell
$ dune exec -- examples/example.exe --listen-prometheus=9090
If run with the option --listen-prometheus=9090, this program serves metrics at
http://localhost:9090/metrics
Tick!
Tick!
...
```

Unikernels can use `Prometheus_app` instead of `Prometheus_unix` to avoid the `Unix` dependency.

The `prometheus-reporter` opam package provides the parts of `prometheus-app`
that do not depend on cohttp. `Prometheus_reporter` renders a registry
snapshot in the Prometheus text format and registers the GC collectors, so
applications can serve metrics with any web server such as Dream or Piaf.
`Prometheus_reporter_unix` adds the process start-time metric and a Logs
reporter that counts logged messages.

### Lwt collectors

The `prometheus-lwt` opam package provides `Prometheus_lwt` with collectors
that may suspend before producing their samples, and also Lwt versions of the
timing helpers.

The `prometheus` core does not depend on Lwt, so libraries that only define
and record metrics do not pull in a concurrency library. Code that used the
core's Lwt-typed functions should use `Prometheus_lwt`, or the synchronous
variants such as `Gauge.set_time`.

### Eio collectors

An Eio application uses the `prometheus-eio` opam package.
`Prometheus_eio.callback` is a cohttp-eio handler for `/metrics`:

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 9090) in
let socket = Eio.Net.listen ~sw (Eio.Stdenv.net env) ~backlog:5 addr in
let server = Cohttp_eio.Server.make ~callback:Prometheus_eio.callback () in
let log_warning ex = Logs.warn (fun f -> f "%a" Eio.Exn.pp ex) in
Cohttp_eio.Server.run socket server ~on_error:log_warning
```

### API docs

Generated API documentation is available at <https://mirage.github.io/prometheus/>.

## Licensing

This code is licensed under the Apache License, Version 2.0. See
[LICENSE](https://github.com/docker/datakit/blob/master/LICENSE.md) for the full
license text.

[Prometheus]: https://prometheus.io
