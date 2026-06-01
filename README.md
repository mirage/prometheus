## OCaml client library for Prometheus monitoring

To run services reliably, it is useful if they can report various metrics
(for example, heap size, queue lengths, number of warnings logged, etc).

A monitoring service can be configured to collect this data regularly.
The data can be graphed to help understand the performance of the service over time,
or to help debug problems quickly.
It can also be used to send alerts if a service is down or behaving poorly.

This repository contains code to report metrics to a [Prometheus][] monitoring server.

### Packages

The library is split so that defining and recording metrics never pulls in a
concurrency library; an application picks a backend (Lwt or Eio) only when it
comes to serving them.

| Package | Adds | Use it for |
|---|---|---|
| `prometheus` | — (astring, asetmap, re) | Defining and recording metrics. Libraries depend only on this. |
| `prometheus-app` | `fmt` | Rendering a snapshot to the text format, plus standard GC collectors. No HTTP, no concurrency library. |
| `prometheus-cohttp` | `cohttp` | A `/metrics` handler functor for any cohttp backend. |
| `prometheus-lwt` (+ `.unix`) | `lwt`, `cohttp-lwt(-unix)`, `cmdliner`, `logs` | Serving metrics from an Lwt application. |
| `prometheus-eio` | `eio`, `cohttp-eio` | Serving metrics from an Eio application (no Lwt). |

### Use by libraries

Library authors should define a set of metrics that may be useful, depending
only on the backend-agnostic `prometheus` package. For example, the DataKitCI
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

Recording is synchronous and backend-agnostic, so this code is identical whether
the eventual application uses Lwt or Eio.

### Use by applications (Lwt)

An Lwt application enables reporting with the `prometheus-lwt` package. The
`prometheus-lwt.unix` library provides the `Prometheus_lwt_unix` module, which
adds a cmdliner option and a pre-configured web-server. See `examples/example.ml`,
which can be run as:

```shell
$ dune exec -- examples/example.exe --listen-prometheus=9090
If run with the option --listen-prometheus=9090, this program serves metrics at
http://localhost:9090/metrics
Tick!
Tick!
...
```

`prometheus-lwt` also provides collectors that may suspend (`register_lwt`), an
Lwt-returning `CollectorRegistry.collect`, and Lwt timing helpers
(`Prometheus_lwt.Gauge.time`, `track_inprogress`, `Prometheus_lwt.Summary.time`).

### Use by applications (Eio)

An Eio application uses `prometheus-eio`, which has no Lwt dependency.
`Prometheus_eio.callback` is a cohttp-eio handler for `/metrics`:

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 9090) in
let socket = Eio.Net.listen ~sw (Eio.Stdenv.net env) ~backlog:5 addr in
let server = Cohttp_eio.Server.make ~callback:Prometheus_eio.callback () in
Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ())
```

A collector that needs to perform I/O while collecting just does so; called from
a fiber, `Prometheus_eio.collect` suspends and resumes around it, so there is no
`register_eio` counterpart to `register_lwt`. Direct-style timing helpers live in
`Prometheus_eio.Gauge` / `Prometheus_eio.Summary`.

### Exporting without an HTTP server

Unikernels and applications that push metrics elsewhere can use `prometheus-app`
directly: `Prometheus_app.TextFormat_0_0_4.output` renders a snapshot, with no
dependency on cohttp, Unix, or a concurrency library.

### API docs

Generated API documentation is available at <https://mirage.github.io/prometheus/>.

## Licensing

This code is licensed under the Apache License, Version 2.0. See
[LICENSE](https://github.com/docker/datakit/blob/master/LICENSE.md) for the full
license text.

[Prometheus]: https://prometheus.io
