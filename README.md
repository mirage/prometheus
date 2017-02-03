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

Applications can enable metric reporting using the `Prometheus_app` module.
This depends on cohttp and can serve the metrics collected above over HTTP.
It provides a command-line option, `--listen-prometheus=PORT`, to enable this.

## Licensing

This code is licensed under the Apache License, Version 2.0. See
[LICENSE](https://github.com/docker/datakit/blob/master/LICENSE.md) for the full
license text.

[Prometheus]: https://prometheus.io
