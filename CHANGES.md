## v2.0-dev

- `prometheus-app`: add `Prometheus_unix.config` to build a configuration
  without cmdliner (@avsm @talex5 requested by @Nymphium #44 #73)

- `prometheus-app`: allow serving a registry other than the default one.
  (@avsm #54)

  `Prometheus_app.Cohttp(S).callback` and `Prometheus_unix.serve` take an
  optional `?registry` argument.

- `prometheus-app`: allow specifying the address as well as the port to bind
  to with `--listen-prometheus`.

  As well as a bare port number, the option now accepts `tcp:HOST[:PORT]`
  (with IPv6 addresses in square brackets, and the port defaulting to 9090)
  and `unix:PATH` for domain sockets, in the same syntax as `capnp-rpc-unix`.
  (@rbjorklin @avsm @talex5 #51 #72)

- Add a new `prometheus-eio` serving package (@mtelvers @avsm #60)

  `Prometheus_eio.callback ()` is a cohttp-eio handler that serves the
  default registry at `/metrics`. It depends only on `prometheus-reporter`,
  so no Lwt or cohttp-lwt code is linked into an Eio application.

- Add a new `prometheus-reporter` package with the cohttp-free parts of
  `prometheus-app` (@ulrikstrid @avsm #39 #46)

  `Prometheus_reporter` renders the text format and registers the GC
  collectors. `Prometheus_reporter_unix` adds the process start-time metric
  and the Logs reporter. `prometheus-app` reexports both so existing code
  keeps working. Applications can now serve the rendered metrics with
  any web server of choice.

- Remove Lwt from the `prometheus` core (@mtelvers @avsm #60 #65)

  The deprecated Lwt functions are removed from the core package, and
  `CollectorRegistry.collect` now returns a snapshot directly.
  Lwt collectors are all in `prometheus-lwt`, whose interface is unchanged
  from the v1.4 release.

  Applications that collected via the core in an Lwt context should use
  `Prometheus_lwt.CollectorRegistry.collect`.

## v1.4 (2026-08-09)

Core/Lwt split:

- Add a new `prometheus-lwt` metrics package.
  (@avsm @mtelvers @talex5 #66 #67 #68 #69, reviewed by @dinosaure)

  This release is not a breaking change, but instead prepares for
  deprecation of Lwt in the core package so a future release can remove Lwt
  from the `prometheus` core. Users of the core's Lwt-typed functions
  should migrate to `Prometheus_lwt` now. Code using the new interface will
  keep working unchanged in future releases.

- Add synchronous variants of the timing helpers to the core. Metric recording
  is synchronous and these will become the only core timing helpers once the
  Lwt-typed ones move to `prometheus-lwt`.

- The replacement time-based functions no longer take a `gettime` argument.
  Instead, this is provided once by the new `Prometheus.init` function,
  which is called automatically when using `Prometheus_unix`.

To migrate existing code, add a dep on `prometheus-lwt` and rename following
the deprecation warnings from the compiler.

Bug fixes:

- Format floats with `%.17g` (@samoht @talex5 #62).  
  The text exposition formatter printed metric values with `%f`,
  so any magnitude below ~5e-7 was reported as 0.000000.

Other changes:

- Remove `Astring` dependency (@talex5 #63).

- Remove `Asetmap` dependency (@talex5 #64).  
  This changes the types of `LabelSetMap` and `MetricFamilyMap` slightly,
  which might affect custom reporters.

## v1.3 (2025-12-08)

- Make help / type information be OpenMetrics compatible (@Nymphium @tmcgilchrist #47).  
  e.g. using `# TYPE` rather than `#TYPE` in the output.

- Minor documentation fixes (@tmcgilchrist @vch9 #50 #53).

- Make default `Makefile` target run tests (@talex5 #49).

## v1.2 (2022-06-16)

- Add lwt collectors and pre-collectors (@killian-delarue, #43).
  Note that this is a temporary feature while we wait for OCaml 5 to be released,
  when this can be replaced by the use of effects.

- Fix deprecations in Fmt 0.8.10 (@MisterDA, #36).

- General build updates, upstream deprecations, etc (@talex5, #33 #34 #35 #40 #42).

## v1.1 (2021-06-08)

- Allow using a custom formatter for log output (@MisterDA #31).
  Windows services crash if they try to use stderr.

## v1.0 (2020-12-22)

- Add logging configuration (#29, @talex5).  
  To configure a server to report counts for log messages:
  ```ocaml
  let () = Prometheus_unix.Logging.init ()
  ```
  This installs a reporter that reports the number of messages logged by each log source and at each level.
  The reporter also displays the timestamp and log source with each message, which is a more suitable configuration for servers.

- Add bounds on cohttp to prepare for cohttp 3 release (#28, @talex5).

## v0.7 (2020-03-03)

- switch float representation to OCaml's default `"%f"` (#22, @toots)
- use `Gc.quick_stat` for faster stats (#25, @talex5)

## v0.6 (2019-11-23)

- upgrade build to dune (@talex5)
- upgrade to opam2 format (@talex5)

## v0.5 (2017-12-20)

- prometheus-app: update to cohttp.1.0.0 API (#15, @djs55)
- add support for histograms (#14, @stijn-devriendt and @talex5)
- add `Sample_set module` to clean up the API a bit (#13, @talex5)
- fix gettimeofday parameter not used in favor of Unix.gettimeofdaya (#12, @stijn-devriendt)

## v0.4 (2017-08-02)

- unix: update to cohttp >= 0.99.0. Note this means the unix package
  requires OCaml 4.03+. The main library still only requires OCaml 4.01+

## v0.3 (2017-07-03)

- Build tweaks to support topkg versioning (@avsm)

## v0.2 (2017-05-18)

- add example program and update README
- switch to jbuilder
- throw a clearer error on registering a duplicate metric
- use `Re` rather than `Str`

## v0.1

- Initial release.
