(** Report metrics for Prometheus.

    See: {{:https://prometheus.io/}https://prometheus.io/}

    Notes:

    - This module is intended to be used by applications that export Prometheus metrics.
      Libraries should only link against the `Prometheus` module.

    - Applications must call {!init} once, inside [Eio_main.run], to register the
      Unix-specific runtime metrics (e.g. [process_start_time_seconds]). The GC
      statistics in {!Prometheus_app} are still registered automatically at
      module load.

    - This extends [Prometheus_app] with support for cmdliner option parsing, a server pre-configured
      for Eio-based applications, and a start-time metric that uses {!Eio.Time.now}.
 *)

type config

val init : clock:_ Eio.Time.clock -> unit -> unit
(** [init ~clock ()] registers the Unix-specific runtime metrics
    ([process_start_time_seconds]) against
    {!Prometheus.CollectorRegistry.default}. The start time is captured from
    [clock] at the moment of the call, so this should be invoked early in the
    program's lifetime, inside [Eio_main.run]. Call once: a second call will
    raise because the metric is already registered. *)

val serve :
  ?backlog:int ->
  ?addr:Eio.Net.Ipaddr.v4v6 ->
  net:_ Eio.Net.t ->
  config ->
  (unit -> unit) list
(** [serve ~net config] returns a (possibly empty) list of fiber bodies, each
    of which serves the Prometheus metrics endpoint at [/metrics] when run.

    Compose them with your application work using {!Eio.Fiber.all} or
    {!Eio.Fiber.fork}. Each fiber owns its listening socket via an internal
    switch; cancelling the fiber closes the socket and shuts the server down.

    @param backlog Maximum length of the pending-connection queue. Default [128].
    @param addr Address to bind to. Default {!Eio.Net.Ipaddr.V4.any} (all IPv4
                interfaces). Pass {!Eio.Net.Ipaddr.V6.any} for IPv6.
    @return An empty list if [config] specifies no port; otherwise a singleton. *)

val opts : config Cmdliner.Term.t
(** [opts] is the extra command-line options to offer Prometheus
    monitoring. *)

(** Report metrics for messages logged. *)
module Logging : sig
  val init :
    clock:_ Eio.Time.clock ->
    ?default_level:Logs.level ->
    ?levels:(string * Logs.level) list ->
    ?formatter:Format.formatter ->
    unit -> unit
  (** Initialise the Logs library with a reporter that reports prometheus metrics too.
      The reporter is configured to log to stderr and the log messages include a
      timestamp (sourced from [clock]) and the event's source.

      Call this from inside [Eio_main.run]:
      {[
      Eio_main.run @@ fun env ->
      let clock = Eio.Stdenv.clock env in
      Prometheus_unix.Logging.init ~clock ()
        ~default_level:Logs.Debug
        ~levels:[
          "cohttp.eio.io", Logs.Info;
        ];
      ...
      ]}
      @param clock Used to source the timestamp shown on each log line.
      @param default_level The default log-level to use (default {!Logs.Info}).
      @param levels Provides levels for specific log sources.
      @param formatter A custom formatter (default {!Fmt.stderr}). *)

  val inc_counter : Logs.level -> string -> unit
  (** [inc_counter level src] increments the count of messages logged by [src] at [level].
      The reporter installed by [init] calls this automatically, but you might want to
      use this if you use your own reporter instead. *)
end
