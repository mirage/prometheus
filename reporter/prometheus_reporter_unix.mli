(** Report metrics for Prometheus on Unix.

    See: {{:https://prometheus.io/}https://prometheus.io/}

    Notes:

    - This module is intended to be used by applications that export Prometheus metrics.
      Libraries should only link against the `Prometheus` module.

    - This extends [Prometheus_reporter] with a process start-time metric that uses
      [Unix.gettimeofday] and a Logs reporter that counts logged messages.
 *)

(** Report metrics for messages logged. *)
module Logging : sig
  val init :
    ?default_level:Logs.level ->
    ?levels:(string * Logs.level) list ->
    ?formatter:Format.formatter ->
    unit -> unit
  (** Initialise the Logs library with a reporter that reports prometheus metrics too.
      The reporter is configured to log to stderr and the log messages include a
      timestamp and the event's source.

      A server will typically use the following code to initialise logging:
      {[
      let () = Prometheus_reporter_unix.Logging.init ()
      ]}

      Or:
      {[
      let () =
        Prometheus_reporter_unix.Logging.init ()
          ~default_level:Logs.Debug
          ~levels:[
            "cohttp.lwt.io", Logs.Info;
          ]
      ]}
      @param default_level The default log-level to use (default [Logs.Info]).
      @param levels Provides levels for specific log sources.
      @param formatter A custom formatter (default [Fmt.stderr]). *)

  val inc_counter : Logs.level -> string -> unit
  (** [inc_counter level src] increments the count of messages logged by [src] at [level].
      The reporter installed by [init] calls this automatically, but you might want to
      use this if you use your own reporter instead. *)
end
