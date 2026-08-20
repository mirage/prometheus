(** Report metrics for Prometheus.

    See: {{:https://prometheus.io/}https://prometheus.io/}

    Notes:

    - This module is intended to be used by applications that export Prometheus metrics.
      Libraries should only link against the `Prometheus` module.

    - This module automatically initialises itself and registers some standard collectors relating to
      GC statistics, as recommended by Prometheus.

    - This extends [Prometheus_reporter_unix] with support for cmdliner option parsing
      and a server pre-configured for Unix.
 *)

module Logging = Prometheus_reporter_unix.Logging
(** Report metrics for messages logged. See {!Prometheus_reporter_unix.Logging}. *)

module Listen_address : sig
  type t =
    [ `TCP of [ `Host of string ] * [ `Port of int ]
    | `Unix_domain_socket of [ `File of string ] ]
  (** The address to serve metrics on, if any. [`Host] is a hostname, an IPv4
      address or an IPv6 address without brackets. *)

  val of_string : string -> (t, [`Msg of string]) result
  (** [of_string s] parses a listen address such as ["9090"],
      ["tcp:127.0.0.1:9090"], ["tcp:[::1]:9090"], ["tcp:localhost"] or
      ["unix:/run/metrics.sock"]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a config in the syntax accepted by {!of_string}. *)

  val default_host : string
  (** [default_host] is ["0.0.0.0"], the interface used when none is given. *)

  val default_port : int
  (** [default_port] is [9090], the port used when none is given. *)
end

type config = Listen_address.t option

val config : ?host:string -> ?port:int -> unit -> config
(** [config ()] is a configuration that serves metrics over TCP on [host]
    (default {!Listen_address.default_host}) and [port]
    (default {!Listen_address.default_port}).
    @raise Invalid_argument if [port] is not between 1 and 65535. *)

val serve : ?registry:Prometheus_lwt.CollectorRegistry.t -> config -> unit Lwt.t list
(** [serve config] starts a Cohttp server according to config.
    It returns a singleton list containing the thread to monitor,
    or an empty list if no server is configured. The socket is bound before
    the thread is returned.
    @param registry The registry to serve, defaulting to
      {!Prometheus_lwt.CollectorRegistry.default}. *)

val opts : config Cmdliner.Term.t
(** [opts] is the extra command-line options to offer Prometheus
    monitoring. It rejects addresses that {!Listen_address} cannot parse. *)
