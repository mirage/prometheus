open! Astring
open Asetmap

module type NAME_SPEC = sig
  val valid : Re.re
end

module type NAME = sig
  type t = private string
  val v : string -> t
  val pp : t Fmt.t
  val compare : t -> t -> int
end

module Name(N : NAME_SPEC) : NAME = struct
  type t = string

  let v name =
    if not (Re.execp N.valid name) then
      failwith (Fmt.strf "Invalid name %S" name);
    name

  let compare = String.compare

  let pp = Fmt.string
end

let alphabet = Re.(alt [ rg 'a' 'z'; rg 'A' 'Z' ])
module LabelName = struct
  (* "^[a-zA-Z_][a-zA-Z0-9_]*$" *)
  let start = Re.alt [ alphabet; Re.char '_' ]
  let rest  = Re.alt [ start; Re.digit ]
  include Name(struct let valid = Re.compile @@ Re.seq [ Re.bos; start; Re.rep rest; Re.eos] end)
end
module MetricName = struct
  (* "^[a-zA-Z_:][a-zA-Z0-9_:]*$"  *)
  let start = Re.alt [ LabelName.start; Re.char ':' ]
  let rest = Re.alt [ start; Re.digit ]
  include Name(struct let valid = Re.compile @@ Re.seq [ Re.bos; start; Re.rep rest; Re.eos] end)
end

type metric_type =
  | Counter
  | Gauge
  | Summary
  | Histogram

module LabelSet = struct
  type t = string list
  let compare (a:t) (b:t) = compare a b
end
module LabelSetMap = Map.Make(LabelSet)

module MetricInfo = struct
  type t = {
    name : MetricName.t;
    metric_type : metric_type;
    help : string;
    label_names : LabelName.t list;
  }

  let pp_opt () = function
    | None -> ""
    | Some v -> v ^ "_"

  let v ~help ?(label_names=[]) ~metric_type ?namespace ?subsystem name =
    let name = Printf.sprintf "%a%a%s" pp_opt namespace pp_opt subsystem name in
    {
      name = MetricName.v name;
      metric_type;
      help;
      label_names;
    }

  let compare a b = MetricName.compare a.name b.name
end

module MetricFamilyMap = Map.Make(MetricInfo)

module CollectorRegistry = struct
  type t = {
    mutable metrics : (unit -> (string * float * ((LabelName.t * float) option)) list LabelSetMap.t) MetricFamilyMap.t;
    mutable pre_collect : (unit -> unit) list;
  }

  type snapshot = (string * float * ((LabelName.t * float) option)) list LabelSetMap.t MetricFamilyMap.t

  let create () = {
    metrics = MetricFamilyMap.empty;
    pre_collect = [];
  }

  let default = create ()

  let register_pre_collect t f = t.pre_collect <- f :: t.pre_collect

  let register t info collector =
    if MetricFamilyMap.mem info t.metrics
    then failwith (Fmt.strf "%a already registered" MetricName.pp info.MetricInfo.name);
    t.metrics <- MetricFamilyMap.add info collector t.metrics

  let collect t =
    List.iter (fun f -> f ()) t.pre_collect;
    MetricFamilyMap.map (fun f -> f ()) t.metrics
end

module type METRIC = sig
  type family
  type t
  val v_labels : label_names:string list -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> family
  val labels : family -> string list -> t
  val v_label : label_name:string -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> (string -> t)
  val v : ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> t
end

module type CHILD = sig
  type t
  val create : unit -> t
  val values : t -> (string * float * ((LabelName.t * float) option)) list       (* extension, value, (extra label name, extra label value) *)
  val metric_type : metric_type
  val is_valid_label : string -> bool
end

module Metric(Child : CHILD) : sig
  include METRIC with type t = Child.t
end = struct
  type family = {
    metric : MetricInfo.t;
    mutable children : Child.t LabelSetMap.t;
  }

  type t = Child.t

  let collect t =
    LabelSetMap.map Child.values t.children

  let v_labels ~label_names ?(registry=CollectorRegistry.default) ~help ?namespace ?subsystem name =
    if List.exists (fun name -> not (Child.is_valid_label name)) label_names then
      failwith (Fmt.strf "Invalid name %S" name);
    let label_names = List.map LabelName.v label_names in
    let metric = MetricInfo.v ~metric_type:Child.metric_type ~help ~label_names ?namespace ?subsystem name in
    let t = {
      metric;
      children = LabelSetMap.empty;
    } in
    CollectorRegistry.register registry metric (fun () -> collect t);
    t

  let labels t label_values =
    assert (List.length t.metric.MetricInfo.label_names = List.length label_values);
    match LabelSetMap.find label_values t.children with
    | Some child -> child
    | None ->
      let child = Child.create () in
      t.children <- LabelSetMap.add label_values child t.children;
      child

  let v_label ~label_name ?registry ~help ?namespace ?subsystem name =
    let family = v_labels ~label_names:[label_name] ?registry ~help ?namespace ?subsystem name in
    fun x -> labels family [x]

  let v ?registry ~help ?namespace ?subsystem name =
    let family = v_labels ~help ?registry ?namespace ?subsystem name ~label_names:[] in
    labels family []
end

module Counter = struct
  include Metric(struct
      type t = float ref
      let create () = ref 0.0
      let values t = [("", !t, None)]
      let metric_type = Counter
      let is_valid_label _str = true
    end)

  let inc_one t =
    t := !t +. 1.0

  let inc t v =
    assert (v >= 0.0);
    t := !t +. v

  let get t = !t
end

module Gauge = struct
  include Metric(struct
      type t = float ref
      let create () = ref 0.0
      let values t = [("", !t, None)]
      let metric_type = Gauge
      let is_valid_label _str = true
    end)

  let inc t v =
    t := !t +. v
  let inc_one t = inc t 1.0

  let dec t x = inc t (-. x)
  let dec_one t = dec t 1.0

  let set t v =
    t := v

  let get t = !t

  let track_inprogress t fn =
    inc_one t;
    Lwt.finalize fn (fun () -> dec_one t; Lwt.return_unit)

  let time t gettimeofday fn =
    let start = gettimeofday () in
    Lwt.finalize fn
      (fun () ->
         let finish = gettimeofday () in
         inc t (finish -. start);
         Lwt.return_unit
      )
end

module Summary = struct
  module Child = struct
    type t = {
      mutable count : float;
      mutable sum : float;
    }
    let create () = { count = 0.0; sum = 0.0 }
    let values t =
      [
        "_sum", t.sum, None;
        "_count", t.count, None;
      ]
    let metric_type = Summary
    let is_valid_label str = not (String.is_prefix ~affix:"quantile" str)
  end
  include Metric(Child)

  let get t =
    Child.(t.sum, t.count)

  let get_average t =
    let sum, count = get t in
    sum /. count

  let observe t v =
    let open Child in
    t.count <- t.count +. 1.0;
    t.sum <- t.sum +. v

  let time t gettimeofday fn =
    let start = gettimeofday () in
    Lwt.finalize fn
      (fun () ->
         let finish = gettimeofday () in
         observe t (finish -. start);
         Lwt.return_unit
      )
end

type histogram = {
    upper_bounds: float array;
    counts: float array;
    mutable sum: float;
  }

let histogram at_index_f count =
  let real_at_index i =
    if i >= count then
      infinity
    else
      at_index_f i
  in
  let upper_bounds = Array.init (count + 1) real_at_index in
  let counts = Array.make (count + 1) 0. in
  { upper_bounds; counts; sum=0.; }

let histogram_of_linear start interval count =
  let at_index i =
    let f = float_of_int i in
    start +. (interval *. f)
  in
  histogram at_index count

let histogram_of_exponential start factor count =
  let at_index i =
    let f = float_of_int i in
    let multiplier = factor ** (f +. 1.) in
    start *. multiplier
  in
  histogram at_index count

let histogram_of_list lst =
  let length = List.length lst in
  histogram (List.nth lst) length


module type BUCKETS = sig
  val create: unit -> histogram
end

module type HISTOGRAM = sig
  include METRIC
  val observe : t -> float -> unit
  (** [observe t v] adds one to the appropriate bucket for v and adds v to the sum.*)

  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      observes the difference. *)

  val get_all : t -> (float * float) array
  (** [get_all t] returns a list of buckets and counts. *)

  val get_count : t -> float -> float
  (** [get_count t v] returns the bucket count for the bucket that would accept v. *)

  val get_sum : t -> float
  (** [get_sum t] returns the sum of all observed values. *)
end

let bucket_label = LabelName.v "le"

module Histogram (Buckets: BUCKETS) = struct
  module Child = struct
    type t = histogram

    let create = Buckets.create

    let values t =
      let count = Array.length t.counts in
      let rec fold val_acc acc index =
        if index = count then
          ("_sum", t.sum, None) :: ("_count", val_acc, None) :: acc
        else
          let val_acc = t.counts.(index) +. val_acc in
          let label = Some (bucket_label, t.upper_bounds.(index)) in
          let acc = ("_bucket", val_acc, label) :: acc in
          fold val_acc acc (index + 1)
      in
      fold 0. [] 0

    let metric_type = Histogram
    let is_valid_label str = not (String.is_prefix ~affix:"le" str)
  end
  include Metric(Child)

  let observe t v =
    let rec impl index =
      if v <= t.upper_bounds.(index) then
        t.counts.(index) <- t.counts.(index) +. 1.
      else
        impl (index + 1)
    in
    let () = impl 0 in
    t.sum <- t.sum +. v

  let time t gettimeofday fn =
    let start = gettimeofday () in
    Lwt.finalize fn
      (fun () ->
         let finish = gettimeofday () in
         observe t (finish -. start);
         Lwt.return_unit
      )

  let get_all t =
    Array.map2 (fun upper_bound value -> (upper_bound, value)) t.upper_bounds t.counts

  let get_count t v =
    let rec impl index =
      if v <= t.upper_bounds.(index) then
        t.counts.(index)
      else
        impl (index + 1)
    in
    impl 0

  let get_sum t =
    t.sum
end

module DefaultHistogram = Histogram (
  struct
    let create () =
      histogram_of_list [0.005;  0.01; 0.025; 0.05;
                         0.075;  0.1 ; 0.25 ; 0.5;
                         0.75 ;  1.  ; 2.5  ; 5.;
                         7.5  ; 10.  ]

  end)

