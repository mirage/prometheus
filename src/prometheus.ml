open! Astring
open! Asetmap

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
  and metric_type =
    | Counter
    | Gauge
    | Summary
    | Histogram

  let pp_metric_type ppf = function
    | Counter   -> Fmt.string ppf "counter"
    | Gauge     -> Fmt.string ppf "gauge"
    | Summary   -> Fmt.string ppf "summary"
    | Histogram -> Fmt.string ppf "histogram"

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

module Sample_set = struct
  type sample = {
    ext : string;
    value : float;
    bucket : (LabelName.t * float) option;
  }

  type t = sample list

  let sample ?(ext="") ?bucket value = { ext; value; bucket }
end

module TextFormat_0_0_4 = struct
  let failf fmt = Fmt.kstrf failwith fmt
  let re_unquoted_escapes = Re.compile @@ Re.set "\\\n"
  let re_quoted_escapes = Re.compile @@ Re.set "\"\\\n"

  let quote g =
    match Re.Group.get g 0 with
    | "\\" -> "\\\\"
    | "\n" -> "\\n"
    | "\"" -> "\\\""
    | x -> failf "Unexpected match %S" x

  let output_unquoted f s =
    Fmt.string f @@ Re.replace re_unquoted_escapes ~f:quote s

  let output_quoted f s =
    Fmt.string f @@ Re.replace re_quoted_escapes ~f:quote s

  let output_value f v =
    match classify_float v with
    | FP_normal | FP_subnormal | FP_zero -> Fmt.float f v
    | FP_infinite when v > 0.0 -> Fmt.string f "+Inf"
    | FP_infinite -> Fmt.string f "-Inf"
    | FP_nan -> Fmt.string f "Nan"

  let output_pairs f (label_names, label_values) =
    let cont = ref false in
    let output_pair name value =
      if !cont then Fmt.string f ", "
      else cont := true;
      Fmt.pf f "%a=\"%a\"" LabelName.pp name output_quoted value
    in
    List.iter2 output_pair label_names label_values

  let output_labels ~label_names f = function
    | [] -> ()
    | label_values -> Fmt.pf f "{%a}" output_pairs (label_names, label_values)

  let output_sample ~base ~label_names ~label_values f { Sample_set.ext; value; bucket } =
    let label_names, label_values = match bucket with
      | None -> label_names, label_values
      | Some (label_name, label_value) ->
        let label_value_str = Fmt.strf "%a" output_value label_value in
        label_name :: label_names, label_value_str :: label_values
    in
    Fmt.pf f "%a%s%a %a@."
      MetricName.pp base ext
      (output_labels ~label_names) label_values
      output_value value

  let output_metric ~name ~label_names f (label_values, samples) =
    List.iter (output_sample ~base:name ~label_names ~label_values f) samples

  let output f =
    MetricFamilyMap.iter (fun metric samples ->
        let {MetricInfo.name; metric_type; help; label_names} = metric in
        Fmt.pf f
          "#HELP %a %a@.\
           #TYPE %a %a@.\
           %a"
          MetricName.pp name output_unquoted help
          MetricName.pp name MetricInfo.pp_metric_type metric_type
          (LabelSetMap.pp ~sep:Fmt.nop (output_metric ~name ~label_names)) samples
      )
end

module CollectorRegistry = struct
  type t = {
    mutable metrics : (unit -> Sample_set.t LabelSetMap.t) MetricFamilyMap.t;
    mutable pre_collect : (unit -> unit) list;
  }

  type snapshot = Sample_set.t LabelSetMap.t MetricFamilyMap.t

  let pp_snapshot = TextFormat_0_0_4.output

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
  val values : t -> Sample_set.t
  val metric_type : MetricInfo.metric_type
  val validate_label : string -> unit
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
    List.iter Child.validate_label label_names;
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
      let values t = [Sample_set.sample !t]
      let metric_type = MetricInfo.Counter
      let validate_label _ = ()
    end)

  let inc_one t =
    t := !t +. 1.0

  let inc t v =
    assert (v >= 0.0);
    t := !t +. v
end

module Gauge = struct
  include Metric(struct
      type t = float ref
      let create () = ref 0.0
      let values t = [Sample_set.sample !t]
      let metric_type = MetricInfo.Gauge
      let validate_label _ = ()
    end)

  let inc t v =
    t := !t +. v
  let inc_one t = inc t 1.0

  let dec t x = inc t (-. x)
  let dec_one t = dec t 1.0

  let set t v =
    t := v
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
        Sample_set.sample ~ext:"_sum" t.sum;
        Sample_set.sample ~ext:"_count" t.count;
      ]
    let metric_type = MetricInfo.Summary

    let validate_label = function
      | "quantile" -> failwith "Can't use special label 'quantile' in summary"
      | _ -> ()
  end
  include Metric(Child)

  let observe t v =
    let open Child in
    t.count <- t.count +. 1.0;
    t.sum <- t.sum +. v
end

module Histogram_spec = struct
  type t = float array (* Upper bounds *)

  let make at_index_f count =
    let real_at_index i =
      if i >= count then
        infinity
      else
        at_index_f i
    in
    Array.init (count + 1) real_at_index

  let of_linear start interval count =
    let at_index i =
      let f = float_of_int i in
      start +. (interval *. f)
    in
    make at_index count

  let of_exponential start factor count =
    let at_index i =
      let multiplier = factor ** (float_of_int i) in
      start *. multiplier
    in
    make at_index count

  let of_list lst =
    let length = List.length lst in
    make (List.nth lst) length

  (* The index at which to record a value [v]. *)
  let index t v =
    let rec aux index =
      if v <= t.(index) then index
      else aux (index + 1)
    in
    aux 0
end

module type BUCKETS = sig
  val spec : Histogram_spec.t
end

module type HISTOGRAM = sig
  include METRIC
  val observe : t -> float -> unit
end

let bucket_label = LabelName.v "le"

module Histogram (Buckets : BUCKETS) = struct
  module Child = struct
    type t = {
      upper_bounds : Histogram_spec.t;
      counts : float array;
      mutable sum : float;
    }

    let create () =
      let count = Array.length Buckets.spec in
      let counts = Array.make count 0. in
      { upper_bounds = Buckets.spec; counts; sum = 0. }

    let values t =
      let count = Array.length t.counts in
      let rec fold val_acc acc index =
        if index = count then
          Sample_set.sample ~ext:"_sum" t.sum ::
          Sample_set.sample ~ext:"_count" val_acc ::
          acc
        else
          let val_acc = t.counts.(index) +. val_acc in
          let bucket = (bucket_label, t.upper_bounds.(index)) in
          let acc = Sample_set.sample ~ext:"_bucket" val_acc ~bucket :: acc in
          fold val_acc acc (index + 1)
      in
      fold 0. [] 0

    let metric_type = MetricInfo.Histogram

    let validate_label = function
      | "le" -> failwith "Can't use special label 'le' in histogram"
      | _ -> ()
  end

  include Metric(Child)

  let observe t v =
    let open Child in
    let index = Histogram_spec.index t.upper_bounds v in
    t.counts.(index) <- t.counts.(index) +. 1.;
    t.sum <- t.sum +. v
end

module DefaultHistogram = Histogram (
  struct
    let spec =
      Histogram_spec.of_list [0.005;  0.01; 0.025; 0.05;
                              0.075;  0.1 ; 0.25 ; 0.5;
                              0.75 ;  1.  ; 2.5  ; 5.;
                              7.5  ; 10.  ]
  end)
