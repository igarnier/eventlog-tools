type phase

type bucket = int

type counter_kind

type event_payload =
  | Entry of { phase : phase }
  | Exit of { phase : phase }
  | Alloc of { count : int; bucket : bucket }
  | Counter of { count : int; kind : counter_kind }
  | Flush of { duration : int }

type event = { payload : event_payload; timestamp : int; pid : int }

type endianness = Be | Le

type trace_header = { endianness : endianness; ocaml_trace_version : int }

type packet = Header of trace_header | Event of event

val string_of_gc_counter : counter_kind -> string

val string_of_alloc_bucket : bucket -> string

val string_of_phase : phase -> string

val phase_of_string : string -> phase

val counter_kind_of_string : string -> counter_kind

module Parser : sig
  type decoder

  val decoder : unit -> decoder

  val decode :
    decoder -> [> `Await | `End | `Error of [> `Msg of string ] | `Ok of packet ]

  val src : decoder -> Bigstringaf.t -> int -> int -> bool -> unit

  val parse_event : endianness -> packet Angstrom.t
end

module Timeseries : sig
  type 'a t = private (float * 'a) list

  val of_raw : (float * 'a) list -> 'a t

  val duration : 'a t -> float

  val cumulative : float t -> float t

  val bin : timeslice:float -> 'a t -> 'a t array

  val sum : float t -> float

  val length : 'a t -> int

  (* plots *)
  val line_2d :
    points:float t ->
    ?style:Plot.Style.t ->
    ?legend:string ->
    ?with_points:bool ->
    ?error_bars:Plot.r2 Plot.Data.t ->
    unit ->
    Plot.r2 Plot.spec

  val hist :
    points:float t ->
    ?color:Plot.Color.t ->
    ?bins:int ->
    ?binwidth:float ->
    ?legend:string ->
    unit ->
    Plot.r2 Plot.spec
end
