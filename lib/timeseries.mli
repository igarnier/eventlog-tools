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
