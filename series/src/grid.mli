(** A 1d sampling grid is either regular (the most common case)
    or explicitly specified by an array of sampling times. *)
type regular = private Regular

type explicit = private Explicit

type 'a t = private
  | Regular :
      { step : float;  (** Width of a grid cell (always strictly positive). *)
        start : int;  (** Position of the first cell, measured in [step]. *)
        count : int  (** Number of grid points. *)
      }
      -> regular t
  | Explicit : { sampling_points : float array } -> explicit t

(** The intervals are understood to be inclusive. *)
type interval = private
  | Real_interval of { start : float; stop : float }
  | Index_interval of { start : int; stop : int }

(** Grid pretty-printer *)
val pp : Format.formatter -> 'a t -> unit

(** Interval pretty-printer *)
val pp_interval : Format.formatter -> interval -> unit

(** [equal g1 g2] returns true iff two grids are equal. Note that only
    grids of the same kind can be compared. *)
val equal : 'a t -> 'a t -> bool

(** [regular ~start ~step ~count] creates a one-dimensional regular grid, with
    cells half-open intervals [\[n * step; (n + 1) * step) ] for [n]
    ranging from [start] to [start+count].
    The first cell starts at position [start * step]. The grid has [count] cells.

    @raises Invalid_arg if [step <= 0.0] or [count <= 0]. *)
val regular : start:int -> step:float -> count:int -> regular t

(** [sampling ~freq ~start ~stop] returns a [Regular] grid obtained
    by discretizing the interval [\[start; stop)] starting
    at [start], with half-open intervals of width [1./.freq].

    {e Only cells included in the interval [\[start; stop)] are part of the grid.}

    Note in particular that the [start] is always in the domain of the grid while
    [stop] is never in the domain of the grid.

    @raises Invalid_arg if [start > stop], [freq < 0.] or if [stop -. start < 1. /. freq] *)
val sampling : freq:float -> start:float -> stop:float -> regular t

(** [explicit ~sampling_points] returns an explicit grid with the
    given [sampling_points]. The grid's domain is the half-open interval
    [\[sampling_points.(0), sampling_points.(last))].

    @raises Invalid_arg if [sampling_points] is not an array of increasing floats, or if [Array.length sampling_points <= 1]. *)
val explicit : sampling_points:float array -> explicit t

(** [to_explicit] converts a regular grid to an explicit one. *)
val to_explicit : regular t -> explicit t

(** [index_from_real grid t] discretizes [t] according to [grid],
    ie it returns the cell index corresponding to the value [t].
    If [t] is out of the grid domain, returns [None]. *)
val index_from_real : 'a t -> float -> int option

(** [real_from_index grid i] returns the starting position of the cell
    with index [i]. If [i] is out of the grid domain, returns [None]. *)
val real_from_index : 'a t -> int -> float option

val start : regular t -> int

val step : regular t -> float

val count : 'a t -> int

val t0 : 'a t -> float

val t_last : 'a t -> float

val extent : 'a t -> float

val fold : ('acc -> float -> 'acc) -> 'acc -> 'a t -> 'acc

val fold_increments : ('acc -> float -> 'acc) -> 'acc -> 'a t -> 'acc

val real_interval : float -> float -> interval

val index_interval : int -> int -> interval

(** Intersects a grid with an interval. *)
val inter : 'a t -> interval -> 'a t option
