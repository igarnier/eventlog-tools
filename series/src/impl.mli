(* -------------------------------------------------------------------------- *)

(** Type of series discretized over a grid of type 'grid and with elements
    of type 'elt. 'data is the type of the underlying representation. *)
type ('grid, 'elt, 'data) t

(** Type of admissible elements for the series. *)
type 'a elt

(** Admissible elements. *)
val float_elt : float elt

val complex_elt : Complex.t elt

(** Allowed underlying representation for the time series data. *)
type ('elt, 'data) representation

val dense_float : (float, float array) representation

val fun_index : ('e, int -> 'e) representation

val fun_time : ('e, float -> 'e) representation

(** Get grid *)
val grid : ('g, 'e, 'a) t -> 'g Grid.t

(** Get data *)
val data : ('g, 'e, 'a) t -> 'a

(** Get length of data *)
val length : ('g, 'e, 'a) t -> int

(** Make signal from data and grid *)
val make : ('e, 'a) representation -> 'g Grid.t -> 'a -> ('g, 'e, 'a) t

(** Discretise a function over a regular grid. *)
val discretise :
  ('e, 'a) representation -> (float -> 'e) -> 'g Grid.t -> ('g, 'e, 'a) t

(** Evaluate time series at given time. This is logarithmic in the
    length of the data in case of a dense representation. *)
val eval_t : ('g, 'e, 'a) t -> float -> 'e

(** Evaluate time series at given index. This is a constant-time operation. *)
val eval_i : ('g, 'e, 'a) t -> int -> 'e

(** Map a function over a series. *)
val map :
  ('e2, 'a2) representation ->
  ('e1 -> 'e2) ->
  ('g, 'e1, 'a1) t ->
  ('g, 'e2, 'a2) t

(** Lifts a binary operator on data representations to series.
    Fails if the grids are different. *)
(* val lift2 :
 *   ('a -> 'a -> 'a) -> ('g, 'e, 'a) t -> ('g, 'e, 'a) t -> ('g, 'e, 'a) t *)

(* (\* Computes the series of increments of a series.
 *    The series of increment has one element less and has
 *    the same grid has the original series, minus the last
 *    element. The element at index [i] of the series of
 *    increments corresponds to the difference of the elements
 *    of indices [i+1] and [i] of the original series. *\)
 * val increments : ('g, float, 'a) t -> ('g, float, 'a) t *)

(* TODO
   - average
   - variance
   - filter, filter_map
   - sliding window fold & map
   - sample classical stoch proc (brownian, other SDEs?)
     - efficient sampling vector of gaussians
   - other data : R^n, C^n, metric spaces?
   - tagless final approach to not force usage of bigarrays
     (axiomatize arrays, etc, parallel impls, llvm codegen...)
*)
