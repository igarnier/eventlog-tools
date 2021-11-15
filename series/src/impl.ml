module Grid = Grid

type 'a elt = Float_elt : float elt | Complex_elt : Complex.t elt

let float_elt = Float_elt

let complex_elt = Complex_elt

type (_, _) representation =
  | Dense : (float, float array) representation
  | Fun_index : ('e, int -> 'e) representation
  | Fun_time : ('e, float -> 'e) representation

let dense_float = Dense

let fun_index = Fun_index

let fun_time = Fun_time

type ('g, 'elt, 'data) t =
  { grid : 'g Grid.t; data : 'data; repr : ('elt, 'data) representation }

(* -------------------------------------------------------------------------- *)
(* Getters *)

let grid : type g elt arr. (g, elt, arr) t -> g Grid.t = fun { grid; _ } -> grid

let data : type g elt arr. (g, elt, arr) t -> arr = fun { data; _ } -> data

let length : type g e a. (g, e, a) t -> int = fun { grid; _ } -> Grid.count grid

(* -------------------------------------------------------------------------- *)
(* Helpers *)

let index_to_time : type g e. g Grid.t -> (int -> e) -> float -> e =
 fun grid f (t : float) ->
  match Grid.index_from_real grid t with None -> assert false | Some x -> f x

let time_to_index : type g e. g Grid.t -> (float -> e) -> int -> e =
 fun grid f (i : int) ->
  match Grid.real_from_index grid i with None -> assert false | Some x -> f x

(* -------------------------------------------------------------------------- *)

let make : type g e a. (e, a) representation -> g Grid.t -> a -> (g, e, a) t =
 fun repr grid data ->
  match repr with
  | Dense ->
      if Grid.count grid <> Array.length data then
        invalid_arg "make: data and grid dimensions mismatch" ;
      { grid; data; repr }
  | Fun_index -> { grid; data; repr }
  | Fun_time -> { grid; data; repr }

let discretise :
    type e a g. (e, a) representation -> (float -> e) -> g Grid.t -> (g, e, a) t
    =
 fun repr f grid ->
  match repr with
  | Dense ->
      let data =
        Array.init (Grid.count grid) (fun i -> time_to_index grid f i)
      in
      { data; grid; repr }
  | Fun_index ->
      let data i = time_to_index grid f i in
      { data; grid; repr }
  | Fun_time -> { data = f; grid; repr }

let discretise_i :
    type e a g. (e, a) representation -> (int -> e) -> g Grid.t -> (g, e, a) t =
 fun repr f grid ->
  match repr with
  | Dense ->
      let data = Array.init (Grid.count grid) f in
      { data; grid; repr }
  | Fun_index -> { data = f; grid; repr }
  | Fun_time ->
      let data i = index_to_time grid f i in
      { data; grid; repr }

let eval_t : type g e a. (g, e, a) t -> float -> e =
 fun series time ->
  match series.repr with
  | Dense -> (
      match Grid.index_from_real (grid series) time with
      | None -> assert false
      | Some i -> (data series).(i))
  | Fun_index ->
      let f = index_to_time series.grid series.data in
      f time
  | Fun_time ->
      let f = data series in
      f time

let eval_i : type g e a. (g, e, a) t -> int -> e =
 fun series index ->
  match series.repr with
  | Dense -> (data series).(index)
  | Fun_index ->
      let f = series.data in
      f index
  | Fun_time ->
      let f = time_to_index series.grid series.data in
      f index

let map :
    type g e1 a1 e2 a2.
    (e2, a2) representation -> (e1 -> e2) -> (g, e1, a1) t -> (g, e2, a2) t =
 fun repr f series ->
  match series.repr with
  | Dense -> (
      let data = data series in
      let len = Array.length data in
      match repr with
      | Dense ->
          let res = Array.make len 0. in
          for i = 0 to len - 1 do
            Array.unsafe_set res i (f (Array.unsafe_get data i))
          done ;
          make repr (grid series) res
      | Fun_time ->
          let f = index_to_time (grid series) (fun i -> f data.(i)) in
          make repr (grid series) f
      | Fun_index -> make repr (grid series) (fun i -> f data.(i)))
  | Fun_time ->
      let s = data series in
      discretise repr (fun t -> f (s t)) (grid series)
  | Fun_index ->
      let s = data series in
      discretise_i repr (fun i -> f (s i)) (grid series)

(* let lift2 :
 *     type g e a. (a -> a -> a) -> (g, e, a) t -> (g, e, a) t -> (g, e, a) t =
 *   fun (type g e a) (f : a -> a -> a) (s1 : (g, e, a) t) (s2 : (g, e, a) t) ->
 *    if not (Grid.equal s1.grid s2.grid) then
 *      invalid_arg "lift2: grid dimensions mismatch"
 *    else
 *      let repr1 = s1.repr in
 *      let repr2 = s2.repr in
 *      match repr1 with
 *      | Dense -> (
 *          match repr2 with
 *          | Dense ->
 *              (\* we know there is only one dense data representation
 *                 per element type. *\)
 *              { grid = s1.grid; data = f s1.data s2.data; repr = s1.repr }
 *          | _ -> failwith "incompatible representations")
 *      | Fun_index -> (
 *          match repr2 with
 *          | Fun_index ->
 *              { grid = s1.grid; data = f s1.data s2.data; repr = s1.repr }
 *          | _ -> failwith "incompatible representations")
 *      | Fun_time -> (
 *          match repr2 with
 *          | Fun_time ->
 *              { grid = s1.grid; data = f s1.data s2.data; repr = s1.repr }
 *          | _ -> failwith "incompatible representations") *)
