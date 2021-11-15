type regular = Regular

type explicit = Explicit

let _ = Regular

let _ = Explicit

type 'a t =
  | Regular : { step : float; start : int; count : int } -> regular t
  (* [start, count] are measured in [step] *)
  | Explicit : { sampling_points : float array } -> explicit t

type search_outcome = Below | Inside of int | Above

type interval =
  | Real_interval of { start : float; stop : float }
  | Index_interval of { start : int; stop : int }

(* -------------------------------------------------------------------------- *)
(* Helpers *)

let index_from_real_array (arr : float array) (t : float) (start : int)
    (stop : int) : search_outcome =
  let rec bisect arr t start stop =
    let cell_count = stop - start in
    if cell_count = 1 then
      if t <. arr.(start) then Below
      else if t >=. arr.(stop) then Above
      else
        (* let () =
         *   Format.printf
         *     "inside: %f = arr.(%d) <= %f < arr.(%d) = %f@."
         *     arr.(start)
         *     start
         *     t
         *     stop
         *     arr.(stop)
         * in *)
        Inside start
    else
      let mid = start + (cell_count / 2) in
      if t <. arr.(mid) then bisect arr t start mid else bisect arr t mid stop
  in
  let len = stop - start in
  if len <= 0 then invalid_arg "index_from_real_array"
  else bisect arr t start stop

let farray_compare (a1 : float array) (a2 : float array) =
  let l1 = Array.length a1 in
  let l2 = Array.length a2 in
  let c = Int.compare l1 l2 in
  if c <> 0 then c
  else
    let exception Break of int in
    try
      for i = 0 to l1 - 1 do
        let x1 = Array.unsafe_get a1 i in
        let x2 = Array.unsafe_get a2 i in
        let c = Float.compare x1 x2 in
        if c = 0 then () else raise (Break c)
      done ;
      0
    with Break c -> c

let check_increasing (a : float array) =
  let l = Array.length a in
  let exception Invalid in
  try
    for i = 1 to l - 1 do
      let x = Array.unsafe_get a (i - 1) in
      let y = Array.unsafe_get a i in
      if x >=. y then raise Invalid else ()
    done ;
    true
  with Invalid -> false

(* -------------------------------------------------------------------------- *)
(* Printers *)

let pp_arr fmtr farray =
  let l = Array.to_list farray in
  let l = List.map (fun f -> Format.asprintf "%f" f) l in
  let len = List.length l in
  let l =
    if len > 6 then
      let heads = List.filteri (fun i _ -> i < 3) l in
      let tails = List.filteri (fun i _ -> i < 3) (List.rev l) in
      heads @ ["..."] @ List.rev tails
    else l
  in
  Format.fprintf
    fmtr
    "%a (len = %d)"
    (Format.pp_print_list
       ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ", ")
       Format.pp_print_string)
    l
    len

let pp : type a. Format.formatter -> a t -> unit =
 fun fmtr grid ->
  match grid with
  | Regular { step; start; count } ->
      Format.fprintf
        fmtr
        "{ step = %f, start = %d, count = %d }"
        step
        start
        count
  | Explicit { sampling_points } ->
      Format.fprintf fmtr "{ %a }" pp_arr sampling_points

let pp_interval fmtr (intv : interval) =
  match intv with
  | Real_interval { start; stop } -> Format.fprintf fmtr "f[%f, %f]" start stop
  | Index_interval { start; stop } -> Format.fprintf fmtr "i[%d, %d]" start stop

(* -------------------------------------------------------------------------- *)

let equal : type a. a t -> a t -> bool =
 fun g1 g2 ->
  match (g1, g2) with
  | ( Regular { step; start; count },
      Regular { step = step'; start = start'; count = count' } ) ->
      step =. step' && start = start' && count = count'
  | ( Explicit { sampling_points },
      Explicit { sampling_points = sampling_points' } ) ->
      farray_compare sampling_points sampling_points' = 0

let regular ~start ~step ~count =
  if step <=. 0.0 then invalid_arg "regular (step <= 0.0)" ;
  if count <= 0 then invalid_arg "regular (count <= 0)" ;
  Regular { start; step; count }

let sampling ~freq ~start ~stop =
  if start >=. stop then invalid_arg "sampling (start >= stop)" ;
  if freq <=. 0.0 then invalid_arg "sampling (freq <= 0.0)" ;
  let duration = stop -. start in
  let step = 1.0 /. freq in
  if duration <. step then invalid_arg "sampling: duration < sampling period" ;
  let count = truncate (duration *. freq) in
  let start = truncate (start *. freq) in
  Regular { start; step; count }

let explicit ~sampling_points =
  if Array.length sampling_points <= 1 then invalid_arg "explicit: empty input" ;
  if not (check_increasing sampling_points) then
    invalid_arg "explicit: input not strictly increasing" ;
  Explicit { sampling_points }

let to_explicit (Regular { start; step; count }) =
  let sampling_points =
    Array.init (count + 1) (fun i -> float (start + i) *. step)
  in
  explicit ~sampling_points

let max (i : int) (j : int) = if i < j then j else i

let index_from_real (type a) (grid : a t) (t : float) : int option =
  match grid with
  | Regular { start; step; count } ->
      let t0 = float_of_int start *. step in
      let tmax = t0 +. (float_of_int count *. step) in
      if t <. t0 || t >=. tmax then None
      else
        (* precision issues can yield i = -1 *)
        let i = max 0 (int_of_float (t /. step) - start) in
        (* let i = int_of_float ((t -. t0) /. step) in *)
        (* let () =
         *   Format.printf
         *     "index_from_real:@.  t = %f@.  start = %d@.  step = %f@.  count = \
         *      %d =>@.  t0 = %f,@.  tmax = %f,@.  i = %d,@.  v = %f@.@."
         *     t
         *     start
         *     step
         *     count
         *     t0
         *     tmax
         *     i
         *     (float_of_int (start + i) *. step)
         * in *)
        Some i
  | Explicit { sampling_points } -> (
      if Array.length sampling_points = 0 then None
      else
        match
          index_from_real_array
            sampling_points
            t
            0
            (Array.length sampling_points - 1)
        with
        | Above | Below -> None
        | Inside i ->
            (* let () =
             *   Format.printf
             *     "index_from_real: cell = %d (value at cell = %f)@."
             *     i
             *     sampling_points.(i)
             * in *)
            Some i)

let real_from_index (type a) (grid : a t) (i : int) : float option =
  match grid with
  | Regular { start; step; count } ->
      if i < 0 || i >= count then None
      else Some (step *. float_of_int (start + i))
  | Explicit { sampling_points } ->
      (* -1 because the last element is not part of the grid's domain *)
      if i < 0 || i >= Array.length sampling_points - 1 then None
      else Some sampling_points.(i)

let start (x : regular t) = match x with Regular { start; _ } -> start

let step (x : regular t) = match x with Regular { step; _ } -> step

let count : type a. a t -> int =
 fun grid ->
  match grid with
  | Regular { count; _ } -> count
  | Explicit { sampling_points } -> Array.length sampling_points - 1

let t0 : type a. a t -> float =
 fun grid ->
  match grid with
  | Regular { step; start; _ } -> step *. float_of_int start
  | Explicit { sampling_points } -> sampling_points.(0)

let extent : type a. a t -> float =
 fun grid ->
  match grid with
  | Regular { step; count; _ } -> float count *. step
  | Explicit { sampling_points } ->
      let last = Array.length sampling_points - 1 in
      sampling_points.(last) -. sampling_points.(0)

let t_last : type a. a t -> float = fun grid -> t0 grid +. extent grid

let fold_regular f init (g : regular t) =
  match g with
  | Regular { step; start; count } ->
      let stop = start + count - 1 in
      let rec loop acc i =
        if i > stop then acc
        else
          let t = float i *. step in
          let acc = f acc t in
          loop acc (i + 1)
      in
      loop init start

let fold_explicit f init (g : explicit t) =
  match g with
  | Explicit { sampling_points } ->
      let stop = Array.length sampling_points - 2 in
      let rec loop acc i =
        if i > stop then acc
        else
          let t = sampling_points.(i) in
          let acc = f acc t in
          loop acc (i + 1)
      in
      loop init 0

let fold (type g) f init (grid : g t) =
  match grid with
  | Regular _ -> fold_regular f init grid
  | Explicit _ -> fold_explicit f init grid

let fold_increments_regular f init (grid : regular t) =
  match grid with
  | Regular { step; start; count } ->
      let stop = start + count - 2 in
      let rec loop i acc =
        if i > stop then acc
        else
          let acc = f acc step in
          loop (i + 1) acc
      in
      loop start init

let fold_increments_explicit f init (grid : explicit t) =
  match grid with
  | Explicit { sampling_points } ->
      let stop = Array.length sampling_points - 2 in
      let rec loop i acc =
        if i > stop then acc
        else
          let dt = sampling_points.(i) -. sampling_points.(i - 1) in
          let acc = f acc dt in
          loop (i + 1) acc
      in
      loop 1 init

let fold_increments (type g) f init (grid : g t) =
  match grid with
  | Regular _ -> fold_increments_regular f init grid
  | Explicit _ -> fold_increments_explicit f init grid

let real_interval start stop =
  if stop <=. start then
    invalid_arg (Printf.sprintf "real_interval: [%f, %f)" start stop) ;
  Real_interval { start; stop }

let index_interval start stop =
  if stop <= start then invalid_arg "index_interval" ;
  Index_interval { start; stop }

let ( >>= ) = Option.bind

let inter_regular (grid : regular t) (itv : interval) : regular t option =
  match grid with
  | Regular { step; start = grid_start; count } -> (
      match itv with
      | Real_interval { start; stop } ->
          index_from_real grid start >>= fun start_index ->
          index_from_real grid stop >>= fun stop_index ->
          let new_count = stop_index - start_index in

          (* let () =
           *   (\* if stop =. sampling_points.(stop_index) then *\)
           *   Format.printf
           *     "inter_regular: itv = %a, start_index = %d, stop_index = %d, \
           *      new_count = %d, old_count = %d@."
           *     pp_interval
           *     itv
           *     start_index
           *     stop_index
           *     new_count
           *     count
           * in *)
          if new_count = 0 then None
          else
            Some
              (regular ~start:(grid_start + start_index) ~step ~count:new_count)
      | Index_interval { start; stop } ->
          let new_count = stop - start + 1 in
          if new_count >= count || new_count < 0 then None
          else Some (Regular { step; start; count }))

let inter_explicit (grid : explicit t) (itv : interval) : explicit t option =
  match grid with
  | Explicit { sampling_points } -> (
      let count = Array.length sampling_points in
      match itv with
      | Real_interval { start; stop } ->
          index_from_real grid start >>= fun start_index ->
          index_from_real grid stop >>= fun stop_index ->
          let new_count = stop_index - start_index + 1 in
          (* let () =
           *   (\* if stop =. sampling_points.(stop_index) then *\)
           *   Format.printf
           *     "inter_explicit: itv = %a, start_index = %d, stop_index = %d, \
           *      new_count = %d@."
           *     pp_interval
           *     itv
           *     start_index
           *     stop_index
           *     new_count
           * in *)
          if new_count <= 1 then None
          else
            let sampling_points =
              Array.sub sampling_points start_index new_count
            in
            Some (Explicit { sampling_points })
      | Index_interval { start; stop } ->
          let new_count = stop - start + 1 in
          if new_count >= count || new_count < 0 then None
          else
            let sampling_points = Array.sub sampling_points start new_count in
            Some (Explicit { sampling_points }))

let inter (type g) (grid : g t) (itv : interval) : g t option =
  match grid with
  | Regular _ -> inter_regular grid itv
  | Explicit _ -> inter_explicit grid itv
