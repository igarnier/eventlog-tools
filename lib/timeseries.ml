type 'a t = (float * 'a) list

let of_raw x = List.sort (fun (t, _) (t', _) -> Float.compare t t') x

let rec last : 'a list -> 'a = function
  | [] -> assert false
  | [elt] -> elt
  | _ :: tl -> last tl

let duration l =
  let t0 = fst (List.hd l) in
  let tlast = fst (last l) in
  tlast -. t0

let cumulative l =
  let v_acc = ref (snd (List.hd l)) in
  List.rev
  @@ List.fold_left
       (fun acc (t, v') ->
         let v = !v_acc +. v' in
         v_acc := !v_acc +. v' ;
         (t, v) :: acc)
       []
       l

let bin ~timeslice l =
  let t0 = fst (List.hd l) in
  let tlast = fst (last l) in
  let dt = tlast -. t0 in
  assert (timeslice < dt) ;
  let bins = int_of_float (ceil (dt /. timeslice)) in
  Array.init bins (fun i ->
      let t0_itv = t0 +. (float_of_int i *. timeslice) in
      let t1_itv = t0 +. (float_of_int (i + 1) *. timeslice) in
      let data = List.filter (fun (t, _) -> t0_itv <= t && t < t1_itv) l in
      data)

let sum l = List.fold_left (fun acc (_, x) -> acc +. x) 0.0 l

let length = List.length

let line_2d ~points ?style ?legend ?with_points ?error_bars () =
  let points =
    Plot.Data.of_list (List.map (fun (t, v) -> Plot.r2 t v) points)
  in
  Plot.Line.line_2d ~points ?style ?legend ?with_points ?error_bars ()

let hist ~points ?color ?bins ?binwidth ?legend () =
  let points = Plot.Data.of_list (List.map (fun (_t, v) -> Plot.r1 v) points) in
  Plot.Histogram.hist ~points ?color ?bins ?binwidth ?legend ()
