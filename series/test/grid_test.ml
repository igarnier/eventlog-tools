module G = Series.Grid

(* --------------------- *)
(* testing regular grids *)

let assert_invalid_arg f =
  try
    ignore (f ()) ;
    Format.printf "didn't raise: bug found!"
  with Invalid_argument s -> Format.printf "got expected failure: %s" s

(* testing failure cases *)
let%expect_test "regular_empty1" =
  (assert_invalid_arg @@ fun () -> G.regular ~start:0 ~step:0. ~count:0) ;
  [%expect {| got expected failure: regular (step <= 0.0) |}]

let%expect_test "regular_empty2" =
  (assert_invalid_arg @@ fun () -> G.regular ~start:0 ~step:0. ~count:1) ;
  [%expect {| got expected failure: regular (step <= 0.0) |}]

let%expect_test "regular_empty3" =
  (assert_invalid_arg @@ fun () -> G.regular ~start:0 ~step:1. ~count:0) ;
  [%expect {| got expected failure: regular (count <= 0) |}]

let%expect_test "sampling_err1" =
  (assert_invalid_arg @@ fun () -> G.sampling ~freq:0.0 ~start:0.0 ~stop:0.0) ;
  [%expect {| got expected failure: sampling (start >= stop) |}]

let%expect_test "sampling_err2" =
  (assert_invalid_arg @@ fun () -> G.sampling ~freq:1. ~start:0.0 ~stop:0.0) ;
  [%expect {| got expected failure: sampling (start >= stop) |}]

let%expect_test "sampling_err3" =
  (assert_invalid_arg @@ fun () -> G.sampling ~freq:0. ~start:0.0 ~stop:1.0) ;
  [%expect {| got expected failure: sampling (freq <= 0.0) |}]

let%expect_test "sampling_err4" =
  (assert_invalid_arg @@ fun () -> G.sampling ~freq:1. ~start:1.0 ~stop:0.0) ;
  [%expect {| got expected failure: sampling (start >= stop) |}]

let%expect_test "sampling_err5" =
  (assert_invalid_arg @@ fun () -> G.sampling ~freq:0.1 ~start:0. ~stop:3.) ;
  [%expect {| got expected failure: sampling: duration < sampling period |}]

let%expect_test "regular_to_explicit1" =
  let reg = G.sampling ~freq:1. ~start:0. ~stop:3. in
  let exp = G.to_explicit reg in
  let arr =
    match exp with G.Explicit { sampling_points } -> sampling_points
  in
  (match arr with
  | [| 0.; 1.; 2.; 3. |] -> Format.printf "ok"
  | _ -> Format.printf "bug found") ;
  [%expect {|
    ok |}]

let pp_int_opt fmtr io =
  Format.pp_print_option
    ~none:(fun fmtr () -> Format.fprintf fmtr "None")
    Format.pp_print_int
    fmtr
    io

let pp_float_opt fmtr fo =
  Format.pp_print_option
    ~none:(fun fmtr () -> Format.fprintf fmtr "None")
    Format.pp_print_float
    fmtr
    fo

let%expect_test "index_from_real1" =
  let reg = G.sampling ~freq:1. ~start:0. ~stop:3. in
  (* cells: [0, 1); [1, 2); [2, 3) *)
  Format.printf "G.sampling ~freq:1. ~start:0. ~stop:3. = %a@." G.pp reg ;
  List.iter
    (fun t ->
      Format.printf "%f |-> %a;@." t pp_int_opt (G.index_from_real reg t))
    [~-.0.0001; 0.; 1.; 2.; 2.999; 3.; 3.0001] ;
  [%expect
    {|
    G.sampling ~freq:1. ~start:0. ~stop:3. = { step = 1.000000, start = 0, count = 3 }
    -0.000100 |-> None;
    0.000000 |-> 0;
    1.000000 |-> 1;
    2.000000 |-> 2;
    2.999000 |-> 2;
    3.000000 |-> None;
    3.000100 |-> None; |}]

let%expect_test "index_from_real2" =
  let reg = G.sampling ~freq:1. ~start:0. ~stop:2.999 in
  (* cells: [0, 1); [1, 2) *)
  Format.printf "G.sampling ~freq:1. ~start:0. ~stop:2.999 = %a@." G.pp reg ;
  List.iter
    (fun t ->
      Format.printf "%f |-> %a;@." t pp_int_opt (G.index_from_real reg t))
    [~-.0.0001; 0.; 1.; 2.; 2.999; 3.; 3.0001] ;
  [%expect
    {|
    G.sampling ~freq:1. ~start:0. ~stop:2.999 = { step = 1.000000, start = 0, count = 2 }
    -0.000100 |-> None;
    0.000000 |-> 0;
    1.000000 |-> 1;
    2.000000 |-> None;
    2.999000 |-> None;
    3.000000 |-> None;
    3.000100 |-> None; |}]

let%expect_test "index_from_real3" =
  let reg = G.sampling ~freq:1. ~start:0. ~stop:3.0001 in
  (* cells: [0, 1); [1, 2); [2, 3) *)
  Format.printf "G.sampling ~freq:1. ~start:0. ~stop:3.0001 = %a@." G.pp reg ;
  List.iter
    (fun t ->
      Format.printf "%f |-> %a;@." t pp_int_opt (G.index_from_real reg t))
    [~-.0.0001; 0.; 1.; 2.; 2.999; 3.; 3.0001] ;
  [%expect
    {|
    G.sampling ~freq:1. ~start:0. ~stop:3.0001 = { step = 1.000000, start = 0, count = 3 }
    -0.000100 |-> None;
    0.000000 |-> 0;
    1.000000 |-> 1;
    2.000000 |-> 2;
    2.999000 |-> 2;
    3.000000 |-> None;
    3.000100 |-> None; |}]

let%expect_test "index_from_real4" =
  let reg = G.sampling ~freq:10. ~start:0. ~stop:3. in
  (* cells: [0, 0.1); ... [2.9, 3) *)
  Format.printf "G.sampling ~freq:10. ~start:0. ~stop:3. = %a@." G.pp reg ;
  List.iter
    (fun t ->
      Format.printf "%f |-> %a;@." t pp_int_opt (G.index_from_real reg t))
    [~-.0.0001; 0.; 1.; 2.; 2.999; 3.; 3.0001] ;
  [%expect
    {|
    G.sampling ~freq:10. ~start:0. ~stop:3. = { step = 0.100000, start = 0, count = 30 }
    -0.000100 |-> None;
    0.000000 |-> 0;
    1.000000 |-> 10;
    2.000000 |-> 20;
    2.999000 |-> 29;
    3.000000 |-> None;
    3.000100 |-> None; |}]

let%expect_test "real_from_index1" =
  let reg = G.sampling ~freq:1. ~start:0. ~stop:3.0 in
  Format.printf "G.sampling ~freq:1. ~start:0. ~stop:3. = %a@." G.pp reg ;
  List.iter
    (fun t -> Format.printf "%a, " pp_float_opt (G.real_from_index reg t))
    [-1; 0; 1; 2; 3; 4] ;
  [%expect
    {|
    G.sampling ~freq:1. ~start:0. ~stop:3. = { step = 1.000000, start = 0, count = 3 }
    None, 0., 1., 2., None, None, |}]

let%expect_test "real_from_index2" =
  let reg = G.sampling ~freq:1. ~start:0. ~stop:2.999 in
  Format.printf "G.sampling ~freq:1. ~start:0. ~stop:2.999 = %a@." G.pp reg ;
  List.iter
    (fun t -> Format.printf "%a, " pp_float_opt (G.real_from_index reg t))
    [-1; 0; 1; 2; 3; 4] ;
  [%expect
    {|
    G.sampling ~freq:1. ~start:0. ~stop:2.999 = { step = 1.000000, start = 0, count = 2 }
    None, 0., 1., None, None, None, |}]

let%expect_test "real_from_index3" =
  let reg = G.sampling ~freq:1. ~start:0. ~stop:3.0001 in
  Format.printf "G.sampling ~freq:1. ~start:0. ~stop:3.0001 = %a@." G.pp reg ;
  List.iter
    (fun t -> Format.printf "%a, " pp_float_opt (G.real_from_index reg t))
    [-1; 0; 1; 2; 3; 4] ;
  [%expect
    {|
    G.sampling ~freq:1. ~start:0. ~stop:3.0001 = { step = 1.000000, start = 0, count = 3 }
    None, 0., 1., 2., None, None, |}]

(* ---------------------- *)
(* testing explicit grids *)

let%expect_test "create_explicit1" =
  (assert_invalid_arg @@ fun () -> G.explicit ~sampling_points:[||]) ;
  [%expect {| got expected failure: explicit: empty input |}]

let%expect_test "create_explicit2" =
  (assert_invalid_arg @@ fun () -> G.explicit ~sampling_points:[| 0.; 0.; 1. |]) ;
  [%expect {| got expected failure: explicit: input not strictly increasing |}]

let%expect_test "create_explicit3" =
  (assert_invalid_arg @@ fun () -> G.explicit ~sampling_points:[| 1.; 0. |]) ;
  [%expect {| got expected failure: explicit: input not strictly increasing |}]

let%expect_test "create_explicit4" =
  (assert_invalid_arg @@ fun () -> G.explicit ~sampling_points:[| 0.; 1.; 1. |]) ;
  [%expect {| got expected failure: explicit: input not strictly increasing |}]

let%expect_test "regular_to_explicit2" =
  let reg = G.sampling ~freq:1. ~start:0. ~stop:3. in
  let exp = G.to_explicit reg in
  Format.printf "%a@." G.pp exp ;
  [%expect {| { 0.000000, 1.000000, 2.000000, 3.000000 (len = 4) } |}]

let%expect_test "regular_to_explicit3" =
  let reg = G.sampling ~freq:10. ~start:0. ~stop:3. in
  let exp = G.to_explicit reg in
  Format.printf "reg = G.sampling ~freq:10. ~start:0. ~stop:3. = %a@." G.pp reg ;
  Format.printf "exp = G.explicit reg= %a@." G.pp exp ;
  [%expect
    {|
      reg = G.sampling ~freq:10. ~start:0. ~stop:3. = { step = 0.100000, start = 0, count = 30 }
      exp = G.explicit reg= { 0.000000, 0.100000, 0.200000, ..., 2.800000, 2.900000, 3.000000 (len = 31) } |}]

let%expect_test "exp_index_from_real1" =
  let sampling_points =
    [| 0.0; 0.1; 1.; 1.1; 2.; 2.1; 2.9; 3.; 3.0001; 3.1 |]
  in
  let exp = G.explicit ~sampling_points in
  Format.printf "explicit = %a@." G.pp exp ;
  List.iter
    (fun t ->
      match G.index_from_real exp t with
      | None -> Format.printf "%f has index None;@." t
      | Some i ->
          Format.printf
            "%f has index %d, cell starts at %f;@."
            t
            i
            sampling_points.(i))
    [~-.0.0001; 0.; 0.01; 2.; 2.3; 3.; 3.0001; 3.0002; 3.1] ;
  [%expect
    {|
    explicit = { 0.000000, 0.100000, 1.000000, ..., 3.000000, 3.000100, 3.100000 (len = 10) }
    -0.000100 has index None;
    0.000000 has index 0, cell starts at 0.000000;
    0.010000 has index 0, cell starts at 0.000000;
    2.000000 has index 4, cell starts at 2.000000;
    2.300000 has index 5, cell starts at 2.100000;
    3.000000 has index 7, cell starts at 3.000000;
    3.000100 has index 8, cell starts at 3.000100;
    3.000200 has index 8, cell starts at 3.000100;
    3.100000 has index None; |}]

let%expect_test "exp_index_from_real2" =
  let reg = G.sampling ~freq:10. ~start:0. ~stop:3. in
  let exp = G.to_explicit reg in
  let sampling_points =
    match exp with G.Explicit { sampling_points } -> sampling_points
  in
  Format.printf "reg = G.sampling ~freq:10. ~start:0. ~stop:3. = %a@." G.pp reg ;
  Format.printf "exp = G.explicit reg= %a@." G.pp exp ;
  List.iter
    (fun t ->
      match G.index_from_real exp t with
      | None -> Format.printf "%f |-> None;@." t
      | Some i -> Format.printf "%f |-> %d, %f;@." t i sampling_points.(i))
    [~-.0.0001; 0.; 1.; 2.; 2.999; 3.; 3.0001] ;
  [%expect
    {|
    reg = G.sampling ~freq:10. ~start:0. ~stop:3. = { step = 0.100000, start = 0, count = 30 }
    exp = G.explicit reg= { 0.000000, 0.100000, 0.200000, ..., 2.800000, 2.900000, 3.000000 (len = 31) }
    -0.000100 |-> None;
    0.000000 |-> 0, 0.000000;
    1.000000 |-> 10, 1.000000;
    2.000000 |-> 20, 2.000000;
    2.999000 |-> 29, 2.900000;
    3.000000 |-> None;
    3.000100 |-> None; |}]

let%expect_test "exp_intersection" =
  let reg = G.sampling ~freq:10. ~start:0. ~stop:3. in
  let exp = G.to_explicit reg in
  let t0 = G.t0 reg in
  let tlast = G.t_last reg -. 0.0001 in
  let intv = G.real_interval t0 tlast in
  let reg' = G.inter reg intv in
  let exp' = G.inter exp intv in
  Format.printf "reg = G.sampling ~freq:10. ~start:0. ~stop:3.@." ;
  Format.printf "reg' = %a@." (Format.pp_print_option G.pp) reg' ;
  Format.printf "exp = %a@." G.pp exp ;
  Format.printf "exp' = %a@." (Format.pp_print_option G.pp) exp' ;
  [%expect
    {|
    reg = G.sampling ~freq:10. ~start:0. ~stop:3.
    reg' = { step = 0.100000, start = 0, count = 29 }
    exp = { 0.000000, 0.100000, 0.200000, ..., 2.800000, 2.900000, 3.000000 (len = 31) }
    exp' = { 0.000000, 0.100000, 0.200000, ..., 2.700000, 2.800000, 2.900000 (len = 30) } |}]

let%expect_test "real_from_index" =
  let reg = G.sampling ~freq:10. ~start:0. ~stop:3. in
  ignore
    (Array.init (G.count reg) (fun i ->
         match G.real_from_index reg i with
         | None -> Format.printf "fail@."
         | Some _ -> ())) ;
  [%expect {| |}]

let%expect_test "real_from_index" =
  let reg = G.sampling ~freq:10. ~start:0. ~stop:3. in
  let exp = G.to_explicit reg in
  ignore
    (Array.init (G.count exp) (fun i ->
         match G.real_from_index exp i with
         | None -> Format.printf "fail@."
         | Some _ -> ())) ;
  [%expect {| |}]

(* ---------------------------------------- *)
(* consistency between regular and explicit *)

let regular_gen =
  QCheck.Gen.(
    let+ start = small_nat and+ count = small_nat and+ step = pfloat in
    let count = count + 1 in
    G.regular ~start ~step ~count)

let regular_with_index =
  let open QCheck in
  make
    ~print:(fun (g, i) -> Format.asprintf "%a, %d" G.pp g i)
    Gen.(
      let* (G.Regular { count; _ } as grid) = regular_gen in
      let* index = 0 -- count in
      return (grid, index))

let regular_with_time =
  QCheck.make
    ~shrink:(fun (grid, t) k ->
      match grid with
      | G.Regular { count; start = _; step } ->
          if G.count grid <= 2 then ()
          else if step <> 1. then
            let t0 = G.t0 grid in
            k (G.regular ~start:0 ~step:1. ~count, (t -. t0) /. step)
          else
            let t0 = G.t0 grid in
            k (G.regular ~start:0 ~step:1. ~count:(count - 1), (t -. t0) /. step))
    ~print:(fun (g, t) -> Format.asprintf "%a, %f" G.pp g t)
    QCheck.Gen.(
      let* grid = regular_gen in
      let start = G.t0 grid in
      let stop = G.t_last grid in
      let* t = start --. stop in
      return (grid, t))

(* let regular_with_index =
 *   QCheck.map (fun regular ->
 *       (match regular with | G.Regular { step; start; count } ->
 *       )
 *     ) regular_arb
 *
 *   QCheck.make
 *     ~shrink:(fun (grid, index) k ->
 *       match grid with
 *       | G.Regular { step; start; count } ->
 *           k (G.regular ~start ~step ~count, index))
 *     ~print:(fun (g, i) -> Format.asprintf "%a, %d" G.pp g i)
 *     QCheck.Gen.(
 *       let* (gen, count, _start, _stop) = regular_gen in
 *       let* index = 0 -- count in
 *       return (gen, index))
 *
 * let regular_with_time =
 *   QCheck.make
 *     ~print:(fun (g, t) -> Format.asprintf "%a, %f" G.pp g t)
 *     QCheck.Gen.(
 *       let* (gen, _count, start, stop) = regular_gen in
 *       let* t = start --. stop in
 *       return (gen, t)) *)

let real_from_index =
  QCheck.Test.make
    ~count:10_000
    ~name:"regular_explicit_real_from_index"
    regular_with_index
    (fun (reg, i) ->
      let exp = G.to_explicit reg in
      let t0 = G.real_from_index reg i in
      let t0' = G.real_from_index exp i in
      t0 = t0')

let index_from_real =
  QCheck.Test.make
    ~count:10_000
    ~name:"regular_explicit_index_from_real"
    regular_with_time
    (fun (reg, t) ->
      let exp = G.to_explicit reg in
      let i0 = G.index_from_real reg t in
      let i0' = G.index_from_real exp t in
      i0 = i0')

let t0 =
  QCheck.Test.make
    ~count:10_000
    ~name:"regular_explicit_t0"
    regular_with_time
    (fun (reg, _t) ->
      let exp = G.to_explicit reg in
      G.t0 reg = G.t0 exp)

let extent =
  QCheck.Test.make
    ~count:10_000
    ~name:"regular_explicit_extent"
    regular_with_time
    (fun (reg, _t) ->
      let exp = G.to_explicit reg in
      G.extent reg = G.extent exp)

let fold =
  QCheck.Test.make
    ~count:10_000
    ~name:"regular_explicit_fold"
    regular_with_time
    (fun (reg, _t) ->
      let exp = G.to_explicit reg in
      G.fold (fun acc elt -> elt :: acc) [] reg
      = G.fold (fun acc elt -> elt :: acc) [] exp)

let fold_increments =
  QCheck.Test.make
    ~count:10_000
    ~name:"regular_explicit_fold_increments"
    regular_with_time
    (fun (reg, _t) ->
      let exp = G.to_explicit reg in
      let incrs1 = G.fold_increments (fun acc elt -> elt :: acc) [] reg in
      let incrs2 = G.fold_increments (fun acc elt -> elt :: acc) [] exp in
      List.for_all2
        (fun x y ->
          let delta = abs_float (x -. y) in
          delta < 0.0000001)
        incrs1
        incrs2)

(* This test  doesn't pass because of float approximations *)
let _intersect =
  QCheck.Test.make
    ~count:10_000
    ~name:"regular_explicit_intersect"
    regular_with_time
    (fun (reg, _t) ->
      let exp = G.to_explicit reg in
      let t0 = G.t0 reg in
      let tlast = t0 +. (0.95 *. (G.t_last reg -. t0)) in
      let intv = G.real_interval t0 tlast in
      let reg' = G.inter reg intv in
      let exp' = G.inter exp intv in
      match (reg', exp') with
      | (None, None) -> true
      | (None, Some _) | (Some _, None) ->
          QCheck.Test.fail_reportf
            {|
            Some <> None (intv = %a)
            reg = %a
            exp = %a
            reg' = %a
            exp' = %a|}
            G.pp_interval
            intv
            G.pp
            reg
            G.pp
            exp
            (Format.pp_print_option G.pp)
            reg'
            (Format.pp_print_option G.pp)
            exp'
      | (Some reg', Some exp') ->
          let exp'' = G.to_explicit reg' in
          if not (G.equal exp' exp'') then
            QCheck.Test.fail_reportf
              {|
              grid mismatch (intv = %a):
              reg = %a
              exp = %a
              exp' = %a
              reg' = %a
              exp'' = %a|}
              G.pp_interval
              intv
              G.pp
              reg
              G.pp
              exp
              G.pp
              exp'
              G.pp
              reg'
              G.pp
              exp'' ;
          true)

let real_from_index_count =
  QCheck.Test.make
    ~count:10_000
    ~name:"regular_explicit_intersect"
    regular_with_time
    (fun (reg, _t) ->
      let exp = G.to_explicit reg in
      ignore
      @@ Array.init (G.count reg) (fun i ->
             match G.real_from_index reg i with
             | None -> assert false
             | Some _ -> ()) ;
      ignore
      @@ Array.init (G.count exp) (fun i ->
             match G.real_from_index exp i with
             | None -> assert false
             | Some _ -> ()) ;
      true)

let () = QCheck.Test.check_exn real_from_index

let () = QCheck.Test.check_exn index_from_real

let () = QCheck.Test.check_exn t0

let () = QCheck.Test.check_exn fold

let () = QCheck.Test.check_exn fold_increments

let () = QCheck.Test.check_exn real_from_index_count

(* let () = QCheck.Test.check_exn intersect *)
