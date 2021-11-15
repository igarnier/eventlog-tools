open Rresult.R.Infix
module TS = Eventlog.Timeseries

type 'a timestamped = { t0 : int; data : 'a }

type duration = int timestamped

module Events = struct
  type t =
    { h : (Eventlog.phase, int) Hashtbl.t;
      (* temporary table holding the entry event to track an event's lifetime *)
      mutable last_flush : int * int;
      (* timestamp * duration *)
      events : (Eventlog.phase, duration list) Hashtbl.t
    }

  (* note: when computing an event duration, we check if the last flush event happened
     inbetween. if so, we deduce the duration of the flush event from this event. *)
  let update t name v_start v_end =
    match Hashtbl.find_opt t.events name with
    | Some l ->
        let last_flush_ts = fst t.last_flush in
        let v =
          if v_start < last_flush_ts && last_flush_ts < v_end then
            v_end - v_start - snd t.last_flush
          else v_end - v_start
        in
        Hashtbl.replace t.events name ({ t0 = v_start; data = v } :: l)
    | None ->
        Hashtbl.add t.events name [{ t0 = v_start; data = v_end - v_start }]

  let handle_exit ({ h; _ } as t) name v =
    match Hashtbl.find_opt h name with
    | Some v' ->
        if v' > v then assert false
        else (
          Hashtbl.remove h name ;
          update t name v' v)
    | None -> failwith "no attached entry event"

  let handle_entry { h; _ } name v =
    match Hashtbl.find_opt h name with
    | Some _ -> failwith "overlaping entry events"
    | None -> Hashtbl.add h name v

  let handle_flush t ts dur = t.last_flush <- (ts, dur)

  let create () =
    { h = Hashtbl.create 12; events = Hashtbl.create 12; last_flush = (0, 0) }

  let get { events; _ } name = Hashtbl.find events name

  let iter t f = Hashtbl.iter f t.events
end

type allocs = (Eventlog.bucket, int timestamped list) Hashtbl.t

type counters = (Eventlog.counter_kind, int timestamped list) Hashtbl.t

type t =
  { events : Events.t;
    allocs : allocs;
    counters : counters;
    mutable flushs : int list
  }

let read_event { Eventlog.payload; timestamp; _ }
    ({ allocs; events; counters; _ } as t) =
  match payload with
  | Alloc { bucket; count } -> (
      match Hashtbl.find_opt allocs bucket with
      | Some v ->
          Hashtbl.replace allocs bucket ({ t0 = timestamp; data = count } :: v)
      | None -> Hashtbl.add allocs bucket [{ t0 = timestamp; data = count }])
  | Entry { phase } -> Events.handle_entry events phase timestamp
  | Exit { phase } -> Events.handle_exit events phase timestamp
  | Counter { kind; count } -> (
      match Hashtbl.find_opt counters kind with
      | Some l ->
          Hashtbl.replace counters kind ({ t0 = timestamp; data = count } :: l)
      | None -> Hashtbl.add counters kind [{ t0 = timestamp; data = count }])
  | Flush { duration } ->
      t.flushs <- duration :: t.flushs ;
      Events.handle_flush events timestamp duration

let load_file path =
  let open Rresult.R.Infix in
  Fpath.of_string path >>= Bos.OS.File.read >>= fun content ->
  Ok (Bigstringaf.of_string ~off:0 ~len:(String.length content) content)

module Caml_alloc = struct
  (* sizes as reported by the instrumented runtime *)
  type t = Words of int | Words_int of int * int | Words_large

  let compare = Stdlib.compare

  let hash = Hashtbl.hash

  let equal x y = Stdlib.compare x y = 0

  let pp fmtr = function
    | Words i -> Format.fprintf fmtr "%d w" i
    | Words_int (i, j) -> Format.fprintf fmtr "[%d;%d] w" i j
    | Words_large -> Format.pp_print_string fmtr "large"

  let w i = Words i

  let wint i j =
    assert (i < j) ;
    Words_int (i, j)

  let large = Words_large

  (* ugly: must match buckets defined in consts.ml *)
  let buckets =
    [| w 1;
       w 2;
       w 3;
       w 4;
       w 5;
       w 6;
       w 7;
       w 8;
       w 9;
       wint 10 19;
       wint 20 29;
       wint 30 39;
       wint 40 49;
       wint 50 59;
       wint 60 69;
       wint 70 79;
       wint 80 89;
       wint 90 99;
       large
    |]
end

module Caml_alloc_vec =
  Basic_structures.Basic_impl.Free_module.Float_valued.Make_with_map (Caml_alloc)

module Alloc_size = struct
  type t = W of { words : int } | Inf

  let compare x y =
    match (x, y) with
    | (W { words = w1 }, W { words = w2 }) -> Int.compare w1 w2
    | (Inf, Inf) -> 0
    | (W _, Inf) -> -1
    | (Inf, W _) -> 1

  let hash = Hashtbl.hash

  let equal x y = Stdlib.compare x y = 0

  let pp fmtr = function
    | W { words } -> Format.pp_print_int fmtr words
    | Inf -> Format.pp_print_string fmtr "∞"
end

module Alloc_size_vec =
  Basic_structures.Basic_impl.Free_module.Float_valued.Make_with_map (Alloc_size)

let kernel =
  Stats.Fin.Float.kernel
    (module Alloc_size_vec)
    (function
      | Caml_alloc.Words words -> [(Alloc_size.W { words }, 1.)]
      | Words_int (i, j) ->
          let len = j - i + 1 in
          let mass = 1. /. float_of_int len in
          List.init len (fun k -> (Alloc_size.W { words = i + k }, mass))
      | Words_large -> [(Alloc_size.Inf, 1.)])

let alloc_distribution (allocs : allocs) =
  let sum : int timestamped list -> int =
   fun l -> List.fold_left (fun acc { t0 = _; data } -> acc + data) 0 l
  in
  Hashtbl.to_seq allocs
  |> Seq.map (fun (b, counts) ->
         (Caml_alloc.buckets.(b), float_of_int (sum counts)))
  |> List.of_seq
  |> Stats.Fin.Float.measure (module Caml_alloc_vec)

let alloc_total (allocs : allocs) =
  let allocs = Hashtbl.to_seq allocs |> List.of_seq in
  let sorted =
    List.fold_left
      (fun acc (b, allocs) ->
        List.fold_left
          (fun acc { t0; data = count } ->
            match Caml_alloc.buckets.(b) with
            | Caml_alloc.Words w -> { t0; data = (count * w, count * w) } :: acc
            | Caml_alloc.Words_int (l, h) ->
                assert (l < h) ;
                { t0; data = (count * l, count * h) } :: acc
            | Caml_alloc.Words_large -> acc)
          acc
          allocs)
      []
      allocs
    |> List.sort (fun ts1 ts2 -> Int.compare ts1.t0 ts2.t0)
  in
  let low =
    TS.of_raw
    @@ List.map
         (fun { t0; data = (l, _) } -> (float_of_int t0, float_of_int l))
         sorted
  in
  let hi =
    TS.of_raw
    @@ List.map
         (fun { t0; data = (_, h) } -> (float_of_int t0, float_of_int h))
         sorted
  in
  (TS.cumulative low, TS.cumulative hi)

let ev name (events : Events.t) =
  let events = Events.get events (Eventlog.phase_of_string name) in
  let events = List.sort (fun ts1 ts2 -> Int.compare ts1.t0 ts2.t0) events in
  TS.of_raw
  @@ List.map
       (fun { t0; data = dt } -> (float_of_int t0, float_of_int dt))
       events

let ev_cumulative name (events : Events.t) = TS.cumulative (ev name events)

let counters name (counters : counters) =
  let counters = Hashtbl.find counters (Eventlog.counter_kind_of_string name) in
  TS.of_raw
  @@ List.map
       (fun { t0; data } -> (float_of_int t0, float_of_int data))
       counters

let counters_cumulative name (c : counters) = TS.cumulative (counters name c)

let duration_distribution (durations : int list) =
  Stats.Emp.of_raw_data (Array.of_list durations)

(*
   stats:
   - raw allocation rate (minor GC size/minor GC frequency)
   - block size distribution
   - promotion rate (major allocs/seconds)
*)

let plot_alloc_size_dist (dist : (Alloc_size.t, float) Stats.Stats_intf.fin_mes)
    =
  let open Plot in
  let (`Measure points) = Stats.Fin.Float.raw_data_measure dist in
  let data =
    List.map
      (fun (alloc_size, weight) ->
        match alloc_size with
        | Alloc_size.W { words } -> (string_of_int words, weight)
        | Inf -> ("inf", weight))
      points
    |> Data.of_list
  in
  Bar.simple data ()

let sf = Printf.sprintf

let get_id s =
  let i0 = String.rindex_from s (String.length s - 1) '-' in
  let i1 = String.index_from s i0 '.' in
  let id = int_of_string @@ String.sub s (i0 + 1) (i1 - i0 - 1) in
  Format.asprintf "%02d" id

let process in_file =
  let module P = Eventlog.Parser in
  load_file in_file >>= fun data ->
  let decoder = P.decoder () in
  let total_len = Bigstringaf.length data in
  P.src decoder data 0 total_len true ;
  let allocs = Hashtbl.create 10 in
  let events = Events.create () in
  let counters = Hashtbl.create 10 in
  let t = { allocs; events; flushs = []; counters } in
  let rec aux () =
    match P.decode decoder with
    | `Ok (Event ev) ->
        read_event ev t ;
        aux ()
    | `Ok (Header _) -> aux ()
    | `Error (`Msg msg) ->
        Printf.eprintf "some events were discarded: %s" msg ;
        Ok ()
    | `End -> Ok ()
    | `Await -> Ok ()
  in
  aux () >>= fun () -> Ok t

let estimate_allocation_rate ~minor_heap_words ~minor_gc_events ~duration =
  (* this ignore the words present in the minor heap when the program exits *)
  let allocated = minor_heap_words *. minor_gc_events in
  allocated /. duration

let main in_files =
  let rec loop files acc =
    match files with
    | [] -> Ok acc
    | file :: tl -> process file >>= fun t -> loop tl ((get_id file, t) :: acc)
  in
  loop in_files [] >>= fun data ->
  let alloc_plots =
    List.fold_left
      (fun acc (id, t) ->
        let (low, hi) = alloc_total t.allocs in
        let low = TS.line_2d ~points:low ~legend:(sf "lo-%s" id) () in
        let hi = TS.line_2d ~points:hi ~legend:(sf "hi-%s" id) () in
        low :: hi :: acc)
      []
      data
  in
  List.iter
    (fun (_, t) ->
      let update_weak = counters "minor/promoted" t.counters in
      let finalized = ev "minor/finalized" t.events in
      Format.printf
        "update_weak: %d, finalized: %d@."
        (TS.length update_weak)
        (TS.length finalized))
    data ;
  let gc_time_plots =
    List.fold_left
      (fun acc (id, t) ->
        let minor = ev_cumulative "minor" t.events in
        let major = ev_cumulative "major" t.events in
        Format.printf "%s %d %d@." id (TS.length minor) (TS.length major) ;
        let minor =
          TS.line_2d
            ~points:minor
            ~style:Plot.Style.(default |> set_point ~ptyp:Plot.Pointtype.plus)
            ~legend:(sf "minor-%s" id)
            ()
        in
        let major = TS.line_2d ~points:major ~legend:(sf "major-%s" id) () in
        minor :: major :: acc)
      []
      data
  in
  let gc_time_ratio =
    List.fold_left
      (fun acc (id, t) ->
        let minor = (ev_cumulative "minor" t.events :> (float * float) list) in
        let major = (ev_cumulative "major" t.events :> (float * float) list) in
        (* compute union of sampling times *)
        let (minor_times, minor_data) = List.split minor in
        let (major_times, major_data) = List.split major in
        let times =
          Array.of_list
          @@ List.sort_uniq
               Float.compare
               (List.rev_append minor_times major_times)
        in
        let minor_series =
          let minor_grid =
            Series.Grid.explicit ~sampling_points:(Array.of_list @@ minor_times)
          in
          let minor_data = List.rev (List.tl (List.rev minor_data)) in
          Series.make Series.dense_float minor_grid (Array.of_list minor_data)
        in
        let major_series =
          let major_grid =
            Series.Grid.explicit ~sampling_points:(Array.of_list @@ major_times)
          in
          let major_data = List.rev (List.tl (List.rev major_data)) in
          Series.make Series.dense_float major_grid (Array.of_list major_data)
        in
        let plot_data =
          Array.fold_left
            (fun acc t ->
              try
                let minor_time = Series.eval_t minor_series t in
                let major_time = Series.eval_t major_series t in
                let total = minor_time +. major_time in
                let ratio = total /. t in
                Plot.r2 t ratio :: acc
              with Assert_failure _ -> acc)
            []
            times
        in
        let plot_data = Plot.Data.of_list (List.rev plot_data) in
        Plot.Line.line_2d ~points:plot_data ~legend:(sf "ratio-%s" id) () :: acc)
      []
      data
  in
  let promotions =
    List.fold_left
      (fun acc (id, t) ->
        let promoted = counters_cumulative "minor/promoted" t.counters in
        TS.line_2d ~points:promoted ~legend:(sf "promoted-%s" id) () :: acc)
      []
      data
  in
  let promotions_distribution =
    List.fold_left
      (fun acc (id, t) ->
        let promoted = counters "minor/promoted" t.counters in
        (id, TS.hist ~points:promoted ~bins:30 ~legend:(sf "promoted-%s" id) ())
        :: acc)
      []
      data
  in
  let promotions_distribution_binned =
    List.fold_left
      (fun acc (id, t) ->
        let promoted = counters "minor/promoted" t.counters in
        let binned = TS.bin ~timeslice:(0.2 *. 1e9) promoted in
        let bins =
          Array.to_list
          @@ Array.mapi (fun i x -> (string_of_int i, TS.sum x)) binned
        in
        let title = sf "promoted words (%s), by slice of 0.2s" id in
        (id, title, Plot.Bar.simple (Plot.Data.of_list bins) ()) :: acc)
      []
      data
  in

  let minor_gc_stationarity_via_counters =
    (* make bins of width 0.2 seconds *)
    List.fold_left
      (fun acc (id, t) ->
        let promoted = counters "minor/promoted" t.counters in
        let binned = TS.bin ~timeslice:(0.2 *. 1e9) promoted in
        let bins =
          Array.to_list
          @@ Array.mapi
               (fun i x -> (string_of_int i, float_of_int @@ TS.length x))
               binned
        in
        let title =
          sf
            "minor gc events estimated from promotions (%s), by slice of 0.2s"
            id
        in
        (id, title, Plot.Bar.simple (Plot.Data.of_list bins) ()) :: acc)
      []
      data
  in

  (*
     Estimate average allocation rate over the whole execution
     from the total number of minor gc events
  *)
  let () =
    List.iter
      (fun (id, t) ->
        let minor = ev "minor" t.events in
        let duration = TS.duration minor *. 1e-9 in
        let minor_gc_events = float_of_int @@ TS.length minor in
        Format.printf
          "allocation rate for %s (words/s) = %f (duration = %f, minor gcs = \
           %f)@."
          id
          (estimate_allocation_rate
             ~minor_heap_words:262144.0
             ~minor_gc_events
             ~duration)
          duration
          minor_gc_events)
      data
  in

  let minor_gc_stationarity_via_events =
    List.fold_left
      (fun acc (id, t) ->
        let minor = ev "minor" t.events in
        let binned = TS.bin ~timeslice:(0.2 *. 1e9) minor in
        let bins =
          Array.to_list
          @@ Array.mapi
               (fun i x -> (string_of_int i, float_of_int @@ TS.length x))
               binned
        in
        let title =
          sf
            "minor gc events estimated from minor dispatch (%s), by slice of \
             0.2s"
            id
        in
        (id, title, Plot.Bar.simple (Plot.Data.of_list bins) ()) :: acc)
      []
      data
  in

  (* let minor_gc_duration_hist =
   *   List.fold_left
   *     (fun acc (id, t) ->
   *       let minor = ev "minor" t.events in
   *       let binned = TS.bin ~timeslice:(0.2 *. 1e9) minor in
   *       let bins =
   *         Array.to_list
   *         @@ Array.mapi
   *              (fun i x -> (string_of_int i, float_of_int @@ TS.length x))
   *              binned
   *       in
   *       let title =
   *         sf
   *           "minor gc events estimated from minor dispatch (%s), by slice of \
   *            0.2s"
   *           id
   *       in
   *       (id, title, Plot.Bar.simple (Plot.Data.of_list bins) ()) :: acc)
   *     []
   *     data
   * in *)
  List.iter
    (fun (id, t) ->
      let alloc_size_dist =
        Stats.Fin.Float.pushforward (alloc_distribution t.allocs) kernel
      in
      let proba = Stats.Fin.Float.normalize alloc_size_dist in
      let mean =
        Stats.Fin.Float.integrate (Stats.Fin.as_measure proba) (fun s ->
            match s with
            | Alloc_size.W { words } -> float_of_int words
            | Alloc_size.Inf -> 0.)
      in
      Format.printf "%s: average block size = %f@." id mean ;
      Plot.(
        run
          ~plot:
            (plot2
               ~xaxis:"word size"
               ~yaxis:"freq"
               ~title:(sf "block size distribution on major heap (%s)" id)
               [plot_alloc_size_dist alloc_size_dist])
          ~target:
            (png
               ~pixel_size:(1280, 1024)
               ~png_file:(sf "words_dist-%s.png" id)
               ())
          exec))
    data ;
  Plot.(
    run
      ~plot:
        (plot2
           ~xaxis:"time"
           ~yaxis:"words"
           ~title:"cumulative allocs on major heap"
           alloc_plots)
      ~target:
        (png ~pixel_size:(1280, 1024) ~png_file:(sf "allocs_cumulative.png") ())
      exec) ;
  Plot.(
    run
      ~plot:
        (plot2
           ~xaxis:"time"
           ~yaxis:"gc"
           ~title:"cumulative gc time"
           gc_time_plots)
      ~target:
        (png ~pixel_size:(1920, 1260) ~png_file:(sf "gc_cumulative.png") ())
      exec) ;
  Plot.(
    run
      ~plot:
        (plot2
           ~xaxis:"time"
           ~yaxis:"(minor+major)/time"
           ~title:"gc overhead(t)"
           gc_time_ratio)
      ~target:(png ~pixel_size:(1920, 1260) ~png_file:(sf "gc_ratio.png") ())
      exec) ;
  Plot.(
    run
      ~plot:
        (plot2
           ~xaxis:"time"
           ~yaxis:"promoted words"
           ~title:"promoted (cumulative)"
           promotions)
      ~target:(png ~pixel_size:(1920, 1260) ~png_file:(sf "promoted.png") ())
      exec) ;
  ListLabels.iter promotions_distribution ~f:(fun (id, dist) ->
      Plot.(
        run
          ~plot:
            (plot2
               ~xaxis:"words"
               ~yaxis:"# of minor GCs"
               ~title:"promoted words (whole execution)"
               [dist])
          ~target:
            (png
               ~pixel_size:(1920, 1260)
               ~png_file:(sf "promoted_dist_%s.png" id)
               ())
          exec)) ;
  ListLabels.iter promotions_distribution_binned ~f:(fun (id, title, dist) ->
      Plot.(
        run
          ~plot:(plot2 ~xaxis:"t" ~yaxis:"words" ~title [dist])
          ~target:
            (png
               ~pixel_size:(1920, 1260)
               ~png_file:(sf "promoted_dist_statio_%s.png" id)
               ())
          exec)) ;

  ListLabels.iter
    minor_gc_stationarity_via_counters
    ~f:(fun (id, title, dist) ->
      Plot.(
        run
          ~plot:(plot2 ~xaxis:"t" ~yaxis:"minor gcs" ~title [dist])
          ~target:
            (png
               ~pixel_size:(1920, 1260)
               ~png_file:(sf "minor_gc_events_statio_%s_counters.png" id)
               ())
          exec)) ;

  ListLabels.iter minor_gc_stationarity_via_events ~f:(fun (id, title, dist) ->
      Plot.(
        run
          ~plot:(plot2 ~xaxis:"t" ~yaxis:"minor gcs" ~title [dist])
          ~target:
            (png
               ~pixel_size:(1920, 1260)
               ~png_file:(sf "minor_gc_events_statio_%s_events.png" id)
               ())
          exec)) ;
  Ok ()

let () =
  match List.tl @@ Array.to_list Sys.argv with
  | [] -> exit 0
  | files -> ( match main files with Error _ -> exit 1 | Ok () -> exit 0)
