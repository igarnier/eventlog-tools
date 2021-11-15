# eventlog-stats usage

Howto:

- Install `gnuplot` >= 5
- Add `-runtime-variant=i` to the flags of the binary you want to instrument
- Generate eventlog data by running your instrumented program as follows: `OCAML_EVENTLOG_ENABLED=i ./your-program`
  This generates a file `caml-PID.eventlog`a
- `dune exec ./bin/eventlog_stats.exe caml-PID.eventlog` will produce `.png` plots

Warning: this is a hack designed to generate plots for various GC stats; the program expects traces to
be nontrivial (it might crash otherwise)


# eventlog-tools

`eventlog-tools` is a library and set of tools to parse and interact with
traces generated by the OCaml instrumented runtime, available from OCaml 4.11.0
and higher.

## Installation

You can install this package through OPAM.

```
opam pin eventlog-tools .
```

It will install the `eventlog-tools` library, as well as two binaries, `ocaml-eventlog-report` and `ocaml-eventlog-to_chrome`:
- `ocaml-eventlog-report` takes a single `eventlog` tracefile as input and will display histograms and allocations data
- `ocaml-eventlog-to-chrome` takes an `eventlog tracefile` as an input, as well as an output file path as output.
   It will generate a JSON file to be loaded inside Google Chrome's `chrome:\\tracing` viewer.

## Getting started with the instrumented runtime

You can generate a trace file for any OCaml (starting with OCaml 4.11) by compiling it with the appropriate flags.

Here is a sample program:

```ocaml
module SMap = Map.Make(String)

let s i = String.make 512 (Char.chr (i mod 256))

let clear map = SMap.fold (fun k _ m -> SMap.remove k m) map map

let rec seq i = if i = 0 then Seq.empty else fun () -> (Seq.Cons (i, seq (i - 1)))

let () =
  seq 1_000_000
  |> Seq.fold_left (fun m i -> SMap.add (s i) i m) SMap.empty
  |> clear
  |> ignore
```

To enable instrumentation while compiling this program, you need to use the `-runtime-variant=i` flag.
```
ocamlopt -runtime-variant=i program.ml -o program.exe
```
This can also be achieved with `dune` using the `flag` stanza:
```
(executable
  (name program)
 (flags "-runtime-variant=i"))

```

To run, and enable tracing in the freshly compiled program, do as follow:
```
OCAML_EVENTLOG_ENABLED=1 ./program.exe
```

Which will create a new file in the running directory, name `caml-eventlog-$pid`. where `$pid` was the PID of the running process.
```
●●●● ls
caml-eventlog-1663159  program.cmi  program.cmx  program.exe*  program.ml  program.o
```

We can now read this file using the tools provided by this package.

### `ocaml-eventlog-to-chrome`
This tool allows to convert a tool from OCaml's CTF schema to the Catapult format used by the Google Chrome's trace viewing utility, `chrome::/tracing`.
```
●●●● ocaml-eventlog-to-chrome caml-eventlog-1663159 out.json
```

You can then head to the address `chrome://tracing` in Google Chrome and load the newly generated `out.json` file.



### `ocaml-eventlog-report`

This tool provides an overview as well as printing simple histograms for the events traced by the instrumented runtime.

```
●●● ocaml-eventlog-report caml-eventlog-1663159
==== allocs

alloc large: 999711
   alloc 02: 709
   alloc 01: 13
alloc 30-39: 1
   alloc 05: 179015
   alloc 06: 5
   alloc 04: 709
==== major_roots/finalised

   alloc 08: 1

==== minor/finalized
300ns..500ns: 5
200ns..300ns: 34
100ns..200ns: 182
  0ns..100ns: 341

==== major/mark
300ns..500ns: 4
200ns..300ns: 6
100ns..200ns: 26
  0ns..100ns: 666

==== major_roots/memprof
30.0us..50.0us: 1
20.0us..30.0us: 17
10.0us..20.0us: 264

==== major
200ns..300ns: 2
100ns..200ns: 7
  0ns..100ns: 553
    1.0ms..2.0ms: 6
  500.0us..1.0ms: 135
300.0us..500.0us: 4
200.0us..300.0us: 83
100.0us..200.0us: 57
 50.0us..100.0us: 13
  30.0us..50.0us: 235
  20.0us..30.0us: 186
  10.0us..20.0us: 263
   5.0us..10.0us: 1
    3.0us..5.0us: 5
    2.0us..3.0us: 74
==== minor

    1.0us..2.0us: 203
30.0us..50.0us: 2
20.0us..30.0us: 88
10.0us..20.0us: 612
  300ns..500ns: 1
  200ns..300ns: 4
  100ns..200ns: 14
==== major/mark/final

    0ns..100ns: 262
1.0us..2.0us: 3
500ns..1.0us: 42
300ns..500ns: 118
200ns..300ns: 187
==== major/mark/main

100ns..200ns: 72
500ns..1.0us: 1
300ns..500ns: 5
200ns..300ns: 21
100ns..200ns: 96
==== major/roots

  0ns..100ns: 299

==== compact/main
3.0us..5.0us: 3
2.0us..3.0us: 31
1.0us..2.0us: 247
500ns..1.0us: 1

==== major/check_and_compact
500.0us..1.0ms: 140
500.0us..1.0ms: 140
  1.0us..2.0us: 1
  500ns..1.0us: 7
  300ns..500ns: 85
  200ns..300ns: 43
  100ns..200ns: 4
==== major/mark/roots

    0ns..100ns: 1

==== minor/local_roots
5.0us..10.0us: 1
 3.0us..5.0us: 229
 2.0us..3.0us: 192
3.0us..5.0us: 5
2.0us..3.0us: 7
1.0us..2.0us: 48
500ns..1.0us: 355
==== major/sweep

300ns..500ns: 287
  500.0us..1.0ms: 1
300.0us..500.0us: 5
200.0us..300.0us: 87
100.0us..200.0us: 180
 50.0us..100.0us: 25
  30.0us..50.0us: 205
  20.0us..30.0us: 197
==== minor/update_weak

   5.0us..10.0us: 1

==== minor/ref_tables
200ns..300ns: 3
100ns..200ns: 29
  0ns..100ns: 670

==== major_roots/dynamic_global
300ns..500ns: 1
100ns..200ns: 4
  0ns..100ns: 697
3.0us..5.0us: 6
2.0us..3.0us: 134
300ns..500ns: 6
200ns..300ns: 4
100ns..200ns: 82
==== major_roots/C

  0ns..100ns: 330
500ns..1.0us: 3
300ns..500ns: 45
200ns..300ns: 159
100ns..200ns: 291
==== major_roots/local

  0ns..100ns: 64
5.0us..10.0us: 1
 2.0us..3.0us: 1
 1.0us..2.0us: 110
 500ns..1.0us: 311
 300ns..500ns: 111
==== minor/copy

 200ns..300ns: 28

==== major/mark/global_roots_slice
30.0us..50.0us: 1
20.0us..30.0us: 41
10.0us..20.0us: 660

==== major_roots/hook
5.0us..10.0us: 1
 3.0us..5.0us: 179
 2.0us..3.0us: 242

==== major/work/extra
1.0us..2.0us: 1
  0ns..100ns: 561

==== major/mark/slice/pointers
500..1.0K: 422

==== request_major/adjust_gc_speed
1.0K..2.0K: 422

==== request_major/alloc_shr
0..100: 181436

==== major/work/mark
1.0K..2.0K: 702

==== minor/promoted
500.0K..1.0M: 1
      0..100: 1264

==== alloc_jump
  500.0K..1.0M: 421
300.0K..500.0K: 279
200.0K..300.0K: 1

==== major/work/sweep
0..100: 1264

==== eventlog/flush
median flush time: 322.7us
total flush time: 16.8ms
flush count: 52
    1.0M..2.0M: 139
  500.0K..1.0M: 2
200.0K..300.0K: 1
100.0K..200.0K: 140
```
