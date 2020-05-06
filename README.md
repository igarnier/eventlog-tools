# ocaml-eventlog-tools

`ocaml-eventlog-tools` is a library and set of tools to parse and interact with
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
