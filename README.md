# AMECOS validator

A DSL written in scala for evaluating [AMECOS](https://doi.org/10.4230/LIPIcs.OPODIS.2024.4) object specifications, histories and orderings.

## Requirements and commands

The project uses Scala 3.8.3 and SBT 1.12.11.

```sh
sbt compile
sbt test
sbt 'testOnly AmecosSuite -- -z "test name"'
sbt 'run examples/example.amecos'
```

The CLI accepts exactly one input path and prints parser diagnostics, the
resulting order, legality checks, and requested consistency results.

## Example

This concurrent register history checks both linearizability and sequential
consistency:

```amecos
// Concurrent register

check Linearizability, SeqCons

new Register R

// Object.Operation(arguments)/return (start, end)
process p1:
    R.Write(1) (1, 2)
    R.Read()/2 (7, 9)

process p2:
    R.Read()/1 (2, 4)
    R.Write(2) (5, 8)


// References use each process's zero-based operation index.
p1.0->p2.0
p2.0->p2.1
p2.1->p1.1
```
The validator confirms the history is legal and validates linearizability and
sequential consistency.
Run it with:

```sh
sbt 'run examples/example.amecos'
```

## Imports

Quoted imports are expanded textually before parsing. Relative paths resolve
from the importing file, and nested imports are supported:

```amecos
import "fragments/processes.amecos"
```

Imported files are fragments rather than complete applications. Import cycles
and malformed imports are rejected.

## Custom object types

The DSL can define object types and operation predicates. Predicates use the
`V` (validity), `S` (safety), and `L` (liveness) forms and can refer to
`output`, `input`, `context`, `future`, and `count(...):

```amecos
type Counter:
    operation Inc:
        void -> int
        S = output == count(context) + 1;

check Linearizability
new C Counter

process p:
    C.Inc()/1 (0, 2)
```

Built-in register operations are `R.Write(value)` and `R.Read()/value`.
Operation intervals are optional; an omitted interval defaults to `(0, 0)`.
Object names use uppercase letters/underscores and process names start with a
lowercase letter. 

## Partial order search algorithm

If no ordering edges are provided, the validator searches for an order that is
legal and satisfies the requested consistency models. The result may remain a
partial order when the models allow concurrent operations to stay unordered;
linearizability requires a total order. Cyclic explicit ordering is rejected.

## Visualize a history

Generate a history diagram with one colored horizontal timeline per process:

```sh
sbt 'run --diagram examples/example.amecos'
```

The output is written next to the input file, replacing its extension. Each
op-ex is shown as a colored double-headed interval with endpoint dots and its
`(start, end)` values. Explicit ordering is shown with dashed arrows. SVG is
the default; PNG and PDF are also supported:

```sh
sbt 'run --diagram --format png examples/example.amecos'
sbt 'run --diagram --format pdf examples/example.amecos'
```

![Generated diagram for example.amecos](examples/example.svg)
