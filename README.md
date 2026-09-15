# AMECOS validator
A DSL for validating [AMECOS](https://doi.org/10.4230/LIPIcs.OPODIS.2024.4) histories written in Scala.

## Example

The following concurrent register history checks both linearizability and
sequential consistency:

```amecos
// Concurrent register

check Linearizability, SeqCons // Indicate that we want to check linearizability and SeqCons

new Register R // Create a new object of type Register

// Define process and their opexes: Object.Operation(Arguments)/Returns (start, end)
process p1:
    R.Write(1) (1, 2)
    R.Read()/2 (7, 9)

process p2:
    R.Read()/1 (2, 4)
    R.Write(2) (5, 8)


// Ordering, numbers indicate the index of operations in the process
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

## Visualize a history

Generate an SVG history diagram with one colored horizontal timeline per
process, matching the notation used in the AMECOS slides:

```sh
sbt 'run --diagram examples/example.amecos'
```

The output is written next to the input file as `example.svg`. Each op-ex is
shown as a colored double-headed interval with endpoint dots and its
`(start, end)` values. Explicit cross-process ordering is shown with dashed
arrows. PNG and PDF output are also supported:

```sh
sbt 'run --diagram --format png examples/example.amecos'
sbt 'run --diagram --format pdf examples/example.amecos'
```

![Generated diagram for example.amecos](examples/example.svg)
