# AMECOS validator
A DSL for validating AMECOS histories written in Scala.

## Example

The following concurrent register history checks both linearizability and
sequential consistency:

```amecos
// Concurrent register with SeqCons, but not linearizability

check Linearizability, SeqCons // Indicate that we want to check linearizability and SeqCons

new Register R // Create a new CRDT of type Register

// Define process and their opexes: Object.Operation(Arguments)/Returns (start, end)
process p1:
    R.Write(1) (1, 2)
    R.Read()/1 (1, 3)
    R.Read()/2 (7, 9)

process p2:
    R.Read()/1 (0, 0)
    R.Write(2) (3, 4)


// Ordering, numbers indicate the index of operations in the process
p1.0->p1.1
p1.1->p2.0
p2.0->p2.1
p2.1->p1.2
```

Run it with:

```sh
sbt 'run examples/example.amecos'
```
