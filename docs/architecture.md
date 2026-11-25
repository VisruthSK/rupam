# Architecture

One pipeline serves the REPL, the compiler and the reference evaluator:

```text
source ─ Frontend ─▶ Syntax ─ Elaborate ─▶ Core items ─┬─ Reference ─▶ values
                                    ▲                  ├─ R host + dplyr ─▶ R
                              Elaborate.env            └─ Python host + Polars ─▶ Python
```

## Types

`Types.t` is the only type language. Scalars, vectors, options, tuples,
records, tables, variants and functions are Hindley–Milner types with
unification variables. Records and tables share one row representation; a row
is a set of named fields with an optional row variable, so column order is not
part of a type. Numeric literals carry a `number` constraint and default to
Int64 when unconstrained.

## Core

`Core` is the typed program every back end consumes. Each expression carries
its type, its shape (a scalar or a column of a row domain) and its source span.
Every decision the checker makes is explicit in the tree: integer-to-float
conversion is a `Convert` node, optional lifting is a `Lift` node, missing
policies sit on reductions and filters, user functions applied to columns are
inlined, and names are resolved to binding identities. Back ends never consult
the checker.

A table is a source followed by stages. A source is a file, a contracted table,
a join, or an earlier table binding. Column expressions inside stages refer to
columns by name. Each table carries its ordered schema, keys and order.

A program is a list of items: definitions, type declarations, values to show
and tables to write. A table definition is an ordinary definition, so a
compiled script and a REPL session both bind each table once and refer to it
by name.

## Elaborate

`Elaborate.item env declaration` checks one top-level declaration and returns
the extended environment and its core items. Scalar inference, table verbs,
shapes, optionality, refinements and missing policies are one judgment. The
REPL, `rupam check`, `rupam compile`, the language server and the conformance
runner all call it.

## Back ends

A host lowers scalar core to its language and owns printing, worker startup and
source maps. A dialect lowers table core to its library. `R_host` with `Dplyr`
and `Python_host` with `Polars` each turn items into a script. The REPL sends
the same statements to a persistent worker, so each binding is evaluated once
and later items refer to it by name.
