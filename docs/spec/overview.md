# Overview

## What rupam is for

Analysts who work in R write the same transformations again in Python, lose
their abstractions on the way, and discover misspelled columns, silent
missing values, row explosions after joins and time-zone mistakes long after
the code ran. ML languages catch this class of mistake before anything runs,
but none of them is pleasant for exploring data.

rupam takes R's vocabulary (pipes, dplyr verbs, bare column names, formulas)
and puts an inferred ML type system underneath it. A program is checked
against the schema of its data while it is being written, previewed in a REPL,
and compiled to an ordinary R or Python script that runs without rupam.

The audience is R users who want static types and a better experience than R
gives them. rupam has to be an upgrade, not a sidegrade: everything an analyst
does comfortably in R should be at least as short in rupam.

## Principles

1. **Types are inferred.** Annotations are optional documentation. The types
   show up where you look (hover, the REPL, error messages) and stay out of
   the code otherwise.
2. **One meaning, many dialects.** A program means the same thing whether it
   compiles to R with dplyr or Python with Polars. A dialect that cannot
   reproduce a rule refuses to compile that program instead of approximating
   it.
3. **What you preview is what runs.** The REPL compiles every input and runs
   it on the target. There is no second engine whose answers could drift.
4. **Bug classes are removed, not warned about.** Recycling, NA/NaN confusion,
   silent coercion, time-zone mixing and join explosions are made impossible
   by the type system or the semantics. See [Guarantees](guarantees.md).
5. **Everything is immutable.** Values, vectors, records and tables never
   change. A transformation returns a new value; shadowing a name binds a new
   value under it.
6. **No defensive code.** Static checks are erased. Generated scripts contain
   the analysis and only the small adapters needed where a target library's
   behaviour contradicts rupam.
7. **Fast by construction.** Programs lower to native vectorised operations,
   pipelines stay pipelines, and nothing allocates without reason.
8. **APIs take enums, not boolean flags.** `missing = skip`, never
   `na.rm = TRUE`.

## Not in scope now

General statistical modelling, a graphics grammar, SQL targets, distributed
execution, matrices and positional table indexing, GPUs, a notebook format and
arbitrary inline R or Python. Models and graphics are designed for (see
[Future](future.md)); the rest is out.
