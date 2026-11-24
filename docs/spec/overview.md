# Overview

A rupam program is checked against the schema of its data, previewed in a
REPL, and compiled to a standalone R or Python script.

## Rules

1. Types are inferred. Annotations are optional and checked.
2. A program has one meaning in every dialect. A dialect that cannot express
   a construct exactly rejects the program.
3. The REPL runs the compiled program. A preview and the compiled script
   produce the same result.
4. Values are immutable.
5. Static checks produce no code. A generated script contains the analysis
   and the adapters listed in [Dialects](dialects.md), nothing else.
6. Every program lowers to native vectorised operations.
7. Options are enums. No function takes a boolean flag.

## Out of scope

Statistical modelling and graphics (deferred, see [Future](future.md)), SQL
targets, distributed execution, matrices, positional table indexing, notebook
formats, and inline R or Python.
