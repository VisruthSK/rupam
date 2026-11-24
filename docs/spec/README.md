# rupam specification

rupam is a statically typed language for data analysis that compiles to R and
Python. This directory specifies the language. The rest of `docs/` describes
the current implementation; [the roadmap](../roadmap.md) orders the work
between them.

| File | Contents |
| --- | --- |
| [Overview](overview.md) | scope and rules that apply everywhere |
| [Syntax](syntax.md) | lexical rules, grammar, precedence, literals, patterns |
| [Types](types.md) | inference, shapes, optional values, numbers, sign refinements, user types, dates |
| [Missing values](missing.md) | the missing-value policy |
| [Tables](tables.md) | tables, verbs, column names, keys, joins, reading and writing |
| [Standard library](stdlib.md) | functions available in every dialect |
| [Contracts](contracts.md) | data-dict contracts and validation |
| [Dialects](dialects.md) | compilation and generated scripts |
| [Tooling](tooling.md) | REPL, viewer, diagnostics, editor support, tests |
| [Projects](projects.md) | `rupam.toml`, environments, command line |
| [Guarantees](guarantees.md) | properties every rupam program has |
| [Conformance](conformance.md) | required test cases |
| [Future](future.md) | deferred features |

**Experimental** marks a feature that may be removed. **Deferred** marks a
feature that is specified but not scheduled.
