# rupam specification

rupam is a statically typed language for data analysis. It reads like R and
the tidyverse, checks like an ML, and compiles to plain R or Python scripts.
This directory is the design: what the language is meant to be. The rest of
`docs/` describes what the current implementation does, and
[the roadmap](../roadmap.md) tracks the distance between the two.

rupam is alpha. Any change that makes programs nicer to write or faster to
run is made, even when it breaks existing code.

| File | Covers |
| --- | --- |
| [Overview](overview.md) | purpose, audience, principles, non-goals |
| [Syntax](syntax.md) | lexical rules, the grammar, precedence, literals, strings, patterns |
| [Types](types.md) | inference, vectors and lifting, optional values, numbers, newtypes, records, variants, dates |
| [Missing values](missing.md) | the missing-value policy and where it applies |
| [Tables](tables.md) | tables, verbs, column names, keys, joins, reshaping, reading and writing |
| [Standard library](stdlib.md) | aggregates, transforms, numbers, strings, regex, dates |
| [Contracts](contracts.md) | data-dict integration and validation |
| [Dialects](dialects.md) | compilation, lowering, generated code, release builds |
| [Tooling](tooling.md) | the REPL, the data viewer, diagnostics, editors, tests |
| [Projects](projects.md) | `rupam.toml`, the command line, environments, distribution |
| [Guarantees](guarantees.md) | the R and Python bug classes rupam rules out |
| [Conformance](conformance.md) | how dialects are tested, the required cases, benchmarks |
| [Future](future.md) | designs accepted but deliberately not built yet |

Two labels mark exceptions to "this is the design":

- **Experimental**: specified, may be removed.
- **Deferred**: specified so the rest of the design leaves room for it, not
  scheduled.
