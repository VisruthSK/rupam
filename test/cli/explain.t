  $ rupam explain unknown-column
  unknown-column
  
  The table at this stage has no column with this name. Column names match exactly, including case.
  $ rupam explain no-such-code
  Usage: rupam explain [--help] [OPTION]… CODE
  rupam: CODE argument: invalid value no-such-code, expected one of
         syntax-error, incomplete-input, bad-interpolation, unknown-name,
         ambiguous-name, unknown-column, unknown-table, unknown-constructor,
         unknown-type, unknown-module, unknown-setting, unknown-verb,
         unknown-option, type-mismatch, infinite-type, wrong-arguments,
         not-a-function, not-a-value, not-a-table, optional-condition,
         invalid-policy, missing-policy-required, missing-under-error,
         int64-range, inexact-float, arithmetic-overflow, row-domain-mismatch,
         column-required, scalar-required, duplicate-name, invalid-selector,
         unordered-slice, group-missing-without-by, nonexhaustive-match,
         pattern-bindings, join-keys, join-collision, join-relationship,
         pivot-shape, duplicate-pivot-cell, restricted-output,
         unsupported-feature, dialect-capability, unreadable-input,
         unsupported-column-type, contract-failure, invalid-config,
         missing-dialect, stale-lock, host-failure, r-range or internal-error
  [124]
