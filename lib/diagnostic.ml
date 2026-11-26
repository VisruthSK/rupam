type span = Lexing.position * Lexing.position

type code =
  | Syntax_error
  | Incomplete_input
  | Bad_interpolation
  | Unknown_name
  | Ambiguous_name
  | Unknown_column
  | Unknown_table
  | Unknown_constructor
  | Unknown_type
  | Unknown_module
  | Unknown_setting
  | Unknown_verb
  | Unknown_option
  | Type_mismatch
  | Infinite_type
  | Wrong_arguments
  | Not_a_function
  | Not_a_value
  | Not_a_table
  | Optional_condition
  | Invalid_policy
  | Missing_policy_required
  | Missing_under_error
  | Int_range
  | Inexact_float
  | Arithmetic_overflow
  | Row_domain_mismatch
  | Column_required
  | Scalar_required
  | Duplicate_name
  | Invalid_selector
  | Unordered_slice
  | Group_missing_without_by
  | Nonexhaustive_match
  | Pattern_bindings
  | Join_keys
  | Join_collision
  | Join_relationship
  | Pivot_shape
  | Duplicate_pivot_cell
  | Restricted_output
  | Unsupported_feature
  | Dialect_capability
  | Unreadable_input
  | Unsaved_analysis
  | Unsupported_column_type
  | Contract_failure
  | Invalid_config
  | Missing_target
  | Stale_lock
  | Host_failure
  | R_range
  | Internal_error
[@@deriving enumerate]

let all = all_of_code

type t =
  { code : code
  ; span : span option
  ; message : string
  }

exception Error of t

let fail ?span code message = raise (Error { code; span; message })

let of_exn = function
  | Error d -> d
  | Sys_error message -> { code = Unreadable_input; span = None; message }
  | Failure message | Invalid_argument message ->
    { code = Internal_error; span = None; message }
  | exn -> { code = Internal_error; span = None; message = Printexc.to_string exn }
;;

let locate span f =
  try f () with
  | Error ({ span = None; _ } as d) -> raise (Error { d with span = Some span })
;;

let name = function
  | Syntax_error -> "syntax-error"
  | Incomplete_input -> "incomplete-input"
  | Bad_interpolation -> "bad-interpolation"
  | Unknown_name -> "unknown-name"
  | Ambiguous_name -> "ambiguous-name"
  | Unknown_column -> "unknown-column"
  | Unknown_table -> "unknown-table"
  | Unknown_constructor -> "unknown-constructor"
  | Unknown_type -> "unknown-type"
  | Unknown_module -> "unknown-module"
  | Unknown_setting -> "unknown-setting"
  | Unknown_verb -> "unknown-verb"
  | Unknown_option -> "unknown-option"
  | Type_mismatch -> "type-mismatch"
  | Infinite_type -> "infinite-type"
  | Wrong_arguments -> "wrong-arguments"
  | Not_a_function -> "not-a-function"
  | Not_a_value -> "not-a-value"
  | Not_a_table -> "not-a-table"
  | Optional_condition -> "optional-condition"
  | Invalid_policy -> "invalid-policy"
  | Missing_policy_required -> "missing-policy-required"
  | Missing_under_error -> "missing-under-error"
  | Int_range -> "int-range"
  | Inexact_float -> "inexact-float"
  | Arithmetic_overflow -> "arithmetic-overflow"
  | Row_domain_mismatch -> "row-domain-mismatch"
  | Column_required -> "column-required"
  | Scalar_required -> "scalar-required"
  | Duplicate_name -> "duplicate-name"
  | Invalid_selector -> "invalid-selector"
  | Unordered_slice -> "unordered-slice"
  | Group_missing_without_by -> "group-missing-without-by"
  | Nonexhaustive_match -> "nonexhaustive-match"
  | Pattern_bindings -> "pattern-bindings"
  | Join_keys -> "join-keys"
  | Join_collision -> "join-collision"
  | Join_relationship -> "join-relationship"
  | Pivot_shape -> "pivot-shape"
  | Duplicate_pivot_cell -> "duplicate-pivot-cell"
  | Restricted_output -> "restricted-output"
  | Unsupported_feature -> "unsupported-feature"
  | Dialect_capability -> "dialect-capability"
  | Unreadable_input -> "unreadable-input"
  | Unsaved_analysis -> "unsaved-analysis"
  | Unsupported_column_type -> "unsupported-column-type"
  | Contract_failure -> "contract-failure"
  | Invalid_config -> "invalid-config"
  | Missing_target -> "missing-target"
  | Stale_lock -> "stale-lock"
  | Host_failure -> "host-failure"
  | R_range -> "r-range"
  | Internal_error -> "internal-error"
;;

let of_name text = List.find_opt (fun code -> name code = text) all

let explanation = function
  | Syntax_error ->
    "The input is not valid Rupam at this point. Check for a missing operator, comma or \
     closing bracket."
  | Incomplete_input ->
    "The input ended while an expression was still open, such as after `|>`, an operator \
     or an open bracket."
  | Bad_interpolation ->
    "A string slot `{...}` holds exactly one expression. Write `{{` or `}}` for a \
     literal brace."
  | Unknown_name ->
    "No binding, function or column with this name is in scope. A suggestion is given \
     when a similar name exists."
  | Ambiguous_name ->
    "A bare name inside a verb refers to both a column and a binding in scope. Write \n\
    \     `$name` for the column or `^name` for the binding."
  | Unknown_column ->
    "The table at this stage has no column with this name. Column names match exactly, \
     including case."
  | Unknown_table -> "No table or contracted table with this name is in scope."
  | Unknown_constructor -> "No enum or variant declares this constructor."
  | Unknown_type -> "No type with this name and number of parameters is declared."
  | Unknown_module -> "Modules and foreign imports are not available for this name."
  | Unknown_setting -> "`use` accepts `missing` and `group_missing`."
  | Unknown_verb -> "This name is not a table verb or a table function in scope."
  | Unknown_option -> "The verb or function takes no option with this name."
  | Type_mismatch ->
    "The expression has a different type from the one its context requires. Convert \
     explicitly; Rupam never converts nominal types or optional values implicitly."
  | Infinite_type ->
    "A value would need to contain itself, for example a function applied to itself."
  | Wrong_arguments -> "The call has the wrong number, order or names of arguments."
  | Not_a_function -> "Only functions can be called."
  | Not_a_value ->
    "A function was used where a value is required. Call it with arguments."
  | Not_a_table -> "A table expression is required here."
  | Optional_condition ->
    "A condition can be missing for some rows. `filter` needs `missing = skip`, `keep` \
     or `error`; `if` needs a present Bool, for example from `coalesce` or `some(x)`."
  | Invalid_policy -> "A missing-value policy is `skip`, `keep` or `error`."
  | Missing_policy_required ->
    "A reduction or grouping reads values that can be missing. Choose `missing =` (or \
     `group_missing =` for grouping keys): `skip` leaves them out, `keep` makes the \
     result missing, `error` stops the run."
  | Missing_under_error ->
    "A missing value reached an operation whose policy is `missing = error`."
  | Int_range -> "The integer is outside the portable Int range."
  | Inexact_float ->
    "The integer cannot be represented exactly as Float64, so it is not promoted \
     implicitly."
  | Arithmetic_overflow ->
    "The result is outside the range of its type. Rupam fails instead of wrapping or \
     producing infinity."
  | Row_domain_mismatch ->
    "Columns from different tables or differently filtered rows cannot be combined by \
     position. Join the tables or compute both columns from the same rows."
  | Column_required -> "This operation needs a column, not a single value."
  | Scalar_required ->
    "`summarize` needs one value per group. Reduce the column, for example with `mean` \
     or `n()`."
  | Duplicate_name ->
    "A name appears twice where names must be unique, such as output columns, record \
     fields, types or constructors."
  | Invalid_selector ->
    "A column selector is a name, a range `a:b` in schema order, or a list of names."
  | Unordered_slice ->
    "A positional operation needs a total row order. `arrange` by columns that include a \
     key, or establish one with `assert_key`."
  | Group_missing_without_by -> "`group_missing` applies to grouped verbs. Add `by =`."
  | Nonexhaustive_match ->
    "Every `match` covers every possible value. Add the missing case or `_`."
  | Pattern_bindings ->
    "Alternatives in one pattern must bind the same names, and a pattern binds each name \
     once."
  | Join_keys ->
    "A join names its keys with `by = key` or `by = (left_key == right_key)`, and the \
     key types must agree."
  | Join_relationship ->
    "No key proves how many rows match on each side, so the join could multiply rows. \
     State `relationship = one_to_one`, `many_to_one`, `one_to_many` or `many_to_many`."
  | Join_collision ->
    "Both tables have a non-key column with this name. Rename or drop one side before \
     joining."
  | Pivot_shape ->
    "The pivot arguments do not describe a valid reshaping: names and values must be \
     distinct fields with a common type."
  | Duplicate_pivot_cell ->
    "Two rows produce the same output cell. Aggregate with `values_fn` or make the \
     identifying columns unique."
  | Restricted_output ->
    "Every remaining column is restricted, so there is nothing to show."
  | Unsupported_feature ->
    "This feature is specified but not yet implemented in this position."
  | Dialect_capability ->
    "The selected dialect cannot run this operation, or it requires an explicit import \
     and `strict = false`."
  | Unsaved_analysis ->
    "Locking records a saved program. Save the session with `:save FILE` first."
  | Unreadable_input ->
    "The data file is missing, has an unsupported extension, or cannot be read by this \
     reader."
  | Unsupported_column_type ->
    "The file has a column whose physical type Rupam cannot read yet."
  | Contract_failure -> "The data-dict contract could not be loaded or validated."
  | Invalid_config ->
    "A `rupam.toml` key has an invalid value or the file is not valid TOML."
  | Missing_target ->
    "No target is set. Pass `--target` or set `[target] language` and `dialect` in \
     `rupam.toml`."
  | Stale_lock ->
    "An input, setting or dependency changed since the analysis was locked. Run `rupam \
     lock` and recompile."
  | Host_failure ->
    "The R or Python worker reported an error while running generated code."
  | Internal_error ->
    "Rupam itself failed. This is a bug in Rupam, not in the analysis; the session is \
     unchanged. Please report the input that caused it."
  | R_range ->
    "`a:b` selects columns from `a` to `b`. For a vector of integers write `a..=b`; to \
     count down, `a..=b |> rev()`."
;;

let position (p : Lexing.position) =
  Printf.sprintf
    "%s%d:%d"
    (if p.pos_fname = "" then "" else p.pos_fname ^ ":")
    p.pos_lnum
    (p.pos_cnum - p.pos_bol + 1)
;;

let render d =
  let location =
    match d.span with
    | Some (start, _) -> position start ^ "  "
    | None -> ""
  in
  location ^ name d.code ^ "\n  " ^ d.message
;;
