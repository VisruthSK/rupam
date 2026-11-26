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

val all : code list

type t =
  { code : code
  ; span : span option
  ; message : string
  }

exception Error of t

val fail : ?span:span -> code -> string -> 'a
val of_exn : exn -> t
val locate : span -> (unit -> 'a) -> 'a
val name : code -> string
val of_name : string -> code option
val explanation : code -> string
val render : t -> string
