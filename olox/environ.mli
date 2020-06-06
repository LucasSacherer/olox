(* Types that build up the environment *)

(** Imutable representation of the environment state *)
type environ

(** Global environment, usualy stored in an [environ] *)
type global_env

(** The diffierent values that can be stored in an environment *)
type value =
  | StringValue of string
  | FloatValue of float
  | BoolValue of bool
  | FunctionValue of
      { arity: int
      ; to_call:
             value list
          -> global_env
          -> (value * global_env, Reporting.error_record list) result
      ; name: string }
  | ClassValue of {name: string}
  | ReturnValue of value * int
      (** This value is used internaly to implement return in functions.
          The [int] represents the line number where the return was called.*)
  | NilValue

(* Funcitons on values *)
val string_of_value : value -> string

val stringify : value -> string

(* Functions on an environ *)
val create_environ : unit -> environ

val from_global : global_env -> environ

val define : environ -> string -> value -> environ

val assign : environ -> string -> value -> environ option

val get : environ -> string -> value option

val contains : environ -> string -> bool

val push_env : environ -> environ

val pop_env : environ -> environ

val get_global : environ -> global_env

val apply_global : environ -> global_env -> environ
