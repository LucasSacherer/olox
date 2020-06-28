(* Types that build up the environment *)

(** Imutable representation of the environment state *)
type environ

(** Global environment, usualy stored in an [environ] *)
type global_env

(** Imutable representation of a class' environment.*)
type class_inst

(** The diffierent values that can be stored in an environment *)
type value =
  | StringValue of string
  | FloatValue of float
  | BoolValue of bool
  | FunctionValue of func_desc * class_inst option
  | ClassValue of class_desc
  | ClassInstance of class_inst
  | ReturnValue of value * int
      (** This value is used internaly to implement return in functions.
          The [int] represents the line number where the return was called.*)
  | NilValue

and class_desc =
  {class_name: string; superclass: class_desc option; methods: func_desc list}

and func_desc =
  { arity: int
  ; mutable to_call:
         value list
      -> global_env
      -> class_inst option
      -> (value * global_env, Reporting.error_record list) result
  ; func_name: string }

(* Funcitons on values *)
val string_of_value : value -> string

val stringify : value -> string

(* Functions on class instances or descriptions *)
val create_class_inst : class_desc -> value

val get_property : class_inst -> string -> value option

val set_property : class_inst -> string -> value -> unit

val get_init_arity : class_desc -> int

val get_init_func : class_inst -> value option

val get_method : class_desc -> string -> func_desc option

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
