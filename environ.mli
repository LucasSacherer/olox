type environ

val create_environ : unit -> environ

val define : environ -> string -> Value.value -> environ

val assign : environ -> string -> Value.value -> environ option

val get : environ -> string -> Value.value option

val contains : environ -> string -> bool

val push_env : environ -> environ

val pop_env : environ -> environ
