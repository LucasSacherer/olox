type environ

val create_environ : unit -> environ
val define : environ -> string -> Value.value -> environ
val get : environ -> string -> Value.value option
val contains : environ -> string -> bool
