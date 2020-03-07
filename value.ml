open Printf

(* value type description *)
type value =
  | StringValue of string
  | FloatValue of float
  | BoolValue of bool
  | NilValue

let string_of_value = function
  | StringValue str -> sprintf "String: '%s'" str
  | FloatValue fl -> sprintf "Float: %f" fl
  | BoolValue bl -> sprintf "Bool: %b" bl
  | NilValue -> "Nil"

let stringify = function
  | StringValue str -> str
  | FloatValue fl -> sprintf "%f" fl
  | BoolValue bl -> sprintf "%b" bl
  | NilValue -> "Nil"

