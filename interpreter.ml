open Ast
open Printf
open Reporting

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

(* interpreter helper functions *)
let check_number_operator (operator:Token.token) value =
  match value with
  | FloatValue fl -> Ok (fl)
  | _ -> Error ([{line = operator.line  ; where = ""; 
                  message = sprintf "Expected Float, got %s" (string_of_value value)}])

let check_bool_operator (operator:Token.token) value =
  match value with
  | BoolValue bl -> Ok (bl)
  | _ -> Error ([{line = operator.line  ; where = ""; 
                  message = sprintf "Expected Bool, got %s" (string_of_value value)}])

(* interpreter *)
let rec interpret_expression = function
  | Literal lit -> interpet_literal lit
  | Grouping grp -> interpret_expression grp.expr
  | Unary un -> interpret_unary un.operator un.right

and interpet_literal = function
  | StringLiteral str -> Ok (StringValue str)
  | FloatLiteral fl -> Ok (FloatValue fl)
  | BoolLiteral bl -> Ok (BoolValue bl)
  | IdentLiteral id -> Ok (StringValue id) (* This is temporary, need to do a lookup here *)
  | NilLiteral -> Ok (NilValue)

and interpret_unary operator right_expression =
  let interpret_helper value_res =
    match operator.token_type with
    | Token.Minus ->
        let float_res = Result.bind value_res (check_number_operator operator) in
        Result.map (fun fl -> FloatValue (Float.sub 0.0 fl)) float_res
    | Token.Bang ->
        (* TODO: check if truthy here *)
        let bool_res = Result.bind value_res (check_bool_operator operator) in
        Result.map (fun bl -> BoolValue (Bool.not bl)) bool_res
    | _ -> Error ([{line = operator.line; where = ""; 
                    message = sprintf "Unexpected unary operator: %s" operator.lexeme}])
  in
  let right_value = interpret_expression right_expression in
  interpret_helper right_value
