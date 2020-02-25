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

let stringify = function
  | StringValue str -> str
  | FloatValue fl -> sprintf "%f" fl
  | BoolValue bl -> sprintf "%b" bl
  | NilValue -> "Nil"

(* interpreter helper functions *)
let check_unary_float (operator:Token.token) value =
  match value with
  | FloatValue fl -> Ok (fl)
  | _ -> Error ([{line = operator.line  ; where = ""; 
                  message = sprintf "Expected Float, got %s" (string_of_value value)}])

let check_binary_float (operator:Token.token) val_tup =
  match val_tup with
  | FloatValue left_fl, FloatValue right_fl -> Ok (left_fl, right_fl)
  | _ as left_val, right_val -> 
      Error ([{line = operator.line  ; where = sprintf " right of '%s'" operator.lexeme; 
      message = sprintf "Expected Floats, got %s and %s" (string_of_value left_val) (string_of_value right_val)}])

let tuple_operation tuple_op tuple =
  let left, right = tuple in
  tuple_op left right
let tuple_minus = tuple_operation Float.sub
let tuple_div = tuple_operation Float.div
let tuple_mult = tuple_operation Float.mul
let tuple_add = tuple_operation Float.add
let tuple_greater = tuple_operation (>)
let tuple_greater_eq = tuple_operation (>=)
let tuple_less = tuple_operation (<)
let tuple_less_eq = tuple_operation (<=)

let string_add tuple =
  let left_str, right_str = tuple in
  sprintf "%s%s" left_str right_str

let tuple_eq tuple =
  match tuple with
  | NilValue, NilValue | NilValue, _ -> false
  | _ as left, right -> left = right

let combine_results left_res right_res =
  match left_res with
  | Error err -> Error err
  | Ok left_val ->
  match right_res with
  | Error err -> Error err
  | Ok right_val -> Ok (left_val, right_val)

let is_truthy = function
  | BoolValue bl -> bl
  | NilValue -> false
  | _ -> true

(* interpreter *)
let rec interpret_stmt = function
  | Expression exp -> interpret_expression exp.expr
  | PrintExpr exp ->
      match interpret_expression exp.expr with
      | Ok value -> print_endline (stringify value); Ok (NilValue)
      | Error err -> Error err
and interpret_expression = function
  | Literal lit -> interpet_literal lit
  | Grouping grp -> interpret_expression grp.expr
  | Unary un -> interpret_unary un.operator un.right
  | Binary bin -> interpret_binary bin.left bin.operator bin.right

and interpet_literal = function
  | StringLiteral str -> Ok (StringValue str)
  | FloatLiteral fl -> Ok (FloatValue fl)
  | BoolLiteral bl -> Ok (BoolValue bl)
  | IdentLiteral id -> Ok (StringValue id) (* This is temporary, need to do a lookup here *)
  | NilLiteral -> Ok (NilValue)

and interpret_unary operator right_expr =
  let interpret_helper value_res =
    match operator.token_type with
    | Token.Minus ->
        let float_res = Result.bind value_res (check_unary_float operator) in
        Result.map (fun fl -> FloatValue (Float.sub 0.0 fl)) float_res
    | Token.Bang ->
        let bool_res = Result.map is_truthy value_res in
        Result.map (fun bl -> BoolValue (Bool.not bl)) bool_res
    | _ -> Error ([{line = operator.line; where = ""; 
                    message = sprintf "Unexpected unary operator: %s" operator.lexeme}])
  in
  let right_value = interpret_expression right_expr in
  interpret_helper right_value

and interpret_binary left_expr operator right_expr =
  let interpret_helper left_res right_res =
    match operator.token_type with
    | Token.Greater -> interpret_binary_float_log tuple_greater operator left_res right_res
    | Token.GreaterEqual -> interpret_binary_float_log tuple_greater_eq operator left_res right_res
    | Token.Less -> interpret_binary_float_log tuple_less operator left_res right_res
    | Token.LessEqual -> interpret_binary_float_log tuple_less_eq operator left_res right_res
    | Token.Minus -> interpret_binary_float_arith tuple_minus operator left_res right_res
    | Token.Slash -> interpret_binary_float_arith tuple_div operator left_res right_res
    | Token.Star -> interpret_binary_float_arith tuple_mult operator left_res right_res
    | Token.EqualEqual -> interpret_binary_equal false (*is_n_eq*) left_res right_res
    | Token.BangEqual -> interpret_binary_equal true (*is_n_eq*) left_res right_res
    | Token.Plus -> interpret_binary_plus operator left_res right_res
    | _ -> Error ([{line = operator.line; where = "";
                    message = sprintf "Unexpected binary operator: %s" operator.lexeme}])
  in
  let left_value = interpret_expression left_expr in
  let right_value = interpret_expression right_expr in
  interpret_helper left_value right_value

(* TODO: Find a way to get the two following functions into one *)
and interpret_binary_float_arith func operator left_res right_res =
  let tuple_res = combine_results left_res right_res in
  let float_res = Result.bind tuple_res (check_binary_float operator) in
  let result_res = Result.map func float_res in
  Result.map (fun fl -> FloatValue fl) result_res

and interpret_binary_float_log func operator left_res right_res =
  let tuple_res = combine_results left_res right_res in
  let float_res = Result.bind tuple_res (check_binary_float operator) in
  let result_res = Result.map func float_res in
  Result.map (fun fl -> BoolValue fl) result_res

and interpret_binary_equal is_n_eq left_res right_res =
  let tuple_res = combine_results left_res right_res in
  let is_eq_res = Result.map tuple_eq tuple_res in
  Result.map (fun is_eq -> BoolValue (if is_n_eq then Bool.not is_eq else is_eq)) is_eq_res

and interpret_binary_plus operator left_res right_res =
  let tuple_res = combine_results left_res right_res in
  if Result.is_error tuple_res then Error (Result.get_error tuple_res)
  else
  let val_tuple = Result.get_ok tuple_res in
  match val_tuple with
  | FloatValue left_fl, FloatValue right_fl -> Ok (FloatValue (tuple_add (left_fl, right_fl)))
  | StringValue left_str, StringValue right_str -> Ok (StringValue (string_add (left_str, right_str)))
  | _ as left_val, right_val -> 
      Error ([{line = operator.line; where = "";
               message = sprintf "Expected either strings or floats, got: %s + %s"
               (string_of_value left_val) (string_of_value right_val)}])

let rec interpret stmt_list =
  match stmt_list with
  | [] -> Ok (NilValue)
  | stmt::rest ->
      match interpret_stmt stmt with
      | Error err -> Error (err)
      | Ok value ->
          if List.length rest == 0 then Ok value else interpret rest
