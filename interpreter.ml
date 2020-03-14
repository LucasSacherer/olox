open Ast
open Printf
open Reporting
open Value

(* interpreter helper functions *)
let check_unary_float (operator:Token.token) value =
  match value with
  | FloatValue fl -> Ok (fl)
  | _ -> Error ([{line = operator.line  ; where = ""; 
                  message = sprintf "Expected Float, got %s" (string_of_value value)}])

let check_binary_float (operator:Token.token) left_val right_val =
  match left_val, right_val with
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
let rec interpret_stmt stmt env =
  match stmt with
  | Statement stmt -> interpret_expression stmt.expr env
  | PrintStmt stmt -> begin
      match interpret_expression stmt.expr env with
      | Ok (value, new_env) -> print_endline (stringify value); Ok (NilValue, new_env)
      | Error err -> Error err
  end
  | VarStmt stmt -> begin
      let init_res = match stmt.init with
      | Some expr -> interpret_expression expr env
      | None -> Ok (NilValue, env)
      in
      match init_res with
      | Error err -> Error err
      | Ok (init_val, new_env) ->
          let mod_env = Environ.define new_env stmt.name.lexeme init_val in
          Ok (NilValue, mod_env)
  end
  | BlockStmt stmt_list ->
      let inner_env = Environ.push_env env in
      let res = List.fold_left
        (fun prev_res next_stmt ->
          match prev_res with
          | Error err -> Error err
          | Ok (_, prev_env) -> interpret_stmt next_stmt prev_env
        )
        (Ok (NilValue, inner_env))
        stmt_list 
      in
      match res with
      | Error err -> Error err
      | Ok (final_val, final_env) -> Ok (final_val, Environ.pop_env final_env)

and interpret_expression expr env =
  match expr with
  | Literal lit -> interpet_literal lit env
  | Grouping grp -> interpret_expression grp.expr env
  | Unary un -> interpret_unary un.operator un.right env
  | Binary bin -> interpret_binary bin.left bin.operator bin.right env
  | Variable var -> begin
      match Environ.get env var.name.lexeme with
      | Some value -> Ok (value, env)
      | None -> Error ([{line = var.name.line; where="";
                         message = sprintf "No variable called '%s'"
                                           var.name.lexeme}])
  end
  | Assign assi -> begin
    match interpret_expression assi.expr env with
    | Error err -> Error err
    | Ok (set_val, new_env) ->
    match Environ.assign new_env assi.name.lexeme set_val with
    | None -> Error ([{line = assi.name.line; where="";
                       message = sprintf "Can't assign to undeclared variable '%s'"
                       assi.name.lexeme}])
    | Some final_env -> Ok (set_val, final_env)
  end

and interpet_literal lit env =
  match lit with
  | StringLiteral str -> Ok (StringValue str, env)
  | FloatLiteral fl -> Ok (FloatValue fl, env)
  | BoolLiteral bl -> Ok (BoolValue bl, env)
  | NilLiteral -> Ok (NilValue, env)

and interpret_unary operator right_expr env =
  let interpret_helper value_res =
    let value, new_env = value_res in
    match operator.token_type with
    | Token.Minus -> (
        match value with
        | FloatValue fl -> Ok (FloatValue (Float.sub 0.0 fl), new_env)
        | _ -> Error ([{line = operator.line  ; where = ""; 
                        message = sprintf "Expected Float, got %s" 
                                          (string_of_value value)}])
    )
    | Token.Bang ->
        let b_value = is_truthy value in
        Ok (BoolValue (Bool.not b_value), new_env)
    | _ -> Error ([{line = operator.line; where = ""; 
                    message = sprintf "Unexpected unary operator: %s" operator.lexeme}])
  in
  let right_value_res = interpret_expression right_expr env in
  Result.bind right_value_res interpret_helper

and interpret_binary left_expr operator right_expr env =
  let interpret_helper left_val right_val env=
    match operator.token_type with
    | Token.Greater -> interpret_binary_float_log tuple_greater operator left_val right_val env
    | Token.GreaterEqual -> interpret_binary_float_log tuple_greater_eq operator left_val right_val env
    | Token.Less -> interpret_binary_float_log tuple_less operator left_val right_val env
    | Token.LessEqual -> interpret_binary_float_log tuple_less_eq operator left_val right_val env
    | Token.Minus -> interpret_binary_float_arith tuple_minus operator left_val right_val env
    | Token.Slash -> interpret_binary_float_arith tuple_div operator left_val right_val env
    | Token.Star -> interpret_binary_float_arith tuple_mult operator left_val right_val env
    | Token.EqualEqual -> interpret_binary_equal false (*is_n_eq*) left_val right_val env
    | Token.BangEqual -> interpret_binary_equal true (*is_n_eq*) left_val right_val env
    | Token.Plus -> interpret_binary_plus operator left_val right_val env
    | _ -> Error ([{line = operator.line; where = "";
                    message = sprintf "Unexpected binary operator: %s" operator.lexeme}])
  in
  match interpret_expression left_expr env with
  | Error err -> Error err
  | Ok (left_val, left_env) ->
  match interpret_expression right_expr left_env with
  | Error err -> Error err
  | Ok (right_val, right_env) ->
  interpret_helper left_val right_val right_env

(* TODO: Find a way to get the two following functions into one *)
and interpret_binary_float_arith func operator left_val right_val env =
  let float_res = check_binary_float operator left_val right_val in
  let result_res = Result.map func float_res in
  Result.map (fun fl -> (FloatValue fl, env)) result_res

and interpret_binary_float_log func operator left_val right_val env =
  (*let tuple_res = combine_results left_res right_res in*)
  let float_res = check_binary_float operator left_val right_val in
  let result_res = Result.map func float_res in
  Result.map (fun fl -> (BoolValue fl, env)) result_res

and interpret_binary_equal is_n_eq left_val right_val env =
  let is_eq_res = tuple_eq (left_val, right_val) in
  Ok (BoolValue (if is_n_eq then Bool.not is_eq_res else is_eq_res), env)

and interpret_binary_plus operator left_val right_val env =
  match (left_val, right_val) with
  | FloatValue left_fl, FloatValue right_fl -> Ok (FloatValue (tuple_add (left_fl, right_fl)), env)
  | StringValue left_str, StringValue right_str -> Ok (StringValue (string_add (left_str, right_str)), env)
  | _ as left_val, right_val -> 
      Error ([{line = operator.line; where = "";
               message = sprintf "Expected either strings or floats, got: %s + %s"
               (string_of_value left_val) (string_of_value right_val)}])

let rec interpret env stmt_list =
  match stmt_list with
  | [] -> Ok (NilValue, env)
  | stmt::rest ->
      match interpret_stmt stmt env with
      | Error err -> Error (err)
      | Ok (value, new_env) ->
          if List.length rest == 0 then Ok (value, new_env) else interpret new_env rest
