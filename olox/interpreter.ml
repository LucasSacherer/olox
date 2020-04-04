open Ast
open Printf
open Reporting
open Environ

(* interpreter helper functions *)
let check_binary_float (operator : Token.token) left_val right_val =
  match (left_val, right_val) with
  | FloatValue left_fl, FloatValue right_fl ->
      Ok (left_fl, right_fl)
  | (_ as left_val), right_val ->
      Error
        [ { line= operator.line
          ; where= sprintf " right of '%s'" operator.lexeme
          ; message=
              sprintf "Expected Floats, got %s and %s"
                (string_of_value left_val)
                (string_of_value right_val) } ]

let tuple_operation tuple_op tuple =
  let left, right = tuple in
  tuple_op left right

let tuple_minus = tuple_operation Float.sub

let tuple_div = tuple_operation Float.div

let tuple_mult = tuple_operation Float.mul

let tuple_add = tuple_operation Float.add

let tuple_greater = tuple_operation ( > )

let tuple_greater_eq = tuple_operation ( >= )

let tuple_less = tuple_operation ( < )

let tuple_less_eq = tuple_operation ( <= )

let string_add tuple =
  let left_str, right_str = tuple in
  sprintf "%s%s" left_str right_str

let tuple_eq tuple =
  match tuple with
  | NilValue, NilValue | NilValue, _ ->
      false
  | (_ as left), right ->
      left = right

let is_truthy = function BoolValue bl -> bl | NilValue -> false | _ -> true

(* interpreter *)
let rec interpret_stmt stmt env =
  match stmt with
  | Statement stmt ->
      interpret_expression stmt.expr env
  | PrintStmt stmt -> (
    match interpret_expression stmt.expr env with
    | Ok (value, new_env) ->
        print_endline (stringify value) ;
        Ok (NilValue, new_env)
    | Error err ->
        Error err )
  | VarStmt stmt -> (
      let init_res =
        match stmt.init with
        | Some expr ->
            interpret_expression expr env
        | None ->
            Ok (NilValue, env)
      in
      match init_res with
      | Error err ->
          Error err
      | Ok (init_val, new_env) ->
          let mod_env = Environ.define new_env stmt.name.lexeme init_val in
          Ok (NilValue, mod_env) )
  | BlockStmt stmt_list -> (
      let inner_env = Environ.push_env env in
      let res =
        List.fold_left
          (fun prev_res next_stmt ->
            match prev_res with
            | Error err ->
                Error err
            | Ok (_, prev_env) ->
                interpret_stmt next_stmt prev_env)
          (Ok (NilValue, inner_env))
          stmt_list
      in
      match res with
      | Error err ->
          Error err
      | Ok (final_val, final_env) ->
          Ok (final_val, Environ.pop_env final_env) )
  | IfStmt stmt -> (
    match interpret_expression stmt.condition env with
    | Error err ->
        Error err
    | Ok (cond_value, cond_env) -> (
        if is_truthy cond_value then interpret_stmt stmt.then_branch cond_env
        else
          match stmt.else_branch with
          | Some else_stmt ->
              interpret_stmt else_stmt cond_env
          | None ->
              Ok (NilValue, cond_env) ) )
  | WhileStmt stmt ->
      interpret_while stmt.condition stmt.body env
  | FuncStmt stmt ->
      interpret_func stmt.name stmt.params stmt.body env

and interpret_func name params body env =
  let function_val =
    FunctionValue
      { arity= List.length params
      ; name= name.lexeme
      ; to_call= gen_func_to_call params body }
  in
  let new_env = Environ.define env name.lexeme function_val in
  Ok (NilValue, new_env)

and gen_func_to_call params body =
  let string_params = List.map (fun param -> param.Token.lexeme) params in
  let func_to_call args global_env =
    let new_env = Environ.from_global global_env in
    let local_env = Environ.push_env new_env in
    let call_env =
      List.fold_left2
        (fun prev_env param arg -> Environ.define prev_env param arg)
        local_env string_params args
    in
    let env_res = interpret_stmt body call_env in
    match env_res with
    | Error err ->
        Error err
    | Ok (final_val, final_env) ->
        Ok (final_val, Environ.get_global final_env)
  in
  func_to_call

and interpret_while condition body env =
  let cond_res = interpret_expression condition env in
  match cond_res with
  | Error err ->
      Error err
  | Ok (cond_val, cond_env) -> (
    match is_truthy cond_val with
    | false ->
        Ok (NilValue, cond_env)
    | true -> (
        let body_res = interpret_stmt body cond_env in
        match body_res with
        | Error err ->
            Error err
        | Ok (_, body_env) ->
            interpret_while condition body body_env ) )

and interpret_expression expr env =
  match expr with
  | Literal lit ->
      interpet_literal lit env
  | Grouping grp ->
      interpret_expression grp.expr env
  | Unary un ->
      interpret_unary un.operator un.right env
  | Binary bin ->
      interpret_binary bin.left bin.operator bin.right env
  | Variable var -> (
    match Environ.get env var.name.lexeme with
    | Some value ->
        Ok (value, env)
    | None ->
        Error
          [ { line= var.name.line
            ; where= ""
            ; message= sprintf "No variable called '%s'" var.name.lexeme } ] )
  | Assign assi -> (
    match interpret_expression assi.expr env with
    | Error err ->
        Error err
    | Ok (set_val, new_env) -> (
      match Environ.assign new_env assi.name.lexeme set_val with
      | None ->
          Error
            [ { line= assi.name.line
              ; where= ""
              ; message=
                  sprintf "Can't assign to undeclared variable '%s'"
                    assi.name.lexeme } ]
      | Some final_env ->
          Ok (set_val, final_env) ) )
  | Logical log ->
      interpret_logical log.operator log.left log.right env
  | Call call ->
      interpret_call call.callee call.paren call.arguments env

and interpet_literal lit env =
  match lit with
  | StringLiteral str ->
      Ok (StringValue str, env)
  | FloatLiteral fl ->
      Ok (FloatValue fl, env)
  | BoolLiteral bl ->
      Ok (BoolValue bl, env)
  | NilLiteral ->
      Ok (NilValue, env)

and interpret_unary operator right_expr env =
  let interpret_helper value_res =
    let value, new_env = value_res in
    match operator.token_type with
    | Token.Minus -> (
      match value with
      | FloatValue fl ->
          Ok (FloatValue (Float.sub 0.0 fl), new_env)
      | _ ->
          Error
            [ { line= operator.line
              ; where= ""
              ; message=
                  sprintf "Expected Float, got %s" (string_of_value value) } ] )
    | Token.Bang ->
        let b_value = is_truthy value in
        Ok (BoolValue (Bool.not b_value), new_env)
    | _ ->
        Error
          [ { line= operator.line
            ; where= ""
            ; message= sprintf "Unexpected unary operator: %s" operator.lexeme
            } ]
  in
  let right_value_res = interpret_expression right_expr env in
  Result.bind right_value_res interpret_helper

and interpret_binary left_expr operator right_expr env =
  let interpret_helper left_val right_val env =
    match operator.token_type with
    | Token.Greater ->
        interpret_binary_float_log tuple_greater operator left_val right_val env
    | Token.GreaterEqual ->
        interpret_binary_float_log tuple_greater_eq operator left_val right_val
          env
    | Token.Less ->
        interpret_binary_float_log tuple_less operator left_val right_val env
    | Token.LessEqual ->
        interpret_binary_float_log tuple_less_eq operator left_val right_val env
    | Token.Minus ->
        interpret_binary_float_arith tuple_minus operator left_val right_val env
    | Token.Slash ->
        interpret_binary_float_arith tuple_div operator left_val right_val env
    | Token.Star ->
        interpret_binary_float_arith tuple_mult operator left_val right_val env
    | Token.EqualEqual ->
        interpret_binary_equal false (*is_n_eq*) left_val right_val env
    | Token.BangEqual ->
        interpret_binary_equal true (*is_n_eq*) left_val right_val env
    | Token.Plus ->
        interpret_binary_plus operator left_val right_val env
    | _ ->
        Error
          [ { line= operator.line
            ; where= ""
            ; message= sprintf "Unexpected binary operator: %s" operator.lexeme
            } ]
  in
  match interpret_expression left_expr env with
  | Error err ->
      Error err
  | Ok (left_val, left_env) -> (
    match interpret_expression right_expr left_env with
    | Error err ->
        Error err
    | Ok (right_val, right_env) ->
        interpret_helper left_val right_val right_env )

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
  | FloatValue left_fl, FloatValue right_fl ->
      Ok (FloatValue (tuple_add (left_fl, right_fl)), env)
  | StringValue left_str, StringValue right_str ->
      Ok (StringValue (string_add (left_str, right_str)), env)
  | (_ as left_val), right_val ->
      Error
        [ { line= operator.line
          ; where= ""
          ; message=
              sprintf "Expected either strings or floats, got: %s + %s"
                (string_of_value left_val)
                (string_of_value right_val) } ]

and interpret_logical operator left_exp right_expr env =
  let left_res = interpret_expression left_exp env in
  match left_res with
  | Error err ->
      Error err
  | Ok (left_val, left_env) -> (
    match (operator.token_type, is_truthy left_val) with
    | Or, true | And, false ->
        Ok (left_val, left_env)
    | _ ->
        interpret_expression right_expr left_env )

and interpret_call callee left_paren arguments env =
  match interpret_expression callee env with
  | Error err ->
      Error err
  | Ok (callee_val, callee_env) -> (
      let arguments_res =
        List.fold_left
          (fun prev_res next_expr ->
            match prev_res with
            | Error error ->
                Error error
            | Ok (val_acc, prev_env) -> (
                let next_res = interpret_expression next_expr prev_env in
                match next_res with
                | Error error ->
                    Error error
                | Ok (next_val, next_env) ->
                    Ok (next_val :: val_acc, next_env) ))
          (Ok ([], callee_env))
          arguments
      in
      match arguments_res with
      | Error error ->
          Error error
      | Ok (rev_args, args_env) -> (
          let call_args = List.rev rev_args in
          match callee_val with
          | FunctionValue func -> (
              if func.arity != List.length call_args then
                Error
                  [ { line= left_paren.line
                    ; where= ""
                    ; message=
                        sprintf "Expected %i arguments, got %i" func.arity
                          (List.length call_args) } ]
              else
                match func.to_call call_args (get_global args_env) with
                | Error error ->
                    Error error
                | Ok (call_val, new_global) ->
                    Ok (call_val, apply_global args_env new_global) )
          | _ ->
              Error
                [ { line= left_paren.line
                  ; where= ""
                  ; message=
                      sprintf "Can only call functions and classes, got %s"
                        (string_of_value callee_val) } ] ) )

let rec interpret env stmt_list =
  match stmt_list with
  | [] ->
      Ok (NilValue, env)
  | stmt :: rest -> (
    match interpret_stmt stmt env with
    | Error err ->
        Error err
    | Ok (value, new_env) ->
        if List.length rest == 0 then Ok (value, new_env)
        else interpret new_env rest )