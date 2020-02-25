open Ast
open Token
open Reporting
open Printf

exception Parser_Error of string * (token list)

let parse_left_binary operators to_call =
  let rec parse_aux tokens expr =
    match tokens with
    | [] -> (expr, tokens)
    | head::rest ->
    match head.token_type with
    | _ as t_type when List.exists (fun item -> t_type == item) operators ->
      let second, rest = to_call rest in
      let new_expr = Binary {left = expr; operator = head; right = second} in
      parse_aux rest new_expr
    | _ -> (expr, tokens)
  in
  parse_aux

let parse_gen_stmt to_call =
  let parse_stmt_aux expr tokens =
    match tokens with
    | [] -> raise (Parser_Error 
                    ("Missing semicolon after value!", tokens))
    | head::rest ->
    match head.token_type with
    | Semicolon -> to_call expr rest
    | _ -> raise (Parser_Error
                   ("Expected semicolon after value!", tokens))
  in parse_stmt_aux

let rec parse_stmt tokens =
  match tokens with
  | [] -> raise (Parser_Error ("Tried to parse empty statement!", tokens))
  | head::rest ->
  match head.token_type with
  | Print -> parse_print_stmt rest
  | _ -> parse_expr_stmt tokens

and parse_print_stmt tokens =
  let new_expr, rest = parse_expression tokens in
  (parse_gen_stmt (fun expr toks -> (PrintExpr {expr}, toks))) new_expr rest

and parse_expr_stmt tokens =
  let new_expr, rest = parse_expression tokens in
  (parse_gen_stmt (fun expr toks -> (Expression {expr}, toks))) new_expr rest

and parse_expression tokens = parse_equality tokens

and parse_equality tokens =
  let first, rest = parse_comparison tokens in
  (parse_left_binary [BangEqual; EqualEqual] parse_comparison) rest first

and parse_comparison tokens =
  let first, rest = parse_addition tokens in
  (parse_left_binary [Greater; GreaterEqual; Less; LessEqual] parse_addition)
    rest first

and parse_addition tokens =
  let first, rest = parse_multiplication tokens in
  (parse_left_binary [Minus; Plus] parse_multiplication) rest first

and parse_multiplication tokens =
  let first, rest = parse_unary tokens in
  (parse_left_binary [Slash; Star] parse_unary) rest first

and parse_unary tokens =
  match tokens with
  | [] -> parse_primary tokens
  | head::rest ->
  match head.token_type with
  | Bang | Minus ->
      let right, rest = parse_unary rest in
      (Unary {operator = head; right}, rest)
  | _ -> parse_primary tokens

and parse_primary tokens =
  match tokens with
  | [] -> raise (Parser_Error 
                  ("Reached end before finishing expression!", tokens))
  | head::rest ->
  match head.token_type with
  | EOF -> raise (Parser_Error 
                   ("Reached end before finishing expression!", tokens))
  | False -> (Literal (BoolLiteral (false)), rest)
  | True -> (Literal (BoolLiteral (true)), rest)
  | Nil -> (Literal (NilLiteral), rest)
  | Number fl -> (Literal (FloatLiteral (fl)), rest)
  | String str -> (Literal (StringLiteral (str)), rest)
  | Identifier id -> (Literal (IdentLiteral (id)), rest)
  | LeftParen ->
      (
      let next_expr, rest = parse_expression rest in
      match rest with
      | [] -> raise (Parser_Error ("Expected expression after '('", rest))
      | head::rest ->
      match head.token_type with
      | RightParen -> (Grouping {expr = next_expr}, rest)
      | _ -> raise (Parser_Error ("Expected ')' after expression", rest))
      )
  | _ -> raise (Parser_Error ("Expected literal, ident, or group", tokens))

let rec parse prev_stmts prev_errors tokens = 
  match tokens with
  | [] -> if List.length prev_errors > 0 then Error (List.rev prev_errors) 
                                         else Ok (List.rev prev_stmts)
  | head::_ ->
  match head.token_type with
  | EOF -> parse prev_stmts prev_errors []
  | _ ->
    try
      let next_stmt, rest_tokens = parse_stmt tokens in
      parse (next_stmt::prev_stmts) prev_errors rest_tokens 
    with
      Parser_Error (message,tokens) -> 
        match tokens with
        | [] -> parse prev_stmts ({line = -1; where = " at end"; message}::prev_errors) []
        | head::rest ->
        match head.token_type with
        | EOF -> parse prev_stmts ({line = head.line; where = " at end"; message}::prev_errors) []
        | _ -> parse prev_stmts ({line = head.line; where = sprintf " at '%s'" head.lexeme;
                   message}::prev_errors) rest
