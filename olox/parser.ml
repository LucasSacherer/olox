open Ast
open Token
open Reporting
open Printf

exception Parser_Error of string * token list

(* Helper functions for parsing *)

(* Generates a function that parses a left associative operator. operators is the list
 * of token_types at this precedence level and to_call is the function to call to parse
 * the next level of precedence. This function keeps parsing as long as it finds operators
 * that match its operator list and uses to_call to parse the right hand side of any
 * operation. In practice (as long as the function is used correctly) this allows a single
 * top level call to the generated function to parse an arbitrarily large arithmatic/logic
 * expression. See usage bellow for an example. *)
let parse_left_binary operators to_call =
  let rec parse_aux tokens expr =
    match tokens with
    | [] ->
        (expr, tokens)
    | head :: rest -> (
      match head.token_type with
      | _ as t_type when List.exists (fun item -> t_type == item) operators ->
          let second, rest = to_call rest in
          let new_expr = Binary {left= expr; operator= head; right= second} in
          parse_aux rest new_expr
      | _ ->
          (expr, tokens) )
  in
  parse_aux

(* Parses a single token of a given token_type. Returns the token and the rest of the toke
 * list. Throws an error if the list is empty or if the first token in the list is not of
 * the correct type. *)
let parse_one_token token_type message tokens =
  match tokens with
  | [] ->
      raise (Parser_Error (message, []))
  | head :: rest ->
      if head.token_type == token_type then (head, rest)
      else raise (Parser_Error (message, rest))

(* Creates a fuction that makes sure that the next token is a semicolon.
 * If it is, make the callback. This is used at the end of parsing statements. *)
let parse_gen_stmt to_call =
  let parse_stmt_aux expr tokens =
    match tokens with
    | [] ->
        raise (Parser_Error ("Missing semicolon after value!", tokens))
    | head :: rest -> (
      match head.token_type with
      | Semicolon ->
          to_call expr rest
      | _ ->
          raise (Parser_Error ("Expected semicolon after value!", tokens)) )
  in
  parse_stmt_aux

(* Desugars a for loop into a block statement containing a while loop. *)
let generate_for_loop init_stmt_opt cond_expr_opt inc_expr_opt body_stmt =
  let final_body =
    match inc_expr_opt with
    | None ->
        body_stmt
    | Some inc_expr ->
        BlockStmt [body_stmt; Statement {expr= inc_expr}]
  in
  let while_stmt =
    match cond_expr_opt with
    | None ->
        WhileStmt {condition= Literal (BoolLiteral true); body= final_body}
    | Some cond_expr ->
        WhileStmt {condition= cond_expr; body= final_body}
  in
  let final_stmt =
    match init_stmt_opt with
    | None ->
        while_stmt
    | Some init_stmt ->
        BlockStmt [init_stmt; while_stmt]
  in
  final_stmt

(**************************************
 * Start of the parser.
 **************************************)

type func_type = Function (*| Method*)

let rec parse_decl tokens =
  match tokens with
  | [] ->
      raise (Parser_Error ("Tried to parse empty decleration!", tokens))
  | head :: rest -> (
    match head.token_type with
    | Var ->
        parse_var_decl rest
    | Fun ->
        parse_function rest Function
    | _ ->
        parse_stmt tokens )

and parse_function tokens _ (* func_type *) =
  let name, rest =
    parse_one_token Identifier "Expected name to start function decleration!"
      tokens
  in
  let rec parse_params tokens acc =
    match tokens with
    | [] ->
        raise (Parser_Error ("Expected ')' after paramters!", []))
    | head :: rest -> (
      match head.token_type with
      | RightParen ->
          (List.rev acc, rest)
      | Identifier -> (
          let new_params = head :: acc in
          match rest with
          | [] ->
              raise
                (Parser_Error ("Expected ')' or ',' in parameter list!", []))
          | head :: other_rest -> (
            match head.token_type with
            | Comma ->
                parse_params other_rest new_params
            | RightParen ->
                parse_params rest new_params
            | _ ->
                raise
                  (Parser_Error
                     ("Expected ')' or ',' in paramater list!", other_rest)) ) )
      | _ ->
          raise (Parser_Error ("Expected ')' or another parameter name!", rest))
      )
  in
  let _, rest =
    parse_one_token LeftParen "Expected '(' after function name!" rest
  in
  let params, rest = parse_params rest [] in
  let _, rest =
    parse_one_token LeftBrace "Expected '{' to start function body!" rest
  in
  let body, remain_tokens = parse_block_stmt rest in
  (FuncStmt {name; params; body}, remain_tokens)

and parse_var_decl tokens =
  let name, rest =
    parse_one_token Identifier "Expected identifier after var!" tokens
  in
  match rest with
  | [] ->
      raise (Parser_Error ("Expected a ';' or an initializer!", rest))
  | head :: rest -> (
      let init, rest =
        match head.token_type with
        | Equal ->
            let expr, expr_rest = parse_expression rest in
            (Some expr, expr_rest)
        | Semicolon ->
            (None, rest)
        | _ ->
            raise (Parser_Error ("Expected ';' or '=' after value!", rest))
      in
      match init with
      | None ->
          (VarStmt {name; init= None}, rest)
      | _ ->
          let _, rest =
            parse_one_token Semicolon "Expected ';' after value!" rest
          in
          (VarStmt {name; init}, rest) )

and parse_stmt tokens =
  match tokens with
  | [] ->
      raise (Parser_Error ("Tried to parse empty statement!", tokens))
  | head :: rest -> (
    match head.token_type with
    | Print ->
        parse_print_stmt rest
    | LeftBrace ->
        parse_block_stmt rest
    | If ->
        parse_if_stmt rest
    | While ->
        parse_while_stmt rest
    | For ->
        parse_for_stmt rest
    | Return ->
        parse_return_stmt head rest
    | _ ->
        parse_expr_stmt tokens )

and parse_print_stmt tokens =
  let new_expr, rest = parse_expression tokens in
  (parse_gen_stmt (fun expr toks -> (PrintStmt {expr}, toks))) new_expr rest

and parse_block_stmt tokens =
  let rec parse_next_stmt tokens acc =
    match tokens with
    | [] ->
        raise (Parser_Error ("Reached EOF without closing block!", []))
    | head :: rest -> (
      match head.token_type with
      | RightBrace ->
          (BlockStmt (List.rev acc), rest)
      | _ ->
          let next_stmt, next_toks = parse_decl tokens in
          parse_next_stmt next_toks (next_stmt :: acc) )
  in
  parse_next_stmt tokens []

and parse_if_stmt tokens =
  let _, rest = parse_one_token LeftParen "Expected '(' after 'if'!" tokens in
  let condition, rest = parse_expression rest in
  let _, rest =
    parse_one_token RightParen "Expected ')' after 'if' condition!" rest
  in
  let then_branch, tokens_after_then = parse_stmt rest in
  match tokens_after_then with
  | [] ->
      (IfStmt {condition; then_branch; else_branch= None}, [])
  | head :: rest -> (
    match head.token_type with
    | Else ->
        let else_branch, tokens_after_else = parse_stmt rest in
        ( IfStmt {condition; then_branch; else_branch= Some else_branch}
        , tokens_after_else )
    | _ ->
        (IfStmt {condition; then_branch; else_branch= None}, tokens_after_then)
    )

and parse_while_stmt tokens =
  let _, rest = parse_one_token LeftParen "Expect '(' after 'while'!" tokens in
  let condition, rest = parse_expression rest in
  let _, rest =
    parse_one_token RightParen "Expect ')' after 'while' condition!" rest
  in
  let body, rest = parse_stmt rest in
  (WhileStmt {condition; body}, rest)

and parse_for_stmt tokens =
  let _, rest = parse_one_token LeftParen "Expected '(' after 'for'!" tokens in
  match rest with
  | [] ->
      raise (Parser_Error ("Expected ';' or initialize stmt!", []))
  | head :: rest -> (
      let init_stmt_opt, remain_tokens =
        match head.token_type with
        | Semicolon ->
            (None, rest)
        | Var ->
            let var_stmt, rest = parse_var_decl rest in
            (Some var_stmt, rest)
        | _ ->
            let expr_stmt, rest = parse_expr_stmt rest in
            (Some expr_stmt, rest)
      in
      match remain_tokens with
      | [] ->
          raise
            (Parser_Error
               ( "Expected ';' or condition after initialize stmt of 'while'!"
               , [] ))
      | head :: rest -> (
          let cond_expr_opt, remain_tokens =
            match head.token_type with
            | Semicolon ->
                (None, rest)
            | _ ->
                let expr, rest = parse_expression remain_tokens in
                let _, rest =
                  parse_one_token Semicolon "Expected ';' after loop condition!"
                    rest
                in
                (Some expr, rest)
          in
          match remain_tokens with
          | [] ->
              raise
                (Parser_Error ("Expected ')' or increment stmt after init!", []))
          | head :: rest ->
              let inc_expr_opt, remain_tokens =
                match head.token_type with
                | RightParen ->
                    (None, rest)
                | _ ->
                    let expr, rest = parse_expression remain_tokens in
                    let _, rest =
                      parse_one_token RightParen
                        "Expected ')' after 'for' clauses!" rest
                    in
                    (Some expr, rest)
              in
              let body_stmt, remain_tokens = parse_stmt remain_tokens in
              ( generate_for_loop init_stmt_opt cond_expr_opt inc_expr_opt
                  body_stmt
              , remain_tokens ) ) )

and parse_return_stmt keyword tokens =
  match tokens with
  | [] ->
      raise (Parser_Error ("Expected ';' after 'return';", []))
  | head :: _ ->
      let expr, rest =
        match head.token_type with
        | Semicolon ->
            (None, tokens)
        | _ ->
            let ret_expr, rest = parse_expression tokens in
            (Some ret_expr, rest)
      in
      let _, rest =
        parse_one_token Semicolon "Expected ';' at the end of return statement!"
          rest
      in
      (ReturnStmt {keyword; expr}, rest)

and parse_expr_stmt tokens =
  let new_expr, rest = parse_expression tokens in
  (parse_gen_stmt (fun expr toks -> (Statement {expr}, toks))) new_expr rest

and parse_expression tokens = parse_assignment tokens

and parse_assignment tokens =
  let left_expr, right_tokens = parse_or tokens in
  match right_tokens with
  | [] ->
      (left_expr, right_tokens)
  | head :: rest -> (
    match head.token_type with
    | Equal -> (
        let value, remain_tokens = parse_assignment rest in
        match left_expr with
        | Variable var ->
            (Assign {name= var.name; expr= value}, remain_tokens)
        | _ ->
            raise (Parser_Error ("Invalid assignment target!", right_tokens)) )
    | _ ->
        (left_expr, right_tokens) )

and parse_or tokens =
  let left_expr, right_tokens = parse_and tokens in
  match right_tokens with
  | [] ->
      (left_expr, right_tokens)
  | head :: rest -> (
    match head.token_type with
    | Or ->
        let right_expr, final_tokens = parse_and rest in
        ( Logical {left= left_expr; operator= head; right= right_expr}
        , final_tokens )
    | _ ->
        (left_expr, right_tokens) )

and parse_and tokens =
  let left_expr, right_tokens = parse_equality tokens in
  match right_tokens with
  | [] ->
      (left_expr, right_tokens)
  | head :: tail -> (
    match head.token_type with
    | And ->
        let right_expr, final_tokens = parse_equality tail in
        ( Logical {left= left_expr; operator= head; right= right_expr}
        , final_tokens )
    | _ ->
        (left_expr, right_tokens) )

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
  | [] ->
      parse_call tokens
  | head :: rest -> (
    match head.token_type with
    | Bang | Minus ->
        let right, rest = parse_unary rest in
        (Unary {operator= head; right}, rest)
    | _ ->
        parse_call tokens )

and parse_call tokens =
  let rec try_parse_call base_exp tokens =
    match tokens with
    | [] ->
        (base_exp, [])
    | head :: rest -> (
      match head.token_type with
      | LeftParen ->
          let call_expr, remain_tokens = parse_call_end base_exp head rest in
          try_parse_call call_expr remain_tokens
      | _ ->
          (base_exp, tokens) )
  in
  let base_exp, remain_tokens = parse_primary tokens in
  try_parse_call base_exp remain_tokens

and parse_call_end base_exp left_paren tokens =
  let rec try_parse_argument tokens acc =
    if List.length acc >= 255 then
      raise (Parser_Error ("Cannot have more than 255 arguments!", tokens))
    else
      let new_arg, remain_tokens = parse_expression tokens in
      let new_acc = new_arg :: acc in
      match remain_tokens with
      | [] ->
          raise (Parser_Error ("Expected ')' after arguments", []))
      | head :: rest -> (
        match head.token_type with
        | Comma ->
            try_parse_argument rest new_acc
        | RightParen ->
            (List.rev new_acc, rest)
        | _ ->
            raise (Parser_Error ("Expexted ')' after arguments!", rest)) )
  in
  match tokens with
  | [] ->
      raise (Parser_Error ("Expected ')' to end funciton call!", []))
  | head :: rest -> (
    match head.token_type with
    | RightParen ->
        (Call {callee= base_exp; paren= left_paren; arguments= []}, rest)
    | _ ->
        let arguments, remain_tokens = try_parse_argument tokens [] in
        (Call {callee= base_exp; paren= left_paren; arguments}, remain_tokens) )

and parse_primary tokens =
  match tokens with
  | [] ->
      raise (Parser_Error ("Reached end before finishing expression!", tokens))
  | head :: rest -> (
    match head.token_type with
    | EOF ->
        raise
          (Parser_Error ("Reached end before finishing expression!", tokens))
    | False ->
        (Literal (BoolLiteral false), rest)
    | True ->
        (Literal (BoolLiteral true), rest)
    | Nil ->
        (Literal NilLiteral, rest)
    | Number fl ->
        (Literal (FloatLiteral fl), rest)
    | String str ->
        (Literal (StringLiteral str), rest)
    | Identifier ->
        (Variable {name= head}, rest)
    | LeftParen ->
        let next_expr, rest = parse_expression rest in
        let _, rest =
          parse_one_token RightParen "Expected ')' after expression!" rest
        in
        (Grouping {expr= next_expr}, rest)
    | _ ->
        raise (Parser_Error ("Expected literal, ident, or group", tokens)) )

let rec synchronize tokens =
  match tokens with
  | [] ->
      []
  | head :: tail -> (
    match head.token_type with
    | Semicolon ->
        tail
    | Class | Fun | Var | For | If | While | Print | Return ->
        tokens
    | _ ->
        synchronize tail )

let rec parse prev_stmts prev_errors tokens =
  match tokens with
  | [] ->
      if List.length prev_errors > 0 then Error (List.rev prev_errors)
      else Ok (List.rev prev_stmts)
  | head :: _ -> (
    match head.token_type with
    | EOF ->
        parse prev_stmts prev_errors []
    | _ -> (
      try
        let next_stmt, rest_tokens = parse_decl tokens in
        parse (next_stmt :: prev_stmts) prev_errors rest_tokens
      with Parser_Error (message, tokens) -> (
        match tokens with
        | [] ->
            parse prev_stmts
              (create_error ~line:~-1 ~where:" at end" ~message :: prev_errors)
              []
        | head :: rest -> (
          match head.token_type with
          | EOF ->
              parse prev_stmts
                ( create_error ~line:head.line ~where:" at end" ~message
                :: prev_errors )
                []
          | _ ->
              parse prev_stmts
                ( create_error ~line:head.line
                    ~where:(sprintf " at '%s'" head.lexeme)
                    ~message
                :: prev_errors )
                (synchronize rest) ) ) ) )
