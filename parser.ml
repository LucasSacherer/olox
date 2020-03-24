open Ast
open Token
open Reporting
open Printf

exception Parser_Error of string * token list

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

let rec parse_decl tokens =
  match tokens with
  | [] ->
      raise (Parser_Error ("Tried to parse empty decleration!", tokens))
  | head :: rest -> (
    match head.token_type with
    | Var ->
        parse_var_decl rest
    | _ ->
        parse_stmt tokens )

and parse_var_decl tokens =
  match tokens with
  | [] ->
      raise (Parser_Error ("Empty var decleration!", tokens))
  | head :: rest -> (
      let name =
        match head.token_type with
        | Identifier _ ->
            head
        | _ ->
            raise (Parser_Error ("Expected identifier after var!", rest))
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
          | _ -> (
            match rest with
            | [] ->
                raise (Parser_Error ("Expected ';' after value!", rest))
            | head :: rest -> (
              match head.token_type with
              | Semicolon ->
                  (VarStmt {name; init}, rest)
              | _ ->
                  raise (Parser_Error ("Expected ';' after value!", rest)) ) ) )
      )

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
  match tokens with
  | [] ->
      raise (Parser_Error ("Reached EOF after 'if'!", []))
  | head :: rest -> (
    match head.token_type with
    | LeftParen -> (
        let condition, tokens_1 = parse_expression rest in
        match tokens_1 with
        | [] ->
            raise (Parser_Error ("Reached EOF after 'if' condition!", []))
        | head :: rest -> (
          match head.token_type with
          | RightParen -> (
              let then_branch, tokens_2 = parse_stmt rest in
              match tokens_2 with
              | [] ->
                  (IfStmt {condition; then_branch; else_branch= None}, [])
              | head :: rest -> (
                match head.token_type with
                | Else ->
                    let else_branch, tokens_3 = parse_stmt rest in
                    ( IfStmt
                        {condition; then_branch; else_branch= Some else_branch}
                    , tokens_3 )
                | _ ->
                    ( IfStmt {condition; then_branch; else_branch= None}
                    , tokens_2 ) ) )
          | _ ->
              raise (Parser_Error ("Expect ')' after if condition!", [])) ) )
    | _ ->
        raise (Parser_Error ("Expect '(' after 'if'!", rest)) )

and parse_while_stmt tokens =
  match tokens with
  | [] ->
      raise (Parser_Error ("Expect '(' after 'while'!", []))
  | head :: rest -> (
    match head.token_type with
    | LeftParen -> (
        let condition, after_cond_tokens = parse_expression rest in
        match after_cond_tokens with
        | [] ->
            raise (Parser_Error ("Expect ')' after condition!", []))
        | head :: tail -> (
          match head.token_type with
          | RightParen ->
              let body, final_tokens = parse_stmt tail in
              (WhileStmt {condition; body}, final_tokens)
          | _ ->
              raise (Parser_Error ("Expect ')' after condition!", [])) ) )
    | _ ->
        raise (Parser_Error ("Expect '(' after 'while'!", [])) )

and parse_for_stmt tokens =
  match tokens with
  | [] ->
      raise (Parser_Error ("Expected '(' after 'for'!", []))
  | head :: rest -> (
    match head.token_type with
    | LeftParen -> (
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
                   ( "Expected ';' or condition after initialize stmt of \
                      'while'!"
                   , [] ))
          | head :: rest -> (
              let cond_expr_opt, remain_tokens =
                match head.token_type with
                | Semicolon ->
                    (None, rest)
                | _ -> (
                    let expr, rest = parse_expression remain_tokens in
                    match rest with
                    | [] ->
                        raise
                          (Parser_Error
                             ("Expected ';' after loop condition!", []))
                    | head :: rest -> (
                      match head.token_type with
                      | Semicolon ->
                          (Some expr, rest)
                      | _ ->
                          raise
                            (Parser_Error
                               ("Expected ';' after loop condition!", rest)) ) )
              in
              match remain_tokens with
              | [] ->
                  raise
                    (Parser_Error
                       ("Expected ')' or increment stmt after init!", []))
              | head :: rest ->
                  let inc_expr_opt, remain_tokens =
                    match head.token_type with
                    | RightParen ->
                        (None, rest)
                    | _ -> (
                        let expr, rest = parse_expression remain_tokens in
                        match rest with
                        | [] ->
                            raise
                              (Parser_Error
                                 ("Expected ')' after for clauses!", []))
                        | head :: rest -> (
                          match head.token_type with
                          | RightParen ->
                              (Some expr, rest)
                          | _ ->
                              raise
                                (Parser_Error
                                   ("Expected ')' after for clauses!", rest)) )
                        )
                  in
                  let body_stmt, remain_tokens = parse_stmt remain_tokens in
                  ( generate_for_loop init_stmt_opt cond_expr_opt inc_expr_opt
                      body_stmt
                  , remain_tokens ) ) ) )
    | _ ->
        raise (Parser_Error ("Expected '(' after 'for'!", rest)) )

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
      parse_primary tokens
  | head :: rest -> (
    match head.token_type with
    | Bang | Minus ->
        let right, rest = parse_unary rest in
        (Unary {operator= head; right}, rest)
    | _ ->
        parse_primary tokens )

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
    | Identifier _ ->
        (Variable {name= head}, rest)
    | LeftParen -> (
        let next_expr, rest = parse_expression rest in
        match rest with
        | [] ->
            raise (Parser_Error ("Expected expression after '('", rest))
        | head :: rest -> (
          match head.token_type with
          | RightParen ->
              (Grouping {expr= next_expr}, rest)
          | _ ->
              raise (Parser_Error ("Expected ')' after expression", rest)) ) )
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
              ({line= -1; where= " at end"; message} :: prev_errors)
              []
        | head :: _ -> (
          match head.token_type with
          | EOF ->
              parse prev_stmts
                ({line= head.line; where= " at end"; message} :: prev_errors)
                []
          | _ ->
              parse prev_stmts
                ( { line= head.line
                  ; where= sprintf " at '%s'" head.lexeme
                  ; message }
                :: prev_errors )
                (synchronize tokens) ) ) ) )
