type token_type =
  (* single char tokens *)
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  (* one or two char tokens *)
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  (* literals *)
  | Identifier of string
  | String of string
  | Number of float
  (* keywords *)
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  (* source file stuff *)
  | Comment
  | EOF

let get_token_name = function
  | LeftParen ->
      "LeftParen"
  | RightParen ->
      "RightParen"
  | LeftBrace ->
      "LeftBrace"
  | RightBrace ->
      "RightBrace"
  | Comma ->
      "Comma"
  | Dot ->
      "Dot"
  | Minus ->
      "Minus"
  | Plus ->
      "Plus"
  | Semicolon ->
      "Semicolon"
  | Slash ->
      "Slash"
  | Star ->
      "Star"
  | Bang ->
      "Bang"
  | BangEqual ->
      "BangEqual"
  | Equal ->
      "Equal"
  | EqualEqual ->
      "EqualEqual"
  | Greater ->
      "Greater"
  | GreaterEqual ->
      "GreaterEqual"
  | Less ->
      "Less"
  | LessEqual ->
      "LessEqual"
  | Identifier _ ->
      "Identifier"
  | String _ ->
      "String"
  | Number _ ->
      "Number"
  | And ->
      "And"
  | Class ->
      "Class"
  | Else ->
      "Else"
  | False ->
      "False"
  | Fun ->
      "Fun"
  | For ->
      "For"
  | If ->
      "If"
  | Nil ->
      "Nil"
  | Or ->
      "Or"
  | Print ->
      "Print"
  | Return ->
      "Return"
  | Super ->
      "Super"
  | This ->
      "This"
  | True ->
      "True"
  | Var ->
      "Var"
  | While ->
      "While"
  | Comment ->
      "Comment"
  | EOF ->
      "EOF"

type token = {token_type: token_type; lexeme: string; line: int}

let print_token token =
  print_endline
    (String.concat " " [get_token_name token.token_type; token.lexeme])

let rec print_token_list = function
  | token :: rest ->
      print_token token ; print_token_list rest
  | [] ->
      ()
