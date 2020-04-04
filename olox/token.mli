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
  | Identifier
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

type token = {token_type: token_type; lexeme: string; line: int}

val get_token_name : token_type -> string

val print_token : token -> unit

val print_token_list : token list -> unit
