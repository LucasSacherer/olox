(** This module contains the token type created by the olox scanner and consumed by the parser. *)

(** This type contains all the support token types. Note that the [String] type contains the value
    of the string literal without the surrounding quotes. The [Number] type contains the
    float representation of the number literal. *)
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
  | EOF

(** The token type produced by the scanner and consumed by the parser. [lexeme] 
    contains the exact string used to create the token (so, lexems of [String] tokens
    include the quotes on both ends). [line] contains the line number where the lexem
    was found. For a multi line token, the first line is used.*)
type token = {token_type: token_type; lexeme: string; line: int}

val get_token_name : token_type -> string
(** Given a [token_type], returns the matching name. The name for a [token_type] is
    exactly the same as the name of the type (so, [Fun] returns ["Fun"]).*)

val print_token : token -> unit
(** Given a [token], prints it's string representation to stdout. For more information
    on the string representation, see {!Token.string_of_token}. *)

val print_token_list : token list -> unit
(** Just like {!Token.print_token}, but takes a list of [token]. Different tokens are
    seperated by newlines. *)

val string_of_token : token -> string
(** Generates a string representation of a [token]. The format is [token_type<lexem:line>]
    where [token_type], [lexem], and [line] are replaced by the matching values in the
    [token] struct. *)

val string_of_token_list : token list -> string
(** Just like {!Token.string_of_token}, but takes a list of [token]. Different tokens
    are seperated by a space. *)
