open Reporting
open Token

(*** Keyword Hashtable ***)
let keywords = Hashtbl.create 20

let () =
  Hashtbl.add keywords "and" And ;
  Hashtbl.add keywords "class" Class ;
  Hashtbl.add keywords "else" Else ;
  Hashtbl.add keywords "false" False ;
  Hashtbl.add keywords "for" For ;
  Hashtbl.add keywords "fun" Fun ;
  Hashtbl.add keywords "if" If ;
  Hashtbl.add keywords "nil" Nil ;
  Hashtbl.add keywords "or" Or ;
  Hashtbl.add keywords "print" Print ;
  Hashtbl.add keywords "return" Return ;
  Hashtbl.add keywords "super" Super ;
  Hashtbl.add keywords "this" This ;
  Hashtbl.add keywords "true" True ;
  Hashtbl.add keywords "var" Var ;
  Hashtbl.add keywords "while" While

(*** Start of Scanner ***)
type position = {start: int; current: int; line: int}

let is_digit in_char = in_char >= '0' && in_char <= '9'

let is_alpha in_char =
  (in_char >= 'a' && in_char <= 'z') || (in_char >= 'A' && in_char <= 'Z')

let is_alpha_num in_char = is_alpha in_char || is_digit in_char

let is_at_end str pos = pos.current >= String.length str

let get_next_char str pos =
  if is_at_end str pos then None
  else
    let next_char = str.[pos.current] in
    let next_pos = {pos with current= pos.current + 1} in
    Some (next_char, next_pos)

let try_peek str pos =
  match get_next_char str pos with
  | Some (next_char, _) ->
      Some next_char
  | None ->
      None

(* double look ahead *)
let try_peek_next str pos =
  match get_next_char str pos with
  | None ->
      None
  | Some (_, next_pos) -> (
    match get_next_char str next_pos with
    | None ->
        None
    | Some (next_char, _) ->
        Some next_char )

let try_look_ahead str pos target =
  let result = get_next_char str pos in
  match result with
  | Some (next_char, next_pos) when next_char = target ->
      (true, next_pos)
  | _ ->
      (false, pos)

let create_token_pos_pair str pos token_type =
  let length = pos.current - pos.start in
  let lexeme = String.sub str pos.start length in
  (pos, Ok {token_type; lexeme; line= pos.line})

let do_equal_lookahead str start_pos no_type yes_type =
  let got_match, final_pos = try_look_ahead str start_pos '=' in
  let final_type = if got_match then yes_type else no_type in
  create_token_pos_pair str final_pos final_type

let rec scan_string str start_pos =
  match try_peek str start_pos with
  | None ->
      ( start_pos
      , Error {line= start_pos.line; where= ""; message= "Unterminated string."}
      )
  | Some next_char -> (
    match next_char with
    | '"' ->
        let length = start_pos.current - (start_pos.start + 1) in
        let contents = String.sub str (start_pos.start + 1) length in
        let final_pos = {start_pos with current= start_pos.current + 1} in
        create_token_pos_pair str final_pos (String contents)
    | '\n' ->
        let next_pos =
          { start_pos with
            current= start_pos.current + 1
          ; line= start_pos.line + 1 }
        in
        scan_string str next_pos
    | _ ->
        let next_pos = {start_pos with current= start_pos.current + 1} in
        scan_string str next_pos )

let rec scan_comment str start_pos =
  match try_peek str start_pos with
  | None ->
      create_token_pos_pair str start_pos Comment
  | Some next_char -> (
    match next_char with
    | '\n' ->
        let next_pos =
          { start_pos with
            current= start_pos.current + 1
          ; line= start_pos.line + 1 }
        in
        create_token_pos_pair str next_pos Comment
    | _ ->
        let next_pos = {start_pos with current= start_pos.current + 1} in
        scan_comment str next_pos )

let rec scan_digits str start_pos =
  match try_peek str start_pos with
  | None ->
      start_pos
  | Some next_char -> (
    match next_char with
    | _ when is_digit next_char ->
        let next_pos = {start_pos with current= start_pos.current + 1} in
        scan_digits str next_pos
    | _ ->
        start_pos )

let scan_number_aux str start_pos =
  (* get the first numbers *)
  let first_digits_pos = scan_digits str start_pos in
  (* check for a dot *)
  let got_match, final_pos = try_look_ahead str first_digits_pos '.' in
  match got_match with
  | false ->
      first_digits_pos
  | true -> (
    (* check for a number after the dot *)
    match try_peek_next str first_digits_pos with
    | None ->
        first_digits_pos
    | Some next_char -> (
      match next_char with
      | _ when is_digit next_char ->
          let float_pos = scan_digits str final_pos in
          float_pos
      | _ ->
          first_digits_pos ) )

let scan_number str start_pos =
  let final_pos = scan_number_aux str start_pos in
  let length = final_pos.current - final_pos.start in
  let lexeme = String.sub str final_pos.start length in
  let float_val = float_of_string lexeme in
  create_token_pos_pair str final_pos (Number float_val)

let scan_slash str start_pos =
  let got_match, final_pos = try_look_ahead str start_pos '/' in
  if got_match then scan_comment str final_pos
  else create_token_pos_pair str final_pos Slash

let create_ident_token str pos =
  let length = pos.current - pos.start in
  let lexeme = String.sub str pos.start length in
  match Hashtbl.find_opt keywords lexeme with
  | None ->
      create_token_pos_pair str pos Identifier
  | Some key_type ->
      create_token_pos_pair str pos key_type

let rec scan_ident str start_pos =
  match try_peek str start_pos with
  | None ->
      create_ident_token str start_pos
  | Some next_char -> (
    match next_char with
    | _ when is_alpha_num next_char ->
        let next_pos = {start_pos with current= start_pos.current + 1} in
        scan_ident str next_pos
    | _ ->
        create_ident_token str start_pos )

let rec scan_token str pos =
  let result = get_next_char str pos in
  match result with
  | None ->
      (pos, Ok {token_type= EOF; lexeme= ""; line= pos.line})
  | Some (next_char, next_pos) -> (
    match next_char with
    (* simple single char stuff *)
    | '(' ->
        create_token_pos_pair str next_pos LeftParen
    | ')' ->
        create_token_pos_pair str next_pos RightParen
    | '{' ->
        create_token_pos_pair str next_pos LeftBrace
    | '}' ->
        create_token_pos_pair str next_pos RightBrace
    | ',' ->
        create_token_pos_pair str next_pos Comma
    | '.' ->
        create_token_pos_pair str next_pos Dot
    | '-' ->
        create_token_pos_pair str next_pos Minus
    | '+' ->
        create_token_pos_pair str next_pos Plus
    | ';' ->
        create_token_pos_pair str next_pos Semicolon
    | '*' ->
        create_token_pos_pair str next_pos Star
    (* variable length operators *)
    | '!' ->
        do_equal_lookahead str next_pos Bang BangEqual
    | '=' ->
        do_equal_lookahead str next_pos Equal EqualEqual
    | '<' ->
        do_equal_lookahead str next_pos Less LessEqual
    | '>' ->
        do_equal_lookahead str next_pos Greater GreaterEqual
    | '/' ->
        scan_slash str next_pos
    (* whitespace *)
    | ' ' | '\r' | '\t' ->
        let new_pos = {next_pos with start= next_pos.current} in
        scan_token str new_pos
    | '\n' ->
        let new_pos =
          {next_pos with start= next_pos.current; line= next_pos.line + 1}
        in
        scan_token str new_pos
    (* string *)
    | '"' ->
        scan_string str next_pos
    (* numbers *)
    | _ when is_digit next_char ->
        scan_number str next_pos
    (* identifier/keywords *)
    | _ when is_alpha next_char ->
        scan_ident str next_pos
    | _ ->
        ( next_pos
        , Error
            { line= next_pos.line
            ; where= ""
            ; message=
                String.concat ""
                  ["Unexpected character: '"; String.make 1 next_char; "'"] } )
    )

let rec scan_tokens_aux str pos acc error_acc =
  let new_pos, new_tok = scan_token str pos in
  let next_pos = {new_pos with start= new_pos.current} in
  match new_tok with
  | Ok ({token_type= EOF; _} as token) ->
      (token :: acc, error_acc)
  | Ok token ->
      scan_tokens_aux str next_pos (token :: acc) error_acc
  | Error err ->
      scan_tokens_aux str next_pos acc (err :: error_acc)

let scan_tokens str =
  let tokens, errors =
    scan_tokens_aux str {start= 0; current= 0; line= 1} [] []
  in
  match errors with [] -> Ok (List.rev tokens) | _ -> Error (List.rev errors)
