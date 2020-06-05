open OUnit2
open Olox.Token
open Olox.Scanner

(* 
 * The only public function of the scanner is scan_tokens
 * Here, we make sure that the scanner generate the correct list
 * of tokens for some generic examples of the lox language 
 *)

(* test helper functions *)
let token_list_assert_equal = assert_equal ~printer:string_of_token_list

let generate_token_list specs =
  let rec loop specs acc =
    match specs with
    | [] ->
        List.rev acc
    | head :: rest ->
        let token_type, lexeme, line = head in
        let new_token = {token_type; lexeme; line} in
        loop rest (new_token :: acc)
  in
  loop specs []

let run_scanner_test input expected =
  let expected_tokens = generate_token_list expected in
  let tokens = Result.get_ok (scan_tokens input) in
  token_list_assert_equal expected_tokens tokens

(* unit tests start here *)
(* new line tests *)
let basic_new_line_test _ =
  run_scanner_test "if\nif" [(If, "if", 1); (If, "if", 2); (EOF, "", 2)]

let string_new_line_test _ =
  run_scanner_test "if\"ab\ncd\"if"
    [ (If, "if", 1)
    ; (String "ab\ncd", "\"ab\ncd\"", 2)
    ; (If, "if", 2)
    ; (EOF, "", 2) ]

let new_line_suite =
  "NewLineSuite"
  >::: [ "BasiceNewlineTest" >:: basic_new_line_test
       ; "StringNewLineTest" >:: string_new_line_test ]

(* literals tests *)
let integer_tests =
  "IntegerLiteral"
  >::: List.map
         (fun (to_lex, exp) ->
           let title = Printf.sprintf "Lexing:'%s'->%f" to_lex exp in
           title
           >:: fun _ ->
           run_scanner_test to_lex [(Number exp, to_lex, 1); (EOF, "", 1)])
         [("1.0", 1.0); ("34.0", 34.0); ("123.456", 123.456)]

let string_tests =
  "StringLiteral"
  >::: List.map
         (fun to_lex ->
           let title = Printf.sprintf "Lexing:'%s'" to_lex in
           title
           >:: fun _ ->
           run_scanner_test to_lex
             [ ( String (String.sub to_lex 1 (String.length to_lex - 2))
               , to_lex
               , 1 )
             ; (EOF, "", 1) ])
         ["\"hello world\""; "\"\""; "\"if\""]

let literals_test_suite = "LiteralsTestSuite" >::: [integer_tests; string_tests]

(* statement tests *)
let while_stmt_test _ =
  run_scanner_test "while (x > 2) {x = x - 1; print x;}"
    [ (While, "while", 1)
    ; (LeftParen, "(", 1)
    ; (Identifier, "x", 1)
    ; (Greater, ">", 1)
    ; (RightParen, ")", 1)
    ; (LeftBrace, "{", 1)
    ; (Identifier, "x", 1)
    ; (Equal, "=", 1)
    ; (Identifier, "x", 1)
    ; (Minus, "-", 1)
    ; (Number 1.0, "1", 1)
    ; (Semicolon, ";", 1)
    ; (Print, "print", 1)
    ; (Identifier, "x", 1)
    ; (Semicolon, ";", 1)
    ; (RightBrace, "}", 1)
    ; (EOF, "", 1) ]

let return_stmt_test _ =
  run_scanner_test "return x;"
    [(Return, "return", 1); (Identifier, "x", 1); (Semicolon, ";", 1)]

let statement_test_suite =
  "StatementSuite"
  >::: ["WhileStmt" >:: while_stmt_test; "ReturnStmt" >:: return_stmt_test]

(* full test suit and run function *)
let full_suite = "ScannerTests" >::: [new_line_suite; literals_test_suite]

let () = run_test_tt_main full_suite
