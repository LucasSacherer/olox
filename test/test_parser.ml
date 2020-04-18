open OUnit2
open Olox.Ast
open Olox.Token
open Olox.Reporting
open Olox.Parser

(* test helper functions *)
let stmt_list_assert_equal = assert_equal ~printer:string_of_stmt_list

let error_list_assert_equal = assert_equal ~printer:string_of_error_list

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

let generate_error_list specs =
  let rec loop specs acc =
    match specs with
    | [] ->
        List.rev acc
    | head :: rest ->
        let line, where, message = head in
        let new_error = {line; where; message} in
        loop rest (new_error :: acc)
  in
  loop specs []

let run_parser_test input expected =
  let stmt_list = Result.get_ok (parse [] [] input) in
  stmt_list_assert_equal expected stmt_list

let run_error_parser_test input expected =
  let error_list = Result.get_error (parse [] [] input) in
  error_list_assert_equal expected error_list

(* unit tests start here *)
let basic_tests_suite =
  "BasicSuite"
  >::: List.map
         (fun (title, to_parse, exp) ->
           let token_list = generate_token_list to_parse in
           title >:: fun _ -> run_parser_test token_list exp)
         [ ( "BasicPrint"
           , [(Print, "print", 1); (Number 1.0, "1", 1); (Semicolon, ";", 1)]
           , [PrintStmt {expr= Literal (FloatLiteral 1.0)}] ) ]

let basic_error_tests_suite =
  "ErrorSuite"
  >::: List.map
         (fun (title, to_parse, exp) ->
           let token_list = generate_token_list to_parse in
           let error_list = generate_error_list exp in
           title >:: fun _ -> run_error_parser_test token_list error_list)
         [ ( "NoEOFIf"
           , [(If, "if", 1); (If, "if", 1)]
           , [(-1, " at end", "Expect '(' after 'if'!")] )
         ; ( "EOFIf"
           , [(If, "if", 1); (If, "if", 1); (EOF, "", 1)]
           , [(1, " at end", "Expect '(' after 'if'!")] ) ]

(* full test suit and run function *)
let full_suite = "ParserTests" >::: [basic_tests_suite; basic_error_tests_suite]

let () = run_test_tt_main full_suite
