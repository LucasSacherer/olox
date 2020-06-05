open OUnit2
open Olox.Reporting
open Olox.Environ
open Olox.Interpreter

(* test helper functions *)
let value_assert_equal = assert_equal ~printer:string_of_value

let error_assert_equal = assert_equal ~printer:string_of_error_list

let generate_error_list specs =
  let rec loop specs acc =
    match specs with
    | [] ->
        List.rev acc
    | head :: rest ->
        let line, where, message = head in
        loop rest (create_error ~line ~where ~message :: acc)
  in
  loop specs []

let scan_and_parse input =
  let tokens_res = Olox.Scanner.scan_tokens input in
  let tokens = Result.get_ok tokens_res in
  Result.get_ok (Olox.Parser.parse [] [] tokens)

let run_interpreter_test input expected =
  let stmt_list = scan_and_parse input in
  let res = interpret (create_environ ()) stmt_list in
  let value, _ = Result.get_ok res in
  value_assert_equal expected value

let run_error_parser_test input expected =
  let stmt_list = scan_and_parse input in
  let res = interpret (create_environ ()) stmt_list in
  let error = Result.get_error res in
  error_assert_equal expected error

(* unit tests start here *)
let basic_tests_suite =
  "BasicSuite"
  >::: List.map
         (fun (title, to_interp, exp) ->
           title >:: fun _ -> run_interpreter_test to_interp exp)
         [ ("BasicArith", "1 + 2 + 4;", FloatValue 7.0)
         ; ("GreaterThanFalse", "3 > 4;", BoolValue false)
         ; ("GreaterThanTrue", "4 > 3;", BoolValue true)
         ; ("LessThanFalse", "4 < 3;", BoolValue false)
         ; ("LessThanTrue", "3 < 4;", BoolValue true)
         ; ("SubstractInPlace", "var a = 3; a = a - 1; a;", FloatValue 2.0)
         ; ("SetInScope", "var a = 1; { a = 20; } a;", FloatValue 20.0)
         ; ( "WhileLoop"
           , "var a = 0; while (a < 10) { a = a + 1; } a;"
           , FloatValue 10.0 )
         ; ( "ReverseWhileLoop"
           , "var a = 10; while (a > 0) { a = a - 1; } a;"
           , FloatValue 0.0 )
         ; ( "ReturnCall"
           , "fun func(a,b) {return a + b;} func(1, 2);"
           , FloatValue 3.0 ) ]

let basic_error_tests_suite =
  "ErrorSuite"
  >::: List.map
         (fun (title, to_interp, exp) ->
           let error_list = generate_error_list exp in
           title >:: fun _ -> run_error_parser_test to_interp error_list)
         [ ("UninitializedVar", "a + 1;", [(1, "", "No variable called 'a'")])
         ; ( "ReturnNotInFunc"
           , "return 1;"
           , [(1, " at return", "Called return outside of a function!")] ) ]

(* full test suit and run function *)
let full_suite =
  "InterpreterTests" >::: [basic_tests_suite; basic_error_tests_suite]

let () = run_test_tt_main full_suite
