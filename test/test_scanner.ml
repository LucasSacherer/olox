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

(* unit tests start here *)
(* new line tests *)
let basic_new_line_test _ =
  let input_string = "if\nif" in
  let expected_tokens =
    generate_token_list [(If, "if", 1); (If, "if", 2); (EOF, "", 2)]
  in
  let tokens = Result.get_ok (scan_tokens input_string) in
  token_list_assert_equal expected_tokens tokens

let new_line_suite =
  "NewLineSuite" >::: ["BasiceNewlineTest" >:: basic_new_line_test]

(* full test suit and run function *)
let full_suite = "ScannerTests" >::: [new_line_suite]

let () = run_test_tt_main full_suite
