open OUnit2
open Olox.Ast
open Olox.Scanner
open Olox.Reporting
open Olox.Parser

(* test helper functions *)
let stmt_list_assert_equal = assert_equal ~printer:string_of_stmt_list

let error_list_assert_equal = assert_equal ~printer:string_of_error_list

let generate_error_list specs =
  let rec loop specs acc =
    match specs with
    | [] ->
        List.rev acc
    | head :: rest ->
        let line, where, message = head in
        let new_error = create_error ~line ~where ~message in
        loop rest (new_error :: acc)
  in
  loop specs []

let run_parser_test input expected =
  let stmt_list = Result.get_ok (parse [] [] input) in
  stmt_list_assert_equal expected stmt_list

let run_error_parser_test input expected =
  let error_list = Result.get_error (parse [] [] input) in
  error_list_assert_equal expected error_list

(* test helper structs *)

let x_token = {Olox.Token.token_type= Identifier; lexeme= "x"; line= 1}

let plus_token = {Olox.Token.token_type= Plus; lexeme= "+"; line= 1}

let minus_token = {Olox.Token.token_type= Minus; lexeme= "-"; line= 1}

let greater_token = {Olox.Token.token_type= Greater; lexeme= ">"; line= 1}

(* unit tests start here *)
let basic_tests_suite =
  "BasicSuite"
  >::: List.map
         (fun (title, to_parse, exp) ->
           let token_list = Result.get_ok (scan_tokens to_parse) in
           title >:: fun _ -> run_parser_test token_list exp)
         [ ( "BasicPrint"
           , "print 1;"
           , [PrintStmt {expr= Literal (FloatLiteral 1.0)}] )
         ; ( "BasicIf"
           , "if (true) 1;"
           , [ IfStmt
                 { condition= Literal (BoolLiteral true)
                 ; then_branch= Statement {expr= Literal (FloatLiteral 1.0)}
                 ; else_branch= None } ] )
         ; ( "BasicIfWithElse"
           , "if (true) 1; else 2;"
           , [ IfStmt
                 { condition= Literal (BoolLiteral true)
                 ; then_branch= Statement {expr= Literal (FloatLiteral 1.0)}
                 ; else_branch=
                     Some (Statement {expr= Literal (FloatLiteral 2.0)}) } ] )
         ; ( "BasicStmt"
           , "1 + 1;"
           , [ Statement
                 { expr=
                     Binary
                       { left= Literal (FloatLiteral 1.0)
                       ; right= Literal (FloatLiteral 1.0)
                       ; operator= plus_token } } ] )
         ; ( "BasicWhile"
           , "while (x > 2) {x = x - 1; print x;}"
           , [ WhileStmt
                 { condition=
                     Binary
                       { left= Variable {name= x_token}
                       ; right= Literal (FloatLiteral 2.0)
                       ; operator= greater_token }
                 ; body=
                     BlockStmt
                       [ Statement
                           { expr=
                               Assign
                                 { name= x_token
                                 ; expr=
                                     Binary
                                       { left= Variable {name= x_token}
                                       ; right= Literal (FloatLiteral 1.0)
                                       ; operator= minus_token } } }
                       ; PrintStmt {expr= Variable {name= x_token}} ] } ] ) ]

let basic_error_tests_suite =
  "ErrorSuite"
  >::: List.map
         (fun (title, to_parse, exp) ->
           let token_list = Result.get_ok (scan_tokens to_parse) in
           let error_list = generate_error_list exp in
           title >:: fun _ -> run_error_parser_test token_list error_list)
         [("NoEOFIf", "if if", [(1, " at end", "Expect '(' after 'if'!")])]

(* full test suit and run function *)
let full_suite = "ParserTests" >::: [basic_tests_suite; basic_error_tests_suite]

let () = run_test_tt_main full_suite
