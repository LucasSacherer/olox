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

let y_token = {Olox.Token.token_type= Identifier; lexeme= "y"; line= 1}

let z_token = {Olox.Token.token_type= Identifier; lexeme= "z"; line= 1}

let plus_token = {Olox.Token.token_type= Plus; lexeme= "+"; line= 1}

let minus_token = {Olox.Token.token_type= Minus; lexeme= "-"; line= 1}

let greater_token = {Olox.Token.token_type= Greater; lexeme= ">"; line= 1}

let less_token = {Olox.Token.token_type= Less; lexeme= "<"; line= 1}

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
         ; ( "BasicFunc1"
           , "fun x() { 1; }"
           , [ FuncStmt
                 { name= x_token
                 ; params= []
                 ; body= BlockStmt [Statement {expr= Literal (FloatLiteral 1.0)}]
                 } ] )
         ; ( "BasicFunc2"
           , "fun x(y,z) { 1; }"
           , [ FuncStmt
                 { name= x_token
                 ; params= [y_token; z_token]
                 ; body= BlockStmt [Statement {expr= Literal (FloatLiteral 1.0)}]
                 } ] )
         ; ( "BasicVarDecl"
           , "var x = 1.0;"
           , [VarStmt {name= x_token; init= Some (Literal (FloatLiteral 1.0))}]
           )
         ; ( "BasicVarDeclNoInit"
           , "var x;"
           , [VarStmt {name= x_token; init= None}] ) ]

let loop_tests_suite =
  "LoopSuite"
  >::: List.map
         (fun (title, to_parse, exp) ->
           let token_list = Result.get_ok (scan_tokens to_parse) in
           title >:: fun _ -> run_parser_test token_list exp)
         [ ( "WhileLoop"
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
                       ; PrintStmt {expr= Variable {name= x_token}} ] } ] )
         ; ( "ForLoopNoArgs"
           , "for (;;) {x;}"
           , [ WhileStmt
                 { condition= Literal (BoolLiteral true)
                 ; body= BlockStmt [Statement {expr= Variable {name= x_token}}]
                 } ] )
         ; ( "ForLoopWithDecl"
           , "for (var x = 1.0;;) {x;}"
           , [ BlockStmt
                 [ VarStmt
                     {name= x_token; init= Some (Literal (FloatLiteral 1.0))}
                 ; WhileStmt
                     { condition= Literal (BoolLiteral true)
                     ; body=
                         BlockStmt [Statement {expr= Variable {name= x_token}}]
                     } ] ] )
         ; ( "ForLoopFull"
           , "for (var x = 1; x < 1.0; x = x + 1) {print x;}"
           , [ BlockStmt
                 [ VarStmt
                     {name= x_token; init= Some (Literal (FloatLiteral 1.0))}
                 ; WhileStmt
                     { condition=
                         Binary
                           { left= Variable {name= x_token}
                           ; operator= less_token
                           ; right= Literal (FloatLiteral 1.0) }
                     ; body=
                         BlockStmt
                           [ BlockStmt
                               [PrintStmt {expr= Variable {name= x_token}}]
                           ; Statement
                               { expr=
                                   Assign
                                     { name= x_token
                                     ; expr=
                                         Binary
                                           { left= Variable {name= x_token}
                                           ; operator= plus_token
                                           ; right= Literal (FloatLiteral 1.0)
                                           } } } ] } ] ] ) ]

let basic_error_tests_suite =
  "ErrorSuite"
  >::: List.map
         (fun (title, to_parse, exp) ->
           let token_list = Result.get_ok (scan_tokens to_parse) in
           let error_list = generate_error_list exp in
           title >:: fun _ -> run_error_parser_test token_list error_list)
         [("NoEOFIf", "if if", [(1, " at end", "Expected '(' after 'if'!")])]

(* full test suit and run function *)
let full_suite =
  "ParserTests"
  >::: [basic_tests_suite; basic_error_tests_suite; loop_tests_suite]

let () = run_test_tt_main full_suite
