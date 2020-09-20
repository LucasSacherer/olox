open OUnit2
open Olox.Ast
open Olox.Scanner
open Olox.Reporting
open Olox.Parser

(* test helper functions *)
let generate_error_list specs =
  List.map (fun (line, where, message) -> create_error ~line ~where ~message) specs

let run_parser_test input expected =
  match parse [] [] input with
  | Ok stmt_list ->
      assert_equal ~printer:string_of_stmt_list expected stmt_list
  | Error err_list ->
      assert_failure (string_of_error_list err_list)

let run_error_parser_test input expected =
  match parse [] [] input with
  | Ok stmt_list ->
      assert_failure (string_of_stmt_list stmt_list)
  | Error err_list ->
      assert_equal ~printer:string_of_error_list expected err_list

(* test helper structs *)

let x_token = {Olox.Token.token_type= Identifier; lexeme= "x"; line= 1}

let y_token = {Olox.Token.token_type= Identifier; lexeme= "y"; line= 1}

let z_token = {Olox.Token.token_type= Identifier; lexeme= "z"; line= 1}

let plus_token = {Olox.Token.token_type= Plus; lexeme= "+"; line= 1}

let minus_token = {Olox.Token.token_type= Minus; lexeme= "-"; line= 1}

let greater_token = {Olox.Token.token_type= Greater; lexeme= ">"; line= 1}

let less_token = {Olox.Token.token_type= Less; lexeme= "<"; line= 1}

let return_token = {Olox.Token.token_type= Return; lexeme= "return"; line= 1}

let paren_token = {Olox.Token.token_type= LeftParen; lexeme= "("; line= 1}

let this_token = {Olox.Token.token_type= This; lexeme= "this"; line= 1}

let super_token = {Olox.Token.token_type= Super; lexeme= "super"; line= 1}

let init_token = {Olox.Token.token_type= Identifier; lexeme= "init"; line= 1}

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
         ; ( "ChainPrint"
           , "print x; print y;"
           , [ PrintStmt {expr= Variable {name= x_token}}
             ; PrintStmt {expr= Variable {name= y_token}} ] )
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
           , [VarStmt {name= x_token; init= None}] )
         ; ( "BasicReturnStmt"
           , "return x;"
           , [ ReturnStmt
                 {expr= Some (Variable {name= x_token}); keyword= return_token}
             ] )
         ; ( "BasicReturnStmtNoExpr"
           , "return;"
           , [ReturnStmt {expr= None; keyword= return_token}] )
         ; ( "BasicClass"
           , "class x{}"
           , [ClassStmt {name= x_token; superclass= None; methods= []}] )
         ; ( "BasicClassMethods"
           , "class x{y(z){return z;}z(){}}"
           , [ ClassStmt
                 { name= x_token
                 ; superclass= None
                 ; methods=
                     [ { name= y_token
                       ; params= [z_token]
                       ; body=
                           BlockStmt
                             [ ReturnStmt
                                 { keyword= return_token
                                 ; expr= Some (Variable {name= z_token}) } ] }
                     ; {name= z_token; params= []; body= BlockStmt []} ] } ] )
         ; ( "BasicClassInit"
           , "class x{init(){}}"
           , [ ClassStmt
                 { name= x_token
                 ; superclass= None
                 ; methods=
                     [ { name= init_token
                       ; params= []
                       ; body=
                           BlockStmt
                             [ ReturnStmt
                                 { keyword= return_token
                                 ; expr= Some (This {keyword= this_token}) } ]
                       } ] } ] )
         ; ("BasicSemicolon", ";;", [])
         ; ( "BasicCall"
           , "x(y,z,3.0);"
           , [ Statement
                 { expr=
                     Call
                       { paren= paren_token
                       ; arguments=
                           [ Variable {name= y_token}
                           ; Variable {name= z_token}
                           ; Literal (FloatLiteral 3.0) ]
                       ; callee= Variable {name= x_token} } } ] )
         ; ( "BasicClassGet"
           , "x.y;"
           , [ Statement
                 {expr= Get {obj= Variable {name= x_token}; name= y_token}} ] )
         ; ( "BasicClassChain"
           , "x.y().z(3);"
           , [ Statement
                 { expr=
                     Call
                       { paren= paren_token
                       ; arguments= [Literal (FloatLiteral 3.0)]
                       ; callee=
                           Get
                             { name= z_token
                             ; obj=
                                 Call
                                   { paren= paren_token
                                   ; arguments= []
                                   ; callee=
                                       Get
                                         { name= y_token
                                         ; obj= Variable {name= x_token} } } }
                       } } ] )
         ; ( "BasicSet"
           , "x.y.z = 3;"
           , [ Statement
                 { expr=
                     Set
                       { obj= Get {name= y_token; obj= Variable {name= x_token}}
                       ; name= z_token
                       ; value= Literal (FloatLiteral 3.0) } } ] )
         ; ( "BasicThis"
           , "return this;"
           , [ ReturnStmt
                 {keyword= return_token; expr= Some (This {keyword= this_token})}
             ] )
         ; ( "BasicClassInheritance"
           , "class x < y {}"
           , [ ClassStmt
                 { name= x_token
                 ; superclass= Some (Variable {name= y_token})
                 ; methods= [] } ] )
         ; ( "BasicSuperCall"
           , "super.x(y);"
           , [ Statement
                 { expr=
                     Call
                       { callee= Super {keyword= super_token; meth= x_token}
                       ; paren= paren_token
                       ; arguments= [Variable {name= y_token}] } } ] ) ]

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
         [ ("NoEOFIf", "if if", [(1, " at end", "Expected '(' after 'if'!")])
         ; ( "ReturnInInit"
           , "class X{init(){return a;}}"
           , [(1, " at '}'", "Init function cannot contain return statement!")]
           )
         ; ( "SuperWithNoCall"
           , "return super;"
           , [(1, " at ';'", "Expected '.' after 'super'!")] ) ]

(* full test suit and run function *)
let full_suite =
  "ParserTests"
  >::: [basic_tests_suite; basic_error_tests_suite; loop_tests_suite]

let () = run_test_tt_main full_suite
