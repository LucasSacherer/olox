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
           , FloatValue 3.0 )
         ; ( "Clojure"
           , "fun a() {var i = 1; fun b() {return i;} i = 2; return b();} a();"
           , FloatValue 1.0 )
         ; ( "RecursiveLambda"
           , "fun a() {fun b(x){if (x <= 1) return x; else return b(x - 1);} \
              return b(5);} a();"
           , FloatValue 1.0 )
         ; ( "ClassGetSet"
           , "class X {} fun y(){var x = X();fun a(){return x.i;} x.i=2; \
              return a();} y();"
           , FloatValue 2.0 )
         ; ("ClassInit", "class X{init(n){this.a = n;}} X(2).a;", FloatValue 2.0)
         ; ( "ClassInheritence"
           , "class a {foo(){return 2;}} class b < a {} b().foo();"
           , FloatValue 2.0 )
         ; ( "ClassSuper"
           , "class a{x(){return 3;}} class b < a{x(){return super.x() + 2;}} \
              class c < b{x(){return super.x() + 1;}} c().x();"
           , FloatValue 6.0 ) ]

let basic_error_tests_suite =
  "ErrorSuite"
  >::: List.map
         (fun (title, to_interp, exp) ->
           let error_list = generate_error_list exp in
           title >:: fun _ -> run_error_parser_test to_interp error_list)
         [ ("UninitializedVar", "a + 1;", [(1, "", "No variable called 'a'")])
         ; ( "ReturnNotInFunc"
           , "return 1;"
           , [(1, " at return", "Called return outside of a function!")] )
         ; ( "InheritFromSelf"
           , "class a {} class a < a {}"
           , [(1, " at class definition", "Class cannot inherit from itself!")]
           ) ]

let end_to_end_suite =
  "EndToEnd"
  >::: List.map
         (fun (title, to_interp, exp) ->
           title >:: fun _ -> run_interpreter_test to_interp exp)
         [ ( "Fibonacci"
           , "fun fibonacci(n) {if (n<=1) return n; return fibonacci(n-2) + \
              fibonacci(n-1);} fibonacci(10);"
           , FloatValue 55.0 )
         ; ( "FibonacciClass"
           , "class A{\n\
             \  next() {var next = this.a + this.b; this.a = this.b; \n\
             \          this.b = next; return next;}\n\
             \  calc(n) {this.a = 0; this.b = 1; \n\
             \           return this.calciter(n);}\n\
             \  calciter(n) {if (n <= 0) return this.a; this.next();\n\
             \                return this.calciter(n - 1);}}\n\
              A().calc(10);"
           , FloatValue 55.0 ) ]

(* full test suit and run function *)
let full_suite =
  "InterpreterTests"
  >::: [basic_tests_suite; basic_error_tests_suite; end_to_end_suite]

let () = run_test_tt_main full_suite
