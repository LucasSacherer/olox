(* lox interpreter written in ocaml *)
(* Writen by Lucas Sacherer *)

open Reporting

(** Runs the interpreter on the given string *)
let run str env =
  let token_res = Scanner.scan_tokens str in
  let parse_res = Result.bind token_res (Parser.parse [] []) in
  let interpret_res = Result.bind parse_res (Interpreter.interpret env) in
  match interpret_res with
  | Error err_list ->
      print_error_list err_list ; env
  | Ok (value, new_env) ->
      print_endline (Value.string_of_value value) ;
      new_env

(** Tries to read a line from the given input channel and catches the error *)
let try_read ic = try Some (input_line ic) with End_of_file -> None

(** Runs the repl *)
let run_prompt () =
  let rec do_repl env =
    print_string "> " ;
    flush stdout ;
    match try_read stdin with
    | None ->
        print_endline "Session Ended"
    | Some s ->
        let new_env = run s env in
        do_repl new_env
  in
  do_repl (Environ.create_environ ())

(** Reads in all the lines in a given file and calls run *)
let run_file file_name =
  let ic = open_in file_name in
  let rec loop acc =
    match try_read ic with
    | Some s ->
        loop (s :: acc)
    | None ->
        close_in ic ; List.rev acc
  in
  let file = String.concat " " (loop []) in
  ignore (run file (Environ.create_environ ()))

(** Calls the different read command depending on the number of args *)
let parse_args =
  match Array.length Sys.argv with
  | 1 ->
      run_prompt ()
  | 2 ->
      run_file Sys.argv.(1)
  | _ ->
      print_endline "Usage: olox [script]" ;
      exit 64
