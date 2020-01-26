(* lox interpreter written in ocaml *)
(* Writen by Lucas Sacherer *)

open Reporting

(** Runs the interpreter on the given string *)
let run str =
  let token_res = Scanner.scan_tokens str in
  let parse_res = Result.bind token_res Parser.parse in
  match parse_res with
  | Error err_list -> print_error_list err_list
  | Ok exp -> print_endline (Ast.string_of_expression exp)

(** Tries to read a line from the given input channel and catches the error *)
let try_read ic =
  try Some (input_line ic) with End_of_file -> None

(** Runs the repl *)
let run_prompt () =
  let rec do_repl () =
    print_string "> ";
    flush stdout;
    match try_read stdin with
    | Some s -> run s; do_repl ()
    | None -> print_endline "Session Ended"; in
  do_repl ()

(** Reads in all the lines in a given file and calls run *)
let run_file file_name =
  let ic = open_in file_name in
  let rec loop acc = match try_read ic with
  | Some s -> loop (s :: acc)
  | None -> close_in ic; List.rev acc in
  let file = String.concat " " (loop []) in
  run file

(** Calls the different read command depending on the number of args *)
let parse_args =
  match Array.length Sys.argv with
  | 1 -> run_prompt ()
  | 2 -> run_file (Array.get Sys.argv 1)
  | _ -> print_endline "Usage: olox [script]"; exit 64
