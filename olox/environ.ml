open Printf

(* type definitions *)
module StringMap = Map.Make (String)

type environ = value StringMap.t list * global_env

and global_env = value StringMap.t

and value =
  | StringValue of string
  | FloatValue of float
  | BoolValue of bool
  | FunctionValue of
      { arity: int
      ; to_call:
             value list
          -> global_env
          -> (value * global_env, Reporting.error_record list) result
      ; name: string }
  | NilValue

(* value functions *)
let string_of_value = function
  | StringValue str ->
      sprintf "String: '%s'" str
  | FloatValue fl ->
      sprintf "Float: %f" fl
  | BoolValue bl ->
      sprintf "Bool: %b" bl
  | FunctionValue func ->
      sprintf "Function<%s:%i>" func.name func.arity
  | NilValue ->
      "Nil"

let stringify = function
  | StringValue str ->
      str
  | FloatValue fl ->
      sprintf "%f" fl
  | BoolValue bl ->
      sprintf "%b" bl
  | FunctionValue func ->
      sprintf "%s:%i" func.name func.arity
  | NilValue ->
      "Nil"

(* environ functions *)
let from_global global_env = ([], global_env)

let define env name value =
  let scope_env, global_env = env in
  match scope_env with
  | [] ->
      ([], StringMap.add name value global_env)
  | head :: rest ->
      (StringMap.add name value head :: rest, global_env)

let create_environ () =
  let new_env = ([], StringMap.empty) in
  define new_env "clock"
    (FunctionValue
       { arity= 0
       ; name= "clock"
       ; to_call= (fun _ global -> Ok (FloatValue (Unix.time ()), global)) })

let get env name =
  let rec search_scope scope_env =
    match scope_env with
    | [] ->
        None
    | head :: rest -> (
      match StringMap.find_opt name head with
      | None ->
          search_scope rest
      | Some x ->
          Some x )
  in
  let scope_env, global_env = env in
  match search_scope scope_env with
  | Some value ->
      Some value
  | None ->
      StringMap.find_opt name global_env

let contains env name =
  let rec seach_scope scope_env =
    match scope_env with
    | [] ->
        false
    | head :: rest -> (
      match StringMap.find_opt name head with
      | Some _ ->
          true
      | None ->
          seach_scope rest )
  in
  let scope_env, global_env = env in
  match seach_scope scope_env with
  | true ->
      true
  | false -> (
    match StringMap.find_opt name global_env with
    | Some _ ->
        true
    | None ->
        false )

let assign env name value =
  match contains env name with
  | false ->
      None
  | true ->
      Some (define env name value)

let push_env env =
  let scope_env, global_env = env in
  (StringMap.empty :: scope_env, global_env)

let pop_env env =
  let scope_env, global_env = env in
  match scope_env with
  | [] ->
      raise (Failure "Tried to pop empty environ!")
  | _ :: rest ->
      (rest, global_env)

let get_global env =
  let _, global_env = env in
  global_env

let apply_global env global_env =
  let scope_env, _ = env in
  (scope_env, global_env)