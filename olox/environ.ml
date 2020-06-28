open Printf

(* type definitions *)
module StringMap = Map.Make (String)

type environ = value StringMap.t list * global_env

and global_env = value StringMap.t

and class_inst = {desc: class_desc; mutable class_env: value StringMap.t}

and value =
  | StringValue of string
  | FloatValue of float
  | BoolValue of bool
  | FunctionValue of func_desc * class_inst option
  | ClassValue of class_desc
  | ClassInstance of class_inst
  | ReturnValue of value * int
  | NilValue

and class_desc =
  {class_name: string; superclass: class_desc option; methods: func_desc list}

and func_desc =
  { arity: int
  ; mutable to_call:
         value list
      -> global_env
      -> class_inst option
      -> (value * global_env, Reporting.error_record list) result
  ; func_name: string }

(* value functions *)
let rec string_of_value = function
  | StringValue str ->
      sprintf "String: '%s'" str
  | FloatValue fl ->
      sprintf "Float: %f" fl
  | BoolValue bl ->
      sprintf "Bool: %b" bl
  | FunctionValue (func, _) ->
      print_func_desc func
  | ClassValue class_desc ->
      print_class_desc class_desc
  | ClassInstance inst ->
      print_class_inst inst
  | ReturnValue (value, _) ->
      sprintf "Return: {%s}" (string_of_value value)
  | NilValue ->
      "Nil"

and print_class_desc desc =
  sprintf "Class<name:%s super:%s methods:{%s}>" desc.class_name
    ( match desc.superclass with
    | None ->
        "()"
    | Some super ->
        print_class_desc super )
    (String.concat ";" (List.map print_func_desc desc.methods))

and print_func_desc desc = sprintf "Function<%s:%i>" desc.func_name desc.arity

and print_class_inst inst = sprintf "Instance<%s>" (print_class_desc inst.desc)

let rec stringify = function
  | StringValue str ->
      str
  | FloatValue fl ->
      sprintf "%f" fl
  | BoolValue bl ->
      sprintf "%b" bl
  | FunctionValue (func, _) ->
      sprintf "%s:%i" func.func_name func.arity
  | ClassValue klass ->
      stringify_class_desc klass
  | ClassInstance inst ->
      sprintf "instance:%s" (stringify_class_desc inst.desc)
  | ReturnValue (value, _) ->
      sprintf "return:%s" (stringify value)
  | NilValue ->
      "Nil"

and stringify_class_desc desc = sprintf "%s" desc.class_name

(* Functions on class instances or descriptions *)
let create_class_inst desc = ClassInstance {desc; class_env= StringMap.empty}

let rec get_method class_desc name =
  match
    List.find_opt
      (fun meth -> String.equal meth.func_name name)
      class_desc.methods
  with
  | Some _ as func_desc ->
      func_desc
      (* If we get a function out, we need to give it the class_inst as 'this' *)
  | _ -> (
    match class_desc.superclass with
    | None ->
        None
    | Some super ->
        get_method super name )

let get_property class_inst name =
  match StringMap.find_opt name class_inst.class_env with
  | Some _ as value ->
      value
  | None -> (
    match get_method class_inst.desc name with
    | None ->
        None
    | Some desc ->
        Some (FunctionValue (desc, Some class_inst)) )

let set_property class_inst name value =
  class_inst.class_env <- StringMap.add name value class_inst.class_env

(* environ functions *)
let from_global global_env = ([], global_env)

let get_init_arity class_desc =
  match
    List.find_opt
      (fun meth -> String.equal meth.func_name "init")
      class_desc.methods
  with
  | None ->
      0
  | Some init_func ->
      init_func.arity

let get_init_func class_inst =
  match get_property class_inst "init" with
  | Some (FunctionValue (_, _)) as func ->
      func
  | _ ->
      None

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
       ( { arity= 0
         ; func_name= "clock"
         ; to_call= (fun _ global _ -> Ok (FloatValue (Unix.time ()), global))
         }
       , None ))

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
  if not (contains env name) then None
  else
    (* peel context until we find the one with the definition *)
    let scope_env, global_env = env in
    let rec assign_loop prev rest =
      match rest with
      | [] ->
          Some (prev, StringMap.add name value global_env)
      | top_scope :: rest -> (
        match StringMap.find_opt name top_scope with
        | Some _ ->
            let new_top_scope = StringMap.add name value top_scope in
            let new_scope_env = List.append prev (new_top_scope :: rest) in
            Some (new_scope_env, global_env)
        | None ->
            assign_loop (List.append prev [top_scope]) rest )
    in
    assign_loop [] scope_env

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
