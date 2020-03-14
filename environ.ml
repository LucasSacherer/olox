module StringMap = Map.Make(String)

type environ = Value.value StringMap.t list

let create_environ () = StringMap.empty :: []

let define env name value = 
  match env with
  | [] -> raise (Failure "Tried to define on an empty env (impossible)!")
  | head::rest -> 
      (StringMap.add name value head) :: rest

let rec get env name =
  match env with
  | [] -> None
  | head::rest ->
  match StringMap.find_opt name head with
  | None -> get rest name
  | Some x -> Some x

let rec contains env name =
  match env with
  | [] -> false
  | head::rest ->
  match StringMap.find_opt name head with
  | Some _ -> true
  | None -> contains rest name

let rec assign env name value =
  match env with
  | [] -> None
  | head::rest ->
  match StringMap.find_opt name head with
  | Some _ -> Some ((StringMap.add name value head) :: rest)
  | None -> Option.map (fun new_rest -> head::new_rest) (assign rest name value)

let push_env env = StringMap.empty :: env

let pop_env env =
  match env with
  | [] -> raise (Failure "Tried to pop empty environ!")
  | _::[] -> raise (Failure "Tried to pop last environ!")
  | _::rest -> rest
