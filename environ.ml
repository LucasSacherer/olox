module StringMap = Map.Make(String)

type environ = Value.value StringMap.t 

let create_environ () = StringMap.empty

let define env name value = StringMap.add name value env

let get env name = StringMap.find_opt name env

let contains env name =
  match StringMap.find_opt name env with
  | Some _ -> true
  | None -> false
