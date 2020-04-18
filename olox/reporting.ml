type error_record = {line: int; where: string; message: string}

let string_of_error_record record =
  Printf.sprintf "[Line %i] Error%s: %s" record.line record.where record.message

let rec print_error_list = function
  | error :: rest ->
      print_endline (string_of_error_record error) ;
      print_error_list rest
  | [] ->
      ()

let string_of_error_list error_list =
  let rec loop error_list acc =
    match error_list with
    | [] ->
        String.concat ";" (List.rev acc)
    | head :: rest ->
        loop rest (string_of_error_record head :: acc)
  in
  loop error_list []
