type error_record =
  { line: int;
    where: string;
    message: string;
  }

(* prints out a list of error_record types, also exits the program *)
let rec print_error_list = function
  | error :: rest ->
      print_endline (String.concat "" ["[Line "; 
        string_of_int error.line; "] Error"; error.where;
        ": "; error.message;]);
      print_error_list rest
  | [] -> ()
