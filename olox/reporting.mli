type error_record = {line: int; where: string; message: string}

val print_error_list : error_record list -> unit
