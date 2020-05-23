(** This module provides the simple error reporting framework the olox uses. All
    the compiler modules return a result type with the error case wraping a
    {!Reporting.error_record} list. Once an error is detected, the utility functions
    in this module are used to display the errors to the user. *)

(** Type that stores an error to print out to the user. Use either 
    {!Reporting.print_error_list} or {!Reporting.string_of_error_list} to
    convert into a user visable form. *)
type error_record

val create_error : line:int -> where:string -> message:string -> error_record
(** Creates an error record which represents an error at a specific point in the source file.
  @param line At what line the error occured, use [-1] if not applicable.
  @param where Adds extra text after [error occured], use [""] if not used.
  @param message Message to give the user. *)

val print_error_list : error_record list -> unit
(** Prints out the error list to stdout. Each error gets its own new line. *)

val string_of_error_list : error_record list -> string
(** Converts a list of errors to a string. Errors are seperated by a semicolon. *)
