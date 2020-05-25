(** This module provides the olox implementation of the lox scanner. For the moment, it only
    provides a simple string->token interface. The token types are stored in the {!Olox.Token}
    module. *)

val scan_tokens :
  string -> (Token.token list, Reporting.error_record list) result
(** Given a string, scans all tokens. Assumes that the end of the string is the end of the last
    token, so always make sure to feed full tokens. Read {!Olox.Token} for more information on
    what tokens look like. *)
