val parse :
     Ast.statement list
  -> Reporting.error_record list
  -> Token.token list
  -> (Ast.statement list, Reporting.error_record list) result
