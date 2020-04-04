val interpret :
     Environ.environ
  -> Ast.statement list
  -> (Environ.value * Environ.environ, Reporting.error_record list) result
