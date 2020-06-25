type literal_type =
  | StringLiteral of string
  | FloatLiteral of float
  | BoolLiteral of bool
  | NilLiteral

type expression =
  | Assign of {name: Token.token; expr: expression}
  | Binary of {left: expression; operator: Token.token; right: expression}
  | Call of {callee: expression; paren: Token.token; arguments: expression list}
  | Get of {obj: expression; name: Token.token}
  | Grouping of {expr: expression}
  | Unary of {operator: Token.token; right: expression}
  | Literal of literal_type
  | Logical of {left: expression; operator: Token.token; right: expression}
  | Set of {obj: expression; name: Token.token; value: expression}
  | Variable of {name: Token.token}
  | This of {keyword: Token.token}

type statement =
  | Statement of {expr: expression}
  | PrintStmt of {expr: expression}
  | ReturnStmt of {keyword: Token.token; expr: expression option}
  | VarStmt of {name: Token.token; init: expression option}
  | BlockStmt of statement list
  | ClassStmt of {name: Token.token; methods: func_def list}
  | IfStmt of
      { condition: expression
      ; then_branch: statement
      ; else_branch: statement option }
  | WhileStmt of {condition: expression; body: statement}
  | FuncStmt of func_def

and func_def = {name: Token.token; params: Token.token list; body: statement}

val string_of_literal : literal_type -> string

val string_of_expression : expression -> string

val string_of_statement : statement -> string

val string_of_stmt_list : statement list -> string
