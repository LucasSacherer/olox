open Printf

type literal_type =
  | StringLiteral of string
  | FloatLiteral of float
  | IdentLiteral of string
  | BoolLiteral of bool
  | NilLiteral

let string_of_literal = function
  | StringLiteral str -> sprintf ("String Literal '%s'") str
  | FloatLiteral flt -> sprintf ("Float Literal '%f'") flt
  | IdentLiteral id -> sprintf ("Ident Literal '%s'") id
  | BoolLiteral bl -> sprintf ("Bool Literal '%B'") bl
  | NilLiteral -> "Nil Literal"

type expression =
  | Binary of { left: expression; operator: Token.token; right: expression }
  | Grouping of {expr: expression}
  | Unary of {operator: Token.token; right: expression}
  | Literal of literal_type

type statement =
  | Expression of {expr: expression}
  | PrintExpr of {expr: expression}

let rec string_of_expression expr =
  String.concat " " (match expr with
  | Binary bin -> ["("; string_of_expression bin.left; bin.operator.lexeme;
                   string_of_expression bin.right; ")"]
  | Grouping grp -> ["("; "group"; string_of_expression grp.expr; ")"]
  | Unary un -> ["("; un.operator.lexeme; string_of_expression un.right; ")"]
  | Literal lit -> [string_of_literal lit])

let string_of_statement stmt =
  String.concat " " (match stmt with
  | Expression expr -> ["Statement:("; string_of_expression expr.expr; ")"]
  | PrintExpr expr -> ["Print:("; string_of_expression expr.expr; ")"])
