open Printf

type literal_type =
  | StringLiteral of string
  | FloatLiteral of float
  | BoolLiteral of bool
  | NilLiteral

let string_of_literal = function
  | StringLiteral str -> sprintf ("String Literal '%s'") str
  | FloatLiteral flt -> sprintf ("Float Literal '%f'") flt
  | BoolLiteral bl -> sprintf ("Bool Literal '%B'") bl
  | NilLiteral -> "Nil Literal"

type expression =
  | Assign of {name: Token.token; expr: expression}
  | Binary of {left: expression; operator: Token.token; right: expression }
  | Grouping of {expr: expression}
  | Unary of {operator: Token.token; right: expression}
  | Literal of literal_type
  | Variable of {name: Token.token}

type statement =
  | Statement of {expr: expression}
  | PrintStmt of {expr: expression}
  | VarStmt of {name: Token.token; init: expression option}

let rec string_of_expression expr =
  String.concat " " (match expr with
  | Assign assi -> ["("; "assign var '"; assi.name.lexeme; "' = ";
                    string_of_expression assi.expr; ")"]
  | Binary bin -> ["("; string_of_expression bin.left; bin.operator.lexeme;
                   string_of_expression bin.right; ")"]
  | Grouping grp -> ["("; "group"; string_of_expression grp.expr; ")"]
  | Unary un -> ["("; un.operator.lexeme; string_of_expression un.right; ")"]
  | Literal lit -> [string_of_literal lit]
  | Variable var -> ["("; "variable '"; var.name.lexeme; "')"])

let string_of_statement stmt =
  String.concat " " (match stmt with
  | Statement stmt -> ["Statement:("; string_of_expression stmt.expr; ")"]
  | PrintStmt stmt -> ["Print:("; string_of_expression stmt.expr; ")"]
  | VarStmt stmt -> ["Var:("; stmt.name.lexeme; " = "; 
                     (match stmt.init with
                      | Some expr -> string_of_expression expr
                      | None -> "<no init>"); 
                     ")"])

let rec print_stmt_list = function
  | [] -> ()
  | stmt::rest -> print_endline (string_of_statement stmt); print_stmt_list rest
