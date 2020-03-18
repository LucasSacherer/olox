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
  | Logical of {left: expression; operator: Token.token; right: expression}
  | Variable of {name: Token.token}

type statement =
  | Statement of {expr: expression}
  | PrintStmt of {expr: expression}
  | VarStmt of {name: Token.token; init: expression option}
  | BlockStmt of statement list
  | IfStmt of {condition: expression; then_branch: statement; else_branch: statement option}
  | WhileStmt of {condition: expression; body: statement}

let rec string_of_expression expr =
  String.concat " " (match expr with
  | Assign assi -> ["("; "assign var '"; assi.name.lexeme; "' = ";
                    string_of_expression assi.expr; ")"]
  | Binary bin -> ["("; string_of_expression bin.left; bin.operator.lexeme;
                   string_of_expression bin.right; ")"]
  | Logical log -> ["("; string_of_expression log.left; log.operator.lexeme;
                   string_of_expression log.right; ")"]
  | Grouping grp -> ["("; "group"; string_of_expression grp.expr; ")"]
  | Unary un -> ["("; un.operator.lexeme; string_of_expression un.right; ")"]
  | Literal lit -> [string_of_literal lit]
  | Variable var -> ["("; "variable '"; var.name.lexeme; "')"])

let rec string_of_statement stmt =
  String.concat " " (match stmt with
    | Statement stmt -> ["Statement:("; string_of_expression stmt.expr; ")"]
    | PrintStmt stmt -> ["Print:("; string_of_expression stmt.expr; ")"]
    | VarStmt stmt -> ["Var:("; stmt.name.lexeme; " = "; 
                       (match stmt.init with
                        | Some expr -> string_of_expression expr
                        | None -> "<no init>"); 
                       ")"]
    | BlockStmt stmt_list -> ["Block:("; 
                              String.concat ", " (List.map string_of_statement stmt_list);
                              ")"]
    | IfStmt stmt -> ["If:(cond: "; string_of_expression stmt.condition; " then: ";
                      string_of_statement stmt.then_branch; " else: ";
                      (match stmt.else_branch with
                      | None -> "None"
                      | Some else_stmt -> string_of_statement else_stmt);
                      ")"]
    | WhileStmt stmt -> ["If:(cond: "; string_of_expression stmt.condition; " body: ";
                         string_of_statement stmt.body; ")"]
  )

let rec print_stmt_list = function
  | [] -> ()
  | stmt::rest -> print_endline (string_of_statement stmt); print_stmt_list rest
