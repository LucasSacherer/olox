open Printf

type literal_type =
  | StringLiteral of string
  | FloatLiteral of float
  | BoolLiteral of bool
  | NilLiteral

let string_of_literal = function
  | StringLiteral str ->
      sprintf "String Literal '%s'" str
  | FloatLiteral flt ->
      sprintf "Float Literal '%f'" flt
  | BoolLiteral bl ->
      sprintf "Bool Literal '%B'" bl
  | NilLiteral ->
      "Nil Literal"

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
  | Super of {keyword: Token.token; meth: Token.token}
  | This of {keyword: Token.token}

type statement =
  | Statement of {expr: expression}
  | PrintStmt of {expr: expression}
  | ReturnStmt of {keyword: Token.token; expr: expression option}
  | VarStmt of {name: Token.token; init: expression option}
  | BlockStmt of statement list
  | ClassStmt of
      {name: Token.token; superclass: expression option; methods: func_def list}
  | IfStmt of
      { condition: expression
      ; then_branch: statement
      ; else_branch: statement option }
  | WhileStmt of {condition: expression; body: statement}
  | FuncStmt of func_def

and func_def = {name: Token.token; params: Token.token list; body: statement}

let rec string_of_expression = function
  | Assign assi ->
      sprintf "(assign var '%s' = %s)" assi.name.lexeme
        (string_of_expression assi.expr)
  | Binary bin ->
      sprintf "(<bin> %s %s %s)"
        (string_of_expression bin.left)
        bin.operator.lexeme
        (string_of_expression bin.right)
  | Call call ->
      sprintf "(calle: %s with args: %s)"
        (string_of_expression call.callee)
        (String.concat ", " (List.map string_of_expression call.arguments))
  | Get get ->
      sprintf "(get: %s from: %s)" get.name.lexeme
        (string_of_expression get.obj)
  | Logical log ->
      sprintf "(<log> %s %s %s)"
        (string_of_expression log.left)
        log.operator.lexeme
        (string_of_expression log.right)
  | Grouping grp ->
      sprintf "(group %s)" (string_of_expression grp.expr)
  | Unary un ->
      sprintf "(%s %s)" un.operator.lexeme (string_of_expression un.right)
  | Literal lit ->
      string_of_literal lit
  | Set set ->
      sprintf "(set: %s in: %s to: %s)" set.name.lexeme
        (string_of_expression set.obj)
        (string_of_expression set.value)
  | Variable var ->
      sprintf "(variable '%s')" var.name.lexeme
  | Super sup ->
      sprintf "(super call:%s)" sup.meth.lexeme
  | This _ ->
      "(this)"

let rec string_of_statement stmt =
  match stmt with
  | Statement stmt ->
      sprintf "Statement:(%s)" (string_of_expression stmt.expr)
  | PrintStmt stmt ->
      sprintf "Print:(%s)" (string_of_expression stmt.expr)
  | ReturnStmt stmt -> (
    match stmt.expr with
    | Some expr ->
        sprintf "Return:(%s)" (string_of_expression expr)
    | None ->
        "Return:None" )
  | VarStmt stmt ->
      sprintf "Var:(%s = %s)" stmt.name.lexeme
        ( match stmt.init with
        | Some expr ->
            string_of_expression expr
        | None ->
            "<no init>" )
  | BlockStmt stmt_list ->
      sprintf "Block:(%s)"
        (String.concat ", " (List.map string_of_statement stmt_list))
  | IfStmt stmt ->
      sprintf "If:(cond:%s then:%s else:%s)"
        (string_of_expression stmt.condition)
        (string_of_statement stmt.then_branch)
        ( match stmt.else_branch with
        | None ->
            "None"
        | Some else_stmt ->
            string_of_statement else_stmt )
  | WhileStmt stmt ->
      sprintf "While:(cond:%s body:%s)"
        (string_of_expression stmt.condition)
        (string_of_statement stmt.body)
  | FuncStmt stmt ->
      string_of_function_def stmt
  | ClassStmt stmt ->
      sprintf "Class:(name:%s super:%s methods:{%s})" stmt.name.lexeme
        ( match stmt.superclass with
        | Some super ->
            string_of_expression super
        | None ->
            "()" )
        (String.concat "," (List.map string_of_function_def stmt.methods))

and string_of_function_def func_def =
  sprintf "Function:(name:%s args:{%s} body:%s)" func_def.name.lexeme
    (String.concat ", "
       (List.map (fun tok -> tok.Token.lexeme) func_def.params))
    (string_of_statement func_def.body)

let string_of_stmt_list stmt_list =
  let rec loop stmt_list acc =
    match stmt_list with
    | [] ->
        String.concat ";" (List.rev acc)
    | head :: rest ->
        loop rest (string_of_statement head :: acc)
  in
  loop stmt_list []
