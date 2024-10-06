module JsEmitter

open Parser

let rec emitExpression (expr: Expression) =
    match expr with
    | Number n -> string n
    | Variable v -> v
    | BinaryOperation (left, op, right) ->
        sprintf "(%s %s %s)" (emitExpression left) op (emitExpression right)

let rec emitStatement (stmt: Statement) =
    match stmt with
    | Return expr -> sprintf "return %s;" (emitExpression expr)
    | If (cond, body) ->
        sprintf "if (%s) { %s }" (emitExpression cond) (emitStatement body)
    | Block stmts ->
        let body = stmts |> List.map emitStatement |> String.concat " "
        sprintf "{ %s }" body

let emitFunction (func: Function) =
    let parameters = func.Parameters |> List.map snd |> String.concat ", "
    let body = func.Body |> List.map emitStatement |> String.concat " "
    sprintf "function %s(%s) { %s }" func.Name parameters body

let emitClass (cls: Class) =
    let members = cls.Members |> List.map (fun (t, n) -> sprintf "this.%s = null;" n) |> String.concat " "
    sprintf "class %s { constructor() { %s } }" cls.Name members

open Parser

let emitTopLevel (decl: Declaration) =
    match decl with
    | ClassDecl cls -> emitClass cls
    | FunctionDecl func -> emitFunction func
