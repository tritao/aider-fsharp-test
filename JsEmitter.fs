module JsEmitter

open Parser

let emitExpression (expr: Expression) =
    match expr with
    | Number n -> string n
    | Variable v -> v
    | BinaryOperation (left, op, right) ->
        sprintf "(%s %s %s)" (emitExpression left) op (emitExpression right)

let emitStatement (stmt: Statement) =
    match stmt with
    | Return expr -> sprintf "return %s;" (emitExpression expr)
    | If (cond, body) ->
        sprintf "if (%s) { %s }" (emitExpression cond) (emitStatement body)
    | Block stmts ->
        let body = stmts |> List.map emitStatement |> String.concat " "
        sprintf "{ %s }" body

let emitFunction (func: Function) =
    let params = func.Parameters |> List.map snd |> String.concat ", "
    let body = func.Body |> List.map emitStatement |> String.concat " "
    sprintf "function %s(%s) %s" func.Name params body

let emitClass (cls: Class) =
    let members = cls.Members |> List.map (fun (t, n) -> sprintf "this.%s = null;" n) |> String.concat " "
    sprintf "class %s { constructor() { %s } }" cls.Name members

let emitTopLevel (decl: TopLevelDeclaration) =
    match decl with
    | TopLevelDeclaration.Class cls -> emitClass cls
    | TopLevelDeclaration.Function func -> emitFunction func
