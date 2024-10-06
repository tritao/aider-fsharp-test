module Parser

open Lexer

type Expression =
    | Number of int
    | Variable of string
    | BinaryOperation of Expression * string * Expression

type Statement =
    | Return of Expression
    | If of Expression * Statement
    | Block of Statement list

type Function =
    { Name: string
      Parameters: (string * string) list
      Body: Statement list }

let parse (tokens: Token list) : Function =
    let rec parseExpression tokens =
        match tokens with
        | Number n :: rest -> Number n, rest
        | Identifier v :: rest -> Variable v, rest
        | _ -> failwith "Unexpected token in expression"

    let rec parseStatement tokens =
        match tokens with
        | Keyword "return" :: rest ->
            let expr, rest' = parseExpression rest
            Return expr, rest'
        | Keyword "if" :: OpenParen :: rest ->
            let expr, rest' = parseExpression rest
            match rest' with
            | CloseParen :: OpenBrace :: rest'' ->
                let stmt, rest''' = parseStatement rest''
                If (expr, stmt), rest'''
            | _ -> failwith "Expected closing parenthesis and opening brace"
        | OpenBrace :: rest ->
            let rec parseBlock tokens acc =
                match tokens with
                | CloseBrace :: rest' -> Block (List.rev acc), rest'
                | _ ->
                    let stmt, rest' = parseStatement tokens
                    parseBlock rest' (stmt :: acc)
            parseBlock rest []
        | _ -> failwith "Unexpected token in statement"

    let rec parseParameters tokens acc =
        match tokens with
        | Identifier t :: Identifier n :: rest ->
            let param = (t, n)
            match rest with
            | Comma :: rest' -> parseParameters rest' (param :: acc)
            | CloseParen :: rest' -> List.rev (param :: acc), rest'
            | _ -> failwith "Unexpected token in parameters"
        | _ -> failwith "Unexpected token in parameters"

    match tokens with
    | Keyword "public" :: Keyword "static" :: Keyword "int" :: Identifier name :: OpenParen :: rest ->
        let parameters, rest' = parseParameters rest []
        match rest' with
        | OpenBrace :: rest'' ->
            let body, _ = parseStatement rest''
            { Name = name; Parameters = parameters; Body = [body] }
        | _ -> failwith "Expected opening brace"
    | _ -> failwith "Unexpected token in function declaration"
