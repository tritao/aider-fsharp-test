module Lexer

type Token =
    | Keyword of string
    | Identifier of string
    | Number of int
    | Operator of string
    | OpenParen
    | CloseParen
    | OpenBrace
    | CloseBrace
    | Semicolon
    | Unknown of char

let tokenize (input: string) =
    let rec tokenize' chars tokens =
        match chars with
        | [] -> List.rev tokens
        | ' ' :: rest -> tokenize' rest tokens
        | '(' :: rest -> tokenize' rest (OpenParen :: tokens)
        | ')' :: rest -> tokenize' rest (CloseParen :: tokens)
        | '{' :: rest -> tokenize' rest (OpenBrace :: tokens)
        | '}' :: rest -> tokenize' rest (CloseBrace :: tokens)
        | ';' :: rest -> tokenize' rest (Semicolon :: tokens)
        | c :: rest when System.Char.IsDigit(c) ->
            let number, rest' = chars |> List.span System.Char.IsDigit
            let numStr = System.String.Concat(number)
            tokenize' rest' (Number (int numStr) :: tokens)
        | c :: rest when System.Char.IsLetter(c) ->
            let identifier, rest' = chars |> List.span System.Char.IsLetterOrDigit
            let idStr = System.String.Concat(identifier)
            let token = 
                match idStr with
                | "public" | "static" | "int" | "return" -> Keyword idStr
                | _ -> Identifier idStr
            tokenize' rest' (token :: tokens)
        | c :: rest when "+-*/".Contains(c) ->
            tokenize' rest (Operator (string c) :: tokens)
        | c :: rest -> tokenize' rest (Unknown c :: tokens)

    tokenize' (List.ofSeq input) []

