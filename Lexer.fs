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
    | Comma
    | Semicolon
    | Unknown of char


let tokenize (input: string) =
    let rec tokenize' chars tokens =
        match chars with
        | [] -> List.rev tokens
        | c :: rest when System.Char.IsWhiteSpace(c) -> tokenize' rest tokens
        | '(' :: rest -> tokenize' rest (OpenParen :: tokens)
        | ')' :: rest -> tokenize' rest (CloseParen :: tokens)
        | '{' :: rest -> tokenize' rest (OpenBrace :: tokens)
        | '}' :: rest -> tokenize' rest (CloseBrace :: tokens)
        | ';' :: rest -> tokenize' rest (Semicolon :: tokens)
        | c :: rest when System.Char.IsDigit(c) ->
            let number = List.takeWhile System.Char.IsDigit chars
            let rest' = List.skipWhile System.Char.IsDigit chars
            let numStr = System.String.Concat(number |> List.map string)
            tokenize' rest' (Number (int numStr) :: tokens)
        | c :: rest when System.Char.IsLetter(c) ->
            let identifier = List.takeWhile System.Char.IsLetterOrDigit chars
            let rest' = List.skipWhile System.Char.IsLetterOrDigit chars
            let idStr = System.String.Concat(identifier |> List.map string)
            let token = 
                match idStr with
                | "public" | "static" | "int" | "return" | "class" -> Keyword idStr
                | _ -> Identifier idStr
            tokenize' rest' (token :: tokens)
        | c :: rest when "+-*/".Contains(c) ->
            tokenize' rest (Operator (string c) :: tokens)
        | c :: rest -> tokenize' rest (Unknown c :: tokens)

    tokenize' (List.ofSeq input) []

