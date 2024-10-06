module LexerTests

open Xunit
open Lexer

[<Fact>]
let ``Test tokenize with keywords and identifiers`` () =
    let code = "public static int CalculatePrice"
    let expectedTokens = [
        Keyword "public"
        Keyword "static"
        Keyword "int"
        Identifier "CalculatePrice"
    ]
    let tokens = tokenize code
    Assert.Equal<Token list>(expectedTokens, tokens)

[<Fact>]
let ``Test tokenize with numbers and operators`` () =
    let code = "val + 100"
    let expectedTokens = [
        Identifier "val"
        Operator "+"
        Number 100
    ]
    let tokens = tokenize code
    Assert.Equal(expectedTokens, tokens)

[<Fact>]
let ``Test tokenize with parentheses and braces`` () =
    let code = "(val) { }"
    let expectedTokens = [
        OpenParen
        Identifier "val"
        CloseParen
        OpenBrace
        CloseBrace
    ]
    let tokens = tokenize code
    Assert.Equal(expectedTokens, tokens)
