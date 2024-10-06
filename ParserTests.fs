module ParserTests

open Xunit
open Lexer
open Parser

[<Fact>]
let ``Test parse function declaration`` () =
    let tokens = [
        Keyword "public"
        Keyword "static"
        Keyword "int"
        Identifier "CalculatePrice"
        OpenParen
        Identifier "int"
        Identifier "val"
        Comma
        Identifier "int"
        Identifier "val2"
        CloseParen
        OpenBrace
        Keyword "return"
        Token.Number 100
        Semicolon
        CloseBrace
    ]
    let expectedFunction = {
        Name = "CalculatePrice"
        Parameters = [("int", "val"); ("int", "val2")]
        Body = [Return (Number 100)]
    }
    let parsedFunction = parse tokens
    Assert.Equal(expectedFunction, parsedFunction)

[<Fact>]
let ``Test parse if statement`` () =
    let tokens = [
        Keyword "if"
        OpenParen
        Identifier "val"
        Operator "<"
        Token.Number 50
        CloseParen
        OpenBrace
        Keyword "return"
        Token.Number 100
        Semicolon
        CloseBrace
    ]
    let expectedStatement = If (
        BinaryOperation (Variable "val", "<", Number 50),
        Return (Number 100)
    )
    let parsedStatement, _ = parseStatement tokens
    Assert.Equal(expectedStatement, parsedStatement)

[<Fact>]
let ``Test parse binary operation expression`` () =
    let tokens = [
        Identifier "val"
        Operator "<"
        Token.Number 50
    ]
    let expectedExpression = BinaryOperation (Variable "val", "<", Number 50)
    let parsedExpression, _ = parseExpression tokens
    Assert.Equal(expectedExpression, parsedExpression)

[<Fact>]
let ``Test parse return statement`` () =
    let tokens = [
        Keyword "return"
        Token.Number 500
        Semicolon
    ]
    let expectedStatement = Return (Number 500)
    let parsedStatement, _ = parseStatement tokens
    Assert.Equal(expectedStatement, parsedStatement)
