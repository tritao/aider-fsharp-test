module JsEmitterTests

open Xunit
open Parser
open JsEmitter

[<Fact>]
let ``Test emit function`` () =
    let func = {
        Name = "calculate"
        Parameters = [("int", "x"); ("int", "y")]
        Body = [Return (BinaryOperation (Variable "x", "+", Variable "y"))]
    }
    let expectedJs = "function calculate(x, y) { return (x + y); }"
    let emittedJs = emitFunction func
    Assert.Equal(expectedJs, emittedJs)

[<Fact>]
let ``Test emit class`` () =
    let cls = {
        Name = "Options"
        Members = [("int", "x"); ("int", "y")]
    }
    let expectedJs = "class Options { constructor() { this.x = null; this.y = null; } }"
    let emittedJs = emitClass cls
    Assert.Equal(expectedJs, emittedJs)

[<Fact>]
let ``Test emit top-level function declaration`` () =
    let funcDecl = FunctionDecl {
        Name = "calculate"
        Parameters = [("int", "x"); ("int", "y")]
        Body = [Return (BinaryOperation (Variable "x", "+", Variable "y"))]
    }
    let expectedJs = "function calculate(x, y) { return (x + y); }"
    let emittedJs = emitTopLevel funcDecl
    Assert.Equal(expectedJs, emittedJs)

[<Fact>]
let ``Test emit top-level class declaration`` () =
    let classDecl = ClassDecl {
        Name = "Options"
        Members = [("int", "x"); ("int", "y")]
    }
    let expectedJs = "class Options { constructor() { this.x = null; this.y = null; } }"
    let emittedJs = emitTopLevel classDecl
    Assert.Equal(expectedJs, emittedJs)
