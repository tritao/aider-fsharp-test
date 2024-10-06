// For more information see https://aka.ms/fsharp-console-apps
module Program

open Lexer

let rec repl () =
    printf "Enter code: "
    let input = System.Console.ReadLine()
    if input <> "exit" then
        let tokens = tokenize input
        printfn "%A" tokens
        repl ()

[<EntryPoint>]
let main argv =
    printfn "F# REPL - Type 'exit' to quit."
    repl ()
    0
