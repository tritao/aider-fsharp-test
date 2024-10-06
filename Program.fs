// For more information see https://aka.ms/fsharp-console-apps
module Program

open System.IO
open Lexer
open Parser
open JsEmitter
open Parser
open JsEmitter

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "Usage: dotnet run <file-path>"
        1
    else
        let filePath = argv.[0]
        if File.Exists(filePath) then
            let input = File.ReadAllText(filePath)
            let tokens = tokenize input
            printfn "Tokens: %A" tokens
            let ast, _ = parse tokens
            let jsOutput = emitTopLevel ast
            printfn "%s" jsOutput
            0
        else
            printfn "File not found: %s" filePath
            1
