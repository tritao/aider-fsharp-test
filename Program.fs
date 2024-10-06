// For more information see https://aka.ms/fsharp-console-apps
module Program

open Lexer

let code = "public static int CalculatePrice(int val, int val2) { if (val < 50) return 100; return val * 500; }"
let tokens = tokenize code
printfn "%A" tokens
