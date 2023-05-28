open System
open Lexer

[<EntryPoint>]
let main argv =
    let lexer = Lexer.init "let inc = fun(x: int): int is x + 1;;"
    for token in lex lexer do
        printfn "%A" token
    0
