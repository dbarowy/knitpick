open Parser
open Evaluator
open System.IO

[<EntryPoint>]
let main args = 
    let s = System.IO.File.ReadAllText "test.kpp"

    let result = parse s

    match result with
    | Some ast ->
        constructDoc ast
        exit 0
    | None -> 
        exit 1
    0    