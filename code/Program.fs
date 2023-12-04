open Parser
open Evaluator
open System.IO

[<EntryPoint>]
let main args = 
    if Array.length args = 2 then 
        
        let s = System.IO.File.ReadAllText args[0]
        let result = parse s

        let name = args[1]

        match result with
        | Some ast ->
            constructDoc ast name
            exit 0
        | None -> 
            exit 1
        0
    else 
        0