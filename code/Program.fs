open Parser
open Evaluator
open System.IO

[<EntryPoint>]
let main args = 
    
    let s = "yarn \"alpaca\" 4"
    let result = parse s

    match result with 
    | Some ast -> 
        printfn "%A" (string ast)
        exit 0
    | None ->
        printfn "parse failed"
        exit 1
    
    (*
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
    *)