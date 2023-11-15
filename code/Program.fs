open Parser
open Evaluator
open System.IO

[<EntryPoint>]
let main args = 
    let s = "+(p1(k3p2))"

    let result = parse s

    

    match result with
    | Some sseq ->
        constructDoc sseq
        exit 0
    | None -> 
        exit 1
    0    