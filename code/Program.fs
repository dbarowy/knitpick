open Parser
open Evaluator
open System.IO

[<EntryPoint>]
let main args = 
    
    // to print AST only
    (* let s = System.IO.File.ReadAllText "headertest.kpp"
    let result = parse s

    match result with 
    | Some ast -> 
        printfn "%A" (string ast)
        exit 0
    | None ->
        printfn "parse failed"
        exit 1
    *)
    
    // to construct entire document
    if Array.length args = 2 then 
        
        let s = 
            try
                Some (System.IO.File.ReadAllText args[0])
            with 
                | :? System.IO.FileNotFoundException -> printfn "File %s not found." args[0]; None
        
        let result = 
            match s with 
            | Some s -> parse s
            | None -> None

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