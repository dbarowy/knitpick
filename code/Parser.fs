module Parser

open Combinator
open AST

let pstitchseq, pstitchseqImpl = recparser()

let pinteger : Parser<Int> = pmany1 pdigit |>> (fun ds -> stringify ds |> int)
let pstring : Parser<String> = pmany1 pitem |>> (fun cs -> stringify cs)

let pad p = pbetween pws0 p pws0

let ppurl : Parser<Stitch> = pchar 'p' |>> (fun _ -> ("purl", 0))
let pknit : Parser<Stitch> = pchar 'k' |>> (fun _ -> ("knit", 0))

let pstitch : Parser<Stitch> = pknit <|> ppurl

let pstitchrep : Parser<StitchSeq> = pseq pstitch pinteger (fun (a, b) -> (a, b)) |>> (fun (a, b) -> StitchRep (a, b))

let pstitchseqtoend : Parser<StitchSeq> = pright (pchar '+') (pstitchseq) |>> (fun s -> StitchSeqToEnd (s))

let ptwostitchseq : Parser<StitchSeq> = pbetween (pchar '(') (pseq (pstitchseq) (pstitchseq) (fun (a, b) -> (a, b)))  (pchar ')') |>> (fun tup -> TwoStitchSeq (tup))

pstitchseqImpl := pstitchrep <|> pstitchseqtoend <|> ptwostitchseq

let prow : Parser<Row> = pmany1 (pleft (pad pstitchseq) pws0)

let pinst : Parser<Instruction> 

let grammar = pleft prow peof

let parse (s: string) = 
    let input = prepare s
    match grammar input with
    | Success(ast, _) -> Some ast
    | Failure(_, _) -> None

