module Parser

open Combinator

type Stitch = string * int

type StitchSeq =
    | StitchRep of Stitch * int
    | StitchSeqToEnd of StitchSeq
    | TwoStitchSeq of StitchSeq * StitchSeq

let stitchseq, stitchseqImpl = recparser()

let integer = pmany1 pdigit |>> (fun ds -> stringify ds |> int)

let pad p = pbetween pws0 p pws0

let purl = pchar 'p' |>> (fun _ -> Stitch ("purl", 0))

let knit = pchar 'k' |>> (fun _ -> Stitch ("knit", 0))

let stitch = knit <|> purl

let stitchrep = pseq stitch integer (fun (a, b) -> (a, b)) |>> (fun (a, b) -> StitchRep (a, b))

let stitchseqtoend = pright (pchar '+') (stitchseq) |>> (fun s -> StitchSeqToEnd(s))

let twostitchseq = pbetween (pchar '(') (pseq (stitchseq) (stitchseq) (fun (a, b) -> (a, b)))  (pchar ')') |>> (fun tup -> TwoStitchSeq(tup))

stitchseqImpl := stitchrep <|> stitchseqtoend <|> twostitchseq

let grammar = pleft stitchseq peof

let parse (s: string) = 
    let input = prepare s
    match grammar input with
    | Success(ast, _) -> Some ast
    | Failure(_, _) -> None

