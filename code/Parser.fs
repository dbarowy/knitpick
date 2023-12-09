module Parser

open Combinator
open AST

let pstitchseq, pstitchseqImpl = recparser()

let pinteger : Parser<Int> = pmany1 pdigit |>> (fun ds -> stringify ds |> int)

let pfloat : Parser<Float> = pseq (pseq (pmany1 pdigit) (pchar '.') (fun (a, b) -> a @ [b])) (pmany1 pdigit) (fun (a, b) -> a @ b)|>> (fun ds ->stringify ds |> float)

//work this out with Locke - for the purpose of allowing metric and imperial measurements in needle size
// let pnumber : Parser<Number> = pinteger <|> pfloat |>> (fun n -> Number n)

let pnotquot: Parser<char> = psat (fun c -> c <> '"')
let pstring : Parser<String> = pbetween (pchar '"') (pmany1 pnotquot) (pchar '"') |>> (fun cs -> stringify cs)

let pinststring : Parser<Instruction> = pstring |>> (fun s -> InstString(s))

let pad p = pbetween pws0 p pws0

let ppurl : Parser<Stitch> = pchar 'p' |>> (fun _ -> ("purl", 0))
let pknit : Parser<Stitch> = pchar 'k' |>> (fun _ -> ("knit", 0))

let pstitch : Parser<Stitch> = pknit <|> ppurl

let pstitchrep : Parser<StitchSeq> = pseq pstitch pinteger (fun (a, b) -> (a, b)) |>> (fun (a, b) -> StitchRep (a, b))

let pstitchseqtoend : Parser<StitchSeq> = pright (pchar '+') (pstitchseq) |>> (fun s -> StitchSeqToEnd (s))

let ptwostitchseq : Parser<StitchSeq> = pbetween (pchar '(') (pseq (pstitchseq) (pstitchseq) (fun (a, b) -> (a, b)))  (pchar ')') |>> (fun tup -> TwoStitchSeq (tup))

pstitchseqImpl := pstitchrep <|> pstitchseqtoend <|> ptwostitchseq

let prow : Parser<Row> = pmany1 (pleft (pstitchseq) pws0) 
let pinstrow : Parser<Instruction> = prow |>> (fun r -> InstRow(r))

let pinst : Parser<Instruction> = pinstrow <|> pinststring

let ppara : Parser<Paragraph> = pright (pstr "pg ") (pseq (pstring) (pmany1 (pad pinst)) (fun (s, is) -> Paragraph(s, is)) )
// let ppara : Parser<Paragraph> = pright (pstr "pg ") (pstring) |>> (fun c -> string c)
// let ppara : Parser<Paragraph> = pright (pstr "pg") (pmany1 (pleft (pad pinst) pws0))

// if we want to make both US and metric sized needles acceptable
// let pmetricneedle = pseq (pad (pfloat)) (pstr "mm") (fun (a, b) -> (b, a))
// let pusneedle = pseq (pad (pstr "us")) (pinteger) (fun (a, b) -> (a, a))
// let pneedle : Parser<Needle> = pright (pstr "needle ") (pseq (pad (pstring)) (pmetricneedle <|> pusneedle) (fun (a, (b, c)) -> (a, b, c)))

let pneedle : Parser<Needle> = pright (pstr "needle ") (pseq (pad (pstring)) (pseq (pad (pstring)) (pinteger) (fun (a,b) -> (a,b))) (fun (a, (b, c)) -> (a, b, c)))

let pgauge : Parser<Gauge> = pright (pstr "gauge ") (pbetween (pchar '(') (pseq (pleft (pfloat) (pad (pchar ','))) (pfloat) (fun (a,b) -> (a,b))) (pchar ')') )

let pyarn : Parser<Yarn> = pright (pstr "yarn ") (pseq (pad (pstring)) (pad (pinteger)) (fun (a,b) -> (a,b)))

let pheader : Parser<Header> = pright (pstr "header ") (pseq
    (pad (pstring))
    (pseq
        (pad (pneedle))
        (pseq
            (pad (pgauge))
            (pad (pyarn))
            (fun (a,b) -> (a,b))
        )
        (fun (a,(b,c)) -> (a,b,c))
    )
    (fun (a,(b,c,d)) -> (a,b,c,d))
    )

let pdocument : Parser<Document> = pseq (pad pheader) (pmany1 (pad ppara)) (fun (a,b)->(a,b))

let grammar = pleft pheader peof

let parse (s: string) = 
    let input = prepare s
    match grammar input with
    | Success(ast, _) -> Some ast
    | Failure(_, _) -> None

