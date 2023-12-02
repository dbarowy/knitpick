module Evaluator

open AST
open System.IO

let startDoc = 
    "\\documentclass[10pt]{article}\n\n\
    \\usepackage{times,graphicx,fancyhdr,amsfonts,xspace,hyperref}\n\
    \\usepackage[left=1in,top=1in,right=1in,bottom=1in]{geometry}\n\
    \\usepackage{sect sty}\n\
    \\usepackage{enumerate}	\n\
    \\usepackage[space]{grffile}\n\
    \\graphicspath{.} \n\
    \\pagestyle{fancy}\n\
    \\renewcommand{\\headrulewidth}{0.4pt}\n\
    \\renewcommand{\\headwidth}{\\textwidth}\n\
    \\renewcommand{\\footrulewidth}{0.4pt}\n\
    \\setlength{\\parindent}{0cm}\n\
    \\title{Knitting Pattern}\n\
    \\date{}\n\n\
    \\begin{document}\n\n\
    \\maketitle\n\n\
    "

let endDoc = "\\end{document}\n"

let writeToFile str = 
    File.AppendAllText("output.tex", str)

let evalStitch (input: Stitch) : string =
    match input with
    | (name, weight) -> name |> string

let evalStitchSeq (input: StitchSeq) : string = 
    let rec evaluate ss = 
        match ss with
        | StitchRep (stitch, reps) -> (evalStitch stitch) + " " + (reps |> string)
        | StitchSeqToEnd (sseq) -> (evaluate sseq) + " " + "to end of row."
        | TwoStitchSeq (sseq1, sseq2) -> (evaluate sseq1) + " " + (evaluate sseq2)
    (evaluate input) + "\n"

let evalRow (input: Row) : string = 
    let rec evaluate r n = 
        match r with
        | [] -> ""
        | x::xs -> (n |> string) + ". " + (evalStitchSeq x) + "\n" + (evaluate xs (n + 1))
    (evaluate input 1)

let constructDoc (ast: Row) = 
    File.WriteAllText("output.tex", "")
    writeToFile startDoc
    writeToFile (evalRow ast)
    writeToFile endDoc
