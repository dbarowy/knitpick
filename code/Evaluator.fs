module Evaluator

open Parser
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
    let rec evaluate i = 
        match i with
        | StitchRep (stitch, reps) -> (evalStitch stitch) + " " + (reps |> string)
        | StitchSeqToEnd (sseq) -> (evaluate sseq) + " " + "to end of row."
        | TwoStitchSeq (sseq1, sseq2) -> (evaluate sseq1) + " " + (evaluate sseq2)
    (evaluate input) + "\n"
 
let constructDoc ast = 
    File.WriteAllText("output.tex", "")
    writeToFile startDoc
    writeToFile (evalStitchSeq ast)
    writeToFile endDoc
