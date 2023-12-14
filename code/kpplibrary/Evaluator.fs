module Evaluator

open AST
open System
open System.IO

// dictionary to store variables and their contents
let varDict = new System.Collections.Generic.Dictionary<string,string>()

let endDoc = "\\end{document}\n"

// function for writing to a LaTeX file
let writeToFile str name = 
    File.AppendAllText(name + ".tex", str)

let evalAssignment (input: Assignment) : string = 
    let name : string = 
        match input with 
        | (n, v) -> n
    let value : string = 
        match input with 
        | (n, v) -> v
    // mutably store result in the dictionary varDict for key name
    varDict[name] <- value

    ""

let evalVar (input: Var) : string = 
    match input with 
    | name -> 
        if (varDict.ContainsKey name) then 
            varDict[name]
        else 
            failwith ("Unknown variable '" + name + "'")

let evalFloat (input: Float) : string = 
    match input with
    | f -> string f

let evalStitch (input: Stitch) : string =
    match input with
    | StitchBuiltIn (name) -> name |> string
    | StitchVar (name) -> evalVar name

let evalStitchSeq (input: StitchSeq) : string = 
    let rec evaluate ss = 
        match ss with
        | StitchRep (stitch, reps) -> (evalStitch stitch) + " " + (reps |> string)
        | StitchSeqToEnd (sseq) -> (evaluate sseq) + " " + "to end of row"
        | TwoStitchSeq (sseq1, sseq2) -> (evaluate sseq1) + ", " + (evaluate sseq2)
    
    (evaluate input) + "."

let rec evalRow (input: Row) : string = 
    match input with
    | [] -> ""
    | x::xs -> (evalStitchSeq x) + " " + (evalRow xs)

let rec evalRows rs = 
    match rs with 
    | [] -> ""
    | r::rs -> "\\item " + evalRow(r) + "\n" + evalRows(rs)

let evalInstTextOnly i = 
    match i with
    | InstRow r -> (evalRow r)
    | InstString s -> s
    | Repeat (i, rs) -> 
        "\n" + @"\begin{enumerate}[label=(\alph*)]" + "\n" + evalRows(rs) + @"\end{enumerate}" + "\nRepeat for a total of " + (i |> string) + " times."
    
let evalInst (input: Instruction) (stepNum: int) : string = 
    let prefix = @"\textbf{Step " + (stepNum |> string) + ". }"

    (* let evalInstHelper i = 
        match i with
        | InstRow r -> prefix + (evalRow r)
        | InstString s -> prefix + s
        | Repeat (i, rs) -> 
            prefix + "\n" + @"\begin{enumerate}[label=(\alph*)]" + "\n" + evalRows(rs) + @"\end{enumerate}" + "\nRepeat for a total of " + (i |> string) + " times." 
    *)
    
    let evalInstHelper i = (prefix + evalInstTextOnly i)

    (evalInstHelper input) + "\n\n"

let evalPara (input: Paragraph) : string = 
    let rec enumerateInsts is n = 
        match is with
        | [] -> ""
        | i::is -> (evalInst i n) + (enumerateInsts is (n + 1))

    match input with
    | (s, is) -> "\\section*{" + s + "}" + "\n\n" + (enumerateInsts is 1) + "\n"

let evalTitle (input: String) : string = 
    "\\documentclass[10pt]{article}\n\n\
    \\usepackage{times,graphicx,fancyhdr,amsfonts,xspace,hyperref}\n\
    \\usepackage[left=1in,top=1in,right=1in,bottom=1in]{geometry}\n\
    \\usepackage{sect sty}\n\
    \\usepackage{enumerate}	\n\
    \\usepackage[space]{grffile}\n\
    \\usepackage{makecell}\n\
    \\usepackage{enumitem}\n\n\
    \\graphicspath{.} \n\
    \\pagestyle{fancy}\n\
    \\renewcommand{\\headrulewidth}{0.4pt}\n\
    \\renewcommand{\\headwidth}{\\textwidth}\n\
    \\renewcommand{\\footrulewidth}{0.4pt}\n\
    \\setlength{\\parindent}{0cm}\n\
    \\title{" + input + "}\n\
    \\date{}\n\
    \\pagenumbering{gobble}\n\n\
    \\begin{document}\n\n\
    \\maketitle\n\n\n\
    "

let evalNeedle (input: Needle) : string = 
    match input with
    | (t, unit, size) -> 
        @"\makecell[t]{" + "\n" + @"size " + unit + " " + (size |> string) +  @"\\" + "\n" + t + @"\\}" + "\n" + "&" + "\n"
        
let evalGauge (input: Gauge) : string = 
    match input with 
    | (a, b) -> 
        @"\makecell[t]{" + "\n" + (a |> string) + @" stitches\\" + "\n" + (b |> string) + @" rows per square inch\\}" + "\n" + "&" + "\n"

let evalYarn (input: Yarn) : string = 
    match input with 
    | (material, size) ->
        @"\makecell[t]{" + "\n" + "size " + (size |> string) + @"\\" + "\n" + material + @" yarn\\" + @"}\\" + "\n"

let evalHeader (input: Header) : string = 
    match input with 
    | (title, needle, gauge, yarn) -> 
        evalTitle(title) + 
        @"\begin{center}" + "\n" + @"\begin{tabular}{|c|c|c|}" + "\n" + @"\hline" + "\n" + @"\thead{\textbf{Needles}} & \thead{\textbf{Gauge}} & \thead{\textbf{Yarn}} \\" + "\n" + @"\hline" + "\n" +  
        evalNeedle(needle) + 
        evalGauge(gauge) + 
        evalYarn(yarn) + 
        @"\hline" + "\n" + @"\end{tabular}" + "\n" + @"\end{center}" + "\n\n\n"

let evalDocument (input: Document) : string = 
    let rec evalParaList paragraphs = 
        match paragraphs with 
        | [] -> ""
        | p::ps -> evalPara(p) + evalParaList(ps)

    match input with 
    | (header, paragraphs) -> evalHeader(header) + evalParaList(paragraphs)

let evalProgram (input: Program) : string = 
    let rec evalAssignments assignments = 
        match assignments with 
        | [] -> ""
        | x::xs -> (evalAssignment x) + (evalAssignments xs)

    match input with 
    | (alist, doc) -> evalAssignments(alist) + (evalDocument doc)

// ProcessResult and executeProcess from
// https://isthisit.nz/posts/2021/execute-a-shell-process-in-fsharp/
type ProcessResult = { 
    ExitCode : int; 
    StdOut : string; 
    StdErr : string 
}

let executeProcess (processName: string) (processArgs: string) =
    let psi = new Diagnostics.ProcessStartInfo(processName, processArgs) 
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.CreateNoWindow <- true        
    let proc = Diagnostics.Process.Start(psi) 
    let output = new Text.StringBuilder()
    let error = new Text.StringBuilder()
    proc.OutputDataReceived.Add(fun args -> output.Append(args.Data) |> ignore)
    proc.ErrorDataReceived.Add(fun args -> error.Append(args.Data) |> ignore)
    proc.BeginErrorReadLine()
    proc.BeginOutputReadLine()
    proc.WaitForExit()
    { ExitCode = proc.ExitCode; StdOut = output.ToString(); StdErr = error.ToString() }

let toPDF (filename: string) = 
    executeProcess "pdflatex" filename |> ignore

let constructDoc (ast: Program) (name: string) = 
    File.WriteAllText(name + ".tex", "")
    writeToFile (evalProgram ast) name
    writeToFile endDoc name
    toPDF (name + ".tex")