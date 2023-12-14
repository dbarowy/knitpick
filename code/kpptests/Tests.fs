namespace kpptests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

open Combinator
open AST
open Parser
open Evaluator

// Parser Test Suite
[<TestClass>]
type ParserTests () =

    [<TestMethod>]
    member this.BuiltInStitchWorks () =
        let input = prepare "p"
        let expected =  StitchBuiltIn "purl"

        let result = pstitch input 
        match result with 
        | Success(s, _) -> Assert.AreEqual(expected, s)
        | Failure(_, _) -> Assert.IsTrue false

    [<TestMethod>]
    member this.StitchVariableWorks () = 
        let input = prepare "name"
        let expected = "name" |> Var |> StitchVar

        let result = pstitch input 
        match result with 
        | Success(s, _) -> Assert.AreEqual(expected, s)
        | Failure(_, _) -> Assert.IsTrue false

    [<TestMethod>]
    member this.StitchRepWorks () = 
        let input = prepare "k2"
        let expected = (StitchBuiltIn "knit", 2) |> StitchRep

        let result = pstitchseq input 
        match result with 
        | Success(ss, _) -> Assert.AreEqual(expected, ss)
        | Failure(_, _) -> Assert.IsTrue false

    [<TestMethod>]
    member this.StitchSeqToEndWorks () = 
        let input = prepare "+p3"
        let expected = (StitchBuiltIn "purl", 3) |> StitchRep |> StitchSeqToEnd

        let result = pstitchseq input 
        match result with 
        | Success(ss, _) -> Assert.AreEqual(expected, ss)
        | Failure(_, _) -> Assert.IsTrue false

    [<TestMethod>]
    member this.TwoStitchSeqWorks () = 
        let input = prepare "(p1k2)"
        let expected = (StitchRep (StitchBuiltIn "purl", 1), StitchRep (StitchBuiltIn "knit", 2) ) |> TwoStitchSeq

        let result = pstitchseq input 
        match result with 
        | Success(ss, _) -> Assert.AreEqual(expected, ss)
        | Failure(_, _) -> Assert.IsTrue false

// Interpreter Test Suite
[<TestClass>]
type InterpreterTests () =

    [<TestMethod>]
    member this.EvalBuiltInStitchWorks () =
        let input = "purl" |> StitchBuiltIn
        let expected = "purl"

        let result = evalStitch input
        
        Assert.AreEqual(result, expected)
    

    [<TestMethod>]
    member this.EvalStitchRepWorks () = 
        let input = (StitchBuiltIn "knit", 2) |> StitchRep
        let expected = "knit 2."

        let result = evalStitchSeq input 
        
        Assert.AreEqual(result, expected)

    [<TestMethod>]
    member this.EvalStitchSeqToEndWorks () = 
        let input = (StitchBuiltIn "purl", 3) |> StitchRep |> StitchSeqToEnd
        let expected = "purl 3 to end of row."

        let result = evalStitchSeq input 

        Assert.AreEqual(result, expected)

    [<TestMethod>]
    member this.TwoStitchSeqWorks () = 
        let input = (StitchRep (StitchBuiltIn "purl", 1), StitchRep (StitchBuiltIn "knit", 2) ) |> TwoStitchSeq
        let expected = "purl 1, knit 2."

        let result = evalStitchSeq input 

        Assert.AreEqual(result, expected)