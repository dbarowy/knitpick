module AST 

(* Primitives *)
type Float = float
type Int = int
type String = string

// type Number = 
//     | NumFloat of Float 
//     | NumInt of Int

(* Combining Forms *)
type Var = String

type Assignment = Var * String

type Stitch = 
    | StitchBuiltIn of String
    | StitchVar of Var

type StitchSeq =
    | StitchRep of Stitch * Int
    | StitchSeqToEnd of StitchSeq
    | TwoStitchSeq of StitchSeq * StitchSeq

type Row = StitchSeq list

type Instruction = 
    | InstRow of Row
    | InstString of String
    | Repeat of Int * Row list

type Paragraph = String * Instruction list

type Needle = String * String * Int

type Gauge = Float * Float

type Yarn = String * Int

type Header = String * Needle * Gauge * Yarn

type Document = Header * Paragraph list

type Program = Assignment list * Document

(* type Expr = 
    | DocumentExpr of Document
    | VarExpr of Var
    | AssignmentExpr of Assignment
*)
