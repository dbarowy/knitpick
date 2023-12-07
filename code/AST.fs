module AST 

(* Primitives *)
type Float = float
type Int = int
type String = string


// type Number = 
//     | NumFloat of Float 
//     | NumInt of Int

(* Combining Forms *)
type Stitch = String * Int

type StitchSeq =
    | StitchRep of Stitch * Int
    | StitchSeqToEnd of StitchSeq
    | TwoStitchSeq of StitchSeq * StitchSeq

type Row = StitchSeq list

type Instruction = 
    | InstRow of Row
    | InstString of String

type Paragraph = String * Instruction list

type Needle = String * String * Int

type Gauge = Float * Float

type Yarn = String * Int

type Header = String * Needle * Gauge * Yarn

type Document = Header * Paragraph list