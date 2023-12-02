module AST 

(* Primitives *)
type Int = int
type String = string

(* Combining Forms *)
type Stitch = String * Int

type StitchSeq =
    | StitchRep of Stitch * Int
    | StitchSeqToEnd of StitchSeq
    | TwoStitchSeq of StitchSeq * StitchSeq

type Row = StitchSeq list

type Instruction = 
    | Row of Row
    | String of String