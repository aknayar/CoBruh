open Ast

type sexpr = dtype * sx
and sx = 
    SNumberLit of float
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SArray of sexpr list
  | SId of string * int
  | SBinop of sexpr * bop * sexpr
  | SUnop of uop * sexpr
  | SCall of string * sexpr list
  | SElem of string * sexpr

type sstmt = 
    SExpr of sexpr
  | SInit of string * sexpr (* for initializing and assigning a variable *)
  | SReassign of string * int * sexpr (* for reassigning an existing variable *)
  | SAlloc of dtype * string * sexpr
  (* | SAllocAssign of dtype * string * sexpr * sexpr list
  | SAllocInferAssign of string * sexpr * sexpr list *)
  | SIf of sexpr * sstmt list * sstmt list
  | SIterLoop of string * sexpr * sexpr * sexpr * sstmt list
  | SCondLoop of sexpr * sstmt list
  | SReturn of sexpr
  | SContinue
  | SStop

type sfunc = {
  sfname: string;
  sparams: bind list;
  srtype: dtype;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc list

