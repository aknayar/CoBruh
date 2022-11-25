open Ast

type sexpr = dtype * sx
and sx = 
    SNumberLit of float
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SUnop of uop * sexpr
  | SCall of string * sexpr list
  | SElem of string * sexpr

type sstmt = 
    SExpr of sexpr
  | SAssign of dtype * string * sexpr
  | SInferAssign of string * sexpr
  | SAlloc of dtype * string * sexpr
  | SAllocAssign of dtype * string * sexpr * sexpr list
  | SAllocInferAssign of string * sexpr * sexpr list
  | SIf of sexpr * sstmt list
  | SIfElse of sexpr * sstmt list * sstmt list
  | SIterLoop of string * sexpr * sexpr * sexpr * sstmt list
  | SCondLoop of sexpr * sstmt list
  | SReturn of sexpr
  | SContinue
  | SStop

type sfunc = {
  sfname: string;
  sparams: bind list;
  srtype: func_rtype;
  sbody: sstmt list;
}

type sdecl = 
    SStmt of sstmt
  | SFunc of sfunc

type sprogram = sdecl list

