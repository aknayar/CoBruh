open Ast

type sexpr = dtype * sx
and sx = 
    SNumberLit of float
  | SBoolLit of bool
  | SStringLit of string
  | SCharLit of char
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SCall of string * sexpr list

type sstmt = 
    SExpr of sexpr
  | SAssign of dtype * string * sexpr
  | SReassign of string * sexpr
  | SIf of sexpr * sstmt list * sstmt list
  | SLoop of string * sexpr * sexpr * sexpr * sstmt list
  | SReturn of sexpr

type sfunc = {
  sfname: string;
  sparams: bind list;
  srtype: func_rtype;
  body: sstmt list;
}

type sprogram = sstmt list * sfunc list

