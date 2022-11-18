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

type sarray_element =
    SExprElem of sexpr
  | SArrayElem of sarray_element list

type sarray_assign = 
    SArrayLit of sarray_element list
  | SArrayId of string

type sstmt = 
    SExpr of sexpr
  | SAssign of dtype * string * sexpr
  | SInferAssign of string * sexpr
  | SAlloc of dtype * string * sexpr list
  | SAllocAssign of dtype * string * sexpr list * sarray_assign
  | SAllocInferAssign of string * sexpr list * sarray_assign
  | SIf of sexpr * sstmt list * sstmt list
  | SIterLoop of string * sexpr * sexpr * sexpr * sstmt list
  | SCondLoop of sexpr * sstmt list
  | SReturn of sexpr

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

