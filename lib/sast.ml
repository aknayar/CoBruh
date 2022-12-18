open Ast

type sexpr = dtype * sx
and sx = 
    SNumberLit of float
  | SBoolLit of bool
  | SCharLit of char
  | SStringLit of string
  | SArrayLit of dtype * sx option * sx list option
  | SId of string * int
  | SElem of sx * sx
  | SBinop of sx * bop * sx
  | SUnop of uop * sx
  | SECall of string * sexpr list

type sstmt = 
    SInit of dtype * string * sx
  | SReassign of sx * sx
  | SIf of sx * sstmt list * sstmt list
  | SCondLoop of sx * sstmt list
  | SReturn of sexpr
  | SContinue
  | SStop
  | SSCall of sx

type sfunc = {
  sfname: string;
  sparams: bind list;
  srtype: dtype;
  sbody: sstmt list;
}

type sprogram = bind list * sfunc list

