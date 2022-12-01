module L = Llvm
open Ast
open Sast
(* test *)
let translate (prog: sprogram): L.llmodule = 
  let context = L.global_context () in
  let mdl = L.create_module context "CoBruh" in

  let f_t = L.float_type context 
  and i1_t = L.i1_type context in
  let lltype_of_dtype = function
      Number -> f_t
    | Bool -> i1_t
    | _ -> raise (Failure "unimplemented") 
  in

  let build_expr builder (_, exp) = match exp with
      SNumberLit n -> L.const_float f_t n
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | _ -> raise (Failure "unimplemented")
  in

  let build_stmt builder = function
      SExpr sexp -> ignore (build_expr builder sexp); builder
    | SInit (id, sexp) -> 
    | _ -> raise (Failure "unimplemented")
