module L = Llvm
module A = Ast
open Sast

let translate (prog: sprogram): L.llmodule = 
  let context = L.global_context () in
  let mdl = L.create_module context "CoBruh" in

  let f_t = L.float_type context 
  and i8_t = L.i8_type context 
  and i1_t = L.i1_type context in

  let printf_t : L.lltype =
    L.var_arg_function_type f_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t mdl in


  let lltype_of_dtype = function
      A.Number -> f_t
    | A.Bool -> i1_t
    | _ -> raise (Failure "unimplemented") 
  in

  let rec build_expr builder (_, exp) = match exp with
      SNumberLit n -> L.const_float f_t n
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SBinop (e1, op, e2) ->
      let e1' = build_expr builder e1
      and e2' = build_expr builder e2 in
      (
        match op with
            A.Plus    -> L.build_fadd
          | A.Minus   -> L.build_fsub
          | A.Times   -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Eq      -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | _         -> raise (Failure "unimplemented")
      ) e1' e2' "tmp" builder
    | SCall ("say", [e]) ->
      let num_format_str = L.build_global_stringptr "%g\n" "fmt" builder in
      L.build_call printf_func [| num_format_str ; (build_expr builder e) |]
        "printf" builder
    | _ -> raise (Failure "unimplemented")
  in

  let build_stmt builder = function
      SExpr sexp -> ignore (build_expr builder sexp); builder
    | SInit (id, sexp) -> raise (Failure "unimplemented")
    | _ -> raise (Failure "unimplemented") in

  let build_decl builder = function
      SStmt sstmt -> ignore(build_stmt builder sstmt)
    | SFunc sfunc -> raise (Failure "unimplemented")
  in
  List.iter build_decl prog;
  mdl
