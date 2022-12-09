module StringMap = Map.Make(String)
module L = Llvm

open Ast
open Sast

let translate (binds, sfuncs): L.llmodule = 
  let context = L.global_context () in
  let mdl = L.create_module context "CoBruh" in

  let f_t = L.float_type context 
  and i8_t = L.i8_type context 
  and i1_t = L.i1_type context 
  and void_t = L.void_type context in

  let lltype_of_dtype = function
      Number -> f_t
    | Bool -> i1_t
    | None -> void_t
    | _ -> raise (Failure "unimplemented") 
  in

  let _ = 
    let add_global m (typ, name) = 
      let init = 
        match typ with
            Number -> L.const_float (lltype_of_dtype typ) 0.0
          | Bool -> L.const_int (lltype_of_dtype typ) 0
          | _ -> raise (Failure "unimplemented")
      in StringMap.add name (L.define_global name init mdl) m 
    in List.fold_left add_global StringMap.empty binds
  in

  let printf_t : L.lltype =
    L.var_arg_function_type f_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t mdl in

  let all_funcs =
    let add_func m fn =
      let param_dtypes = Array.of_list (List.map (fun (typ, _) -> lltype_of_dtype typ) fn.sparams) in
      let ftype = L.function_type (lltype_of_dtype fn.srtype) param_dtypes in
      StringMap.add fn.sfname (L.define_function fn.sfname ftype mdl, fn) m
    in List.fold_left add_func StringMap.empty sfuncs
  in

  let build_func_body fn = 
    let (the_func, _) = StringMap.find fn.sfname all_funcs in
    let rec build_expr builder (_, exp) = match exp with
        SNumberLit n -> L.const_float f_t n
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SBinop (e1, op, e2) ->
          let e1' = build_expr builder e1
          and e2' = build_expr builder e2 in
          (
            match op with
                Plus    -> L.build_fadd
              | Minus   -> L.build_fsub
              | Times   -> L.build_fmul
              | Div     -> L.build_fdiv
              | And     -> L.build_and
              | Or      -> L.build_or
              | Eq      -> L.build_icmp L.Icmp.Eq
              | Neq     -> L.build_icmp L.Icmp.Ne
              | Less    -> L.build_icmp L.Icmp.Slt
              | _         -> raise (Failure "unimplemented")
          ) e1' e2' "tmp" builder
      | SCall ("say", [e]) ->
          let num_format_str = L.build_global_stringptr "%g\n" "fmt" builder in
          L.build_call printf_func [| num_format_str ; (build_expr builder e) |] "printf" builder
      | _ -> raise (Failure "unimplemented")
    in

    let add_terminal builder instr = 
      match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder)
    in

    let rec build_stmt sc builder = function
        SExpr sexp -> ignore (build_expr builder sexp); builder
      | SInit (id, sexp) -> 
          let local = L.build_alloca (lltype_of_dtype (fst sexp)) id builder
          in Hashtbl.add sc id local;
          let sexp' = build_expr builder sexp in
          ignore (L.build_store sexp' local builder); builder
      | SIfElse (prd, if_block, else_block) ->
          let bool_val = build_expr builder prd in
          let merge_bb = L.append_block context "merge" the_func in
          let build_br_merge = L.build_br merge_bb in (* partial function *)

          let then_bb = L.append_block context "then" the_func in
          add_terminal (List.fold_left (
            fun b s -> build_stmt  sc b s
          ) (L.builder_at_end context then_bb) if_block) build_br_merge;

          let else_bb = L.append_block context "else" the_func in
          add_terminal (List.fold_left (
            fun b s -> build_stmt  sc b s
          ) (L.builder_at_end context else_bb) else_block) build_br_merge;

          ignore(L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
      | _ -> raise (Failure "unimplemented")
    in
    
    let func_scope = ref [Hashtbl.create 10] in
    let (the_func, _) = StringMap.find fn.sfname all_funcs in
    let builder = L.builder_at_end context (L.entry_block the_func) in
    let builder = List.fold_left (
      fun b s -> build_stmt (List.hd !func_scope) b s
    ) builder fn.sbody in

    add_terminal builder (
      match fn.srtype with
          None -> L.build_ret_void
        | typ -> L.build_ret (L.const_float (lltype_of_dtype typ) 0.0)
    )

  in List.iter build_func_body sfuncs;
  mdl
