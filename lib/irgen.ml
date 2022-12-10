module L = Llvm

open Ast
open Sast

let translate (binds, sfuncs): L.llmodule = 
  let context = L.global_context () in
  let mdl = L.create_module context "CoBruh" in

  let f_t = L.float_type context 
  and i1_t = L.i1_type context 
  and i8_t = L.i8_type context 
  and void_t = L.void_type context in

  let lltype_of_dtype = function
      Number -> f_t
    | Bool -> i1_t
    | Char -> i8_t
    (* string is special case *)
    | None -> void_t
    | _ -> raise (Failure "unimplemented") 
  in

  let globals = Hashtbl.create (List.length binds) in
  let add_global (typ, name) = 
    let init = 
      match typ with
          Number -> L.const_float (lltype_of_dtype typ) 0.0
        | (Bool | Char) -> L.const_int (lltype_of_dtype typ) 0
        | String -> L.const_stringz context ""
        | _ -> raise (Failure "unimplemented")
    in Hashtbl.add globals name (L.define_global name init mdl)
  in List.iter add_global binds;

  let printf_t : L.lltype = L.var_arg_function_type (lltype_of_dtype None) [| L.pointer_type (lltype_of_dtype Char) |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t mdl in

  let all_funcs = Hashtbl.create (List.length sfuncs) in
  let add_func fn =
    let param_dtypes = Array.of_list (List.map (fun (typ, _) -> lltype_of_dtype typ) fn.sparams) in
    let ftype = L.function_type (lltype_of_dtype fn.srtype) param_dtypes in
    Hashtbl.add all_funcs fn.sfname (L.define_function fn.sfname ftype mdl, fn)
  in List.iter add_func sfuncs;

  let build_func_body fn = 
    let scopes = ref [globals] in

    let (the_func, _) = Hashtbl.find all_funcs fn.sfname in
    let builder = L.builder_at_end context (L.entry_block the_func) in

    let number_format = L.build_global_stringptr "%f\n" "fmt" builder
    and bool_format = L.build_global_stringptr "%d\n" "fmt" builder
    and char_format = L.build_global_stringptr "%c\n" "fmt" builder
    and string_format = L.build_global_stringptr "%s\n" "fmt" builder in
    let format_string_of_dtype typ = ( 
      match typ with
          Number -> number_format
        | Bool -> bool_format
        | Char -> char_format
        | String -> string_format
        | _ -> raise (Failure "unimplemented")
    ) in

    let body_scope = Hashtbl.create (List.length fn.sparams + List.length fn.sbody) in
    let add_param (typ, name) param = 
      L.set_value_name name param;
      let local = L.build_alloca (lltype_of_dtype typ) name builder in
      ignore (L.build_store param local builder);
      Hashtbl.add body_scope name local
    in List.iter2 add_param fn.sparams (Array.to_list (L.params the_func));

    scopes := body_scope::(!scopes);

    let rec build_expr builder (_, exp) = match exp with
        SNumberLit n -> L.const_float (lltype_of_dtype Number) n
      | SBoolLit b -> L.const_int (lltype_of_dtype Bool) (if b then 1 else 0)
      | SCharLit c -> L.const_int (lltype_of_dtype Char) (Char.code c)
      | SStringLit s -> L.const_stringz context s
      | SId (id, sc) -> L.build_load (Hashtbl.find (List.nth !scopes sc) id) id builder
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
              | Leq     -> L.build_icmp L.Icmp.Sle
              | Greater -> L.build_icmp L.Icmp.Sgt
              | Geq     -> L.build_icmp L.Icmp.Sge
              | _       -> raise (Failure "unimplemented")
          ) e1' e2' "tmp" builder
      | SCall ("say", [e]) -> L.build_call printf_func [| format_string_of_dtype (fst e) ; (build_expr builder e) |] "" builder
      | _ -> raise (Failure "unimplemented")
    in

    let add_terminal builder instr = 
      match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder)
    in

    let rec build_stmt builder = function
        SExpr sexp -> ignore (build_expr builder sexp); builder
      | SInit (id, sexp) -> 
          let local = L.build_alloca (lltype_of_dtype (fst sexp)) id builder
          in Hashtbl.add (List.hd !scopes) id local;
          let sexp' = build_expr builder sexp in
          ignore (L.build_store sexp' local builder); builder
      | SReassign (id, sc, sexp) -> 
          let sexp' = build_expr builder sexp in
          ignore (L.build_store sexp' (Hashtbl.find (List.nth !scopes sc) id) builder); builder
      | SIf (prd, if_block, else_block) ->
          let bool_val = build_expr builder prd in
          let merge_bb = L.append_block context "merge" the_func in
          let build_br_merge = L.build_br merge_bb in (* partial function *)

          let then_bb = L.append_block context "then" the_func in
          add_terminal (do_block (Hashtbl.create (List.length if_block)) then_bb if_block) build_br_merge;

          let else_bb = L.append_block context "else" the_func in
          add_terminal (do_block (Hashtbl.create (List.length else_block)) else_bb else_block) build_br_merge;

          ignore(L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
      | SCondLoop (prd, block) ->
          let prd_bb = L.append_block context "loop" the_func in
          ignore(L.build_br prd_bb builder);
      
          let block_bb = L.append_block context "loop_block" the_func in
          add_terminal (do_block (Hashtbl.create (List.length block)) block_bb block) (L.build_br prd_bb);
      
          let pred_builder = L.builder_at_end context prd_bb in
          let bool_val = build_expr pred_builder prd in
      
          let merge_bb = L.append_block context "merge" the_func in
          ignore(L.build_cond_br bool_val block_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb
      | _ -> raise (Failure "unimplemented")
    and do_block scope bb block = 
      scopes := scope::(!scopes);
      let res = List.fold_left build_stmt (L.builder_at_end context bb) block in
      scopes := List.tl (!scopes);
      res
    in
    
    let builder = List.fold_left build_stmt builder fn.sbody in

    add_terminal builder (
      match fn.srtype with
          None -> L.build_ret_void
        | typ -> L.build_ret (L.const_float (lltype_of_dtype typ) 0.0)
    )

  in List.iter build_func_body sfuncs;
  mdl
