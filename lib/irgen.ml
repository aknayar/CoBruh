module L = Llvm

open Ast
open Sast

let translate (binds, sfuncs): L.llmodule = 
  let context = L.global_context () in
  let mdl = L.create_module context "CoBruh" in

  let f_t = L.double_type context 
  and i1_t = L.i1_type context 
  and i8_t = L.i8_type context 
  and i32_t = L.i32_type context
  and void_t = L.void_type context in

  let rec lltype_of_dtype = function
      Number -> f_t
    | Bool -> i1_t
    | Char -> i8_t
    | String -> L.pointer_type i8_t
    | Array typ -> L.pointer_type (lltype_of_dtype typ)
    | None -> void_t
    | _ -> raise (Failure "unimplemented 1") 
  in
  let default_value = function
      Number -> L.const_float (lltype_of_dtype Number) 0.0
    | (Bool | Char) as typ -> L.const_int (lltype_of_dtype typ) 0
    | _ -> raise (Failure "unimplemented 2") 
  in

  let globals = Hashtbl.create (List.length binds) in
  List.iter (fun (typ, name) -> Hashtbl.add globals name (L.define_global name (default_value typ) mdl)) binds;

  let printf_t : L.lltype = L.var_arg_function_type void_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = L.declare_function "printf" printf_t mdl in
  let scanf_t : L.lltype = L.var_arg_function_type void_t [| L.pointer_type i8_t |] in
  let scanf_func : L.llvalue = L.declare_function "scanf" scanf_t mdl in
  
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

    let number_format scan nl = L.build_global_stringptr (if scan then "%lf" else "%g" ^ (if nl then "\n" else "")) "fmt" builder 
    and bool_format nl = L.build_global_stringptr ("%d" ^ (if nl then "\n" else "")) "fmt" builder
    and char_format scan nl = L.build_global_stringptr (if scan then " %c" else "%c" ^ (if nl then "\n" else "")) "fmt" builder
    and string_format nl  = L.build_global_stringptr ("%s" ^ (if nl then "\n" else "")) "fmt" builder in
    let format_string_of_dtype typ scan nl = ( 
      match typ with
          Number -> number_format scan nl
        | Bool -> bool_format nl
        | Char -> char_format scan nl
        | String -> string_format nl
        | _ -> raise (Failure "unimplemented 3")
    ) in

    let body_scope = Hashtbl.create (List.length fn.sparams + List.length fn.sbody) in
    let add_param (typ, name) param = 
      L.set_value_name name param;
      let local = L.build_alloca (lltype_of_dtype typ) name builder in
      ignore (L.build_store param local builder);
      Hashtbl.add body_scope name local
    in List.iter2 add_param fn.sparams (Array.to_list (L.params the_func));

    let read_typ typ = 
      let scanin = L.build_alloca (lltype_of_dtype typ) "scanin" builder in
      ignore (L.build_call scanf_func [| format_string_of_dtype typ true false; scanin |] "" builder);
      L.build_load scanin "scanin" builder
    
    in
    scopes := body_scope::(!scopes);

    let rec build_expr builder (_, exp) = match exp with
        SNumberLit n -> L.const_float (lltype_of_dtype Number) n
      | SBoolLit b -> L.const_int (lltype_of_dtype Bool) (if b then 1 else 0)
      | SCharLit c -> L.const_int (lltype_of_dtype Char) (Char.code c)
      | SStringLit s -> L.build_global_stringptr s "str" builder
      | SArray arr -> 
          let arr' = L.build_array_malloc (lltype_of_dtype (fst (List.hd arr))) (L.const_int i32_t (List.length arr)) "arr" builder in
          List.iteri (
            fun ind item -> 
              let item' = build_expr builder item in
              let ind' = L.build_in_bounds_gep arr' [| L.const_int i32_t ind |] "ind" builder in
              ignore (L.build_store item' ind' builder)
          ) arr; arr'
      | SDefaultArray (typ, n) -> 
          let size = build_expr builder n in
          let size_op = L.int64_of_const (L.const_fptosi size i32_t) in
          let size' = (match size_op with 
              Some act -> let act' = Int64.to_int act in if act' < 1 then raise (Failure "invalid array size") else act'
            | None -> raise (Failure "invalid array size")
          ) in
          let arr = L.build_array_malloc (lltype_of_dtype typ) (L.const_int i32_t size') "arr" builder in
          let elem = default_value typ in
          let rec fill_array ind = 
            if ind = size' then ()
            else
              let ind' = L.build_in_bounds_gep arr [| L.const_int i32_t ind |] "ind" builder in
              ignore (L.build_store elem ind' builder); 
              fill_array (ind + 1) 
          in
          fill_array 0; arr
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
              | Mod     -> L.build_frem
              | And     -> L.build_and
              | Or      -> L.build_or
              | Eq      -> L.build_fcmp L.Fcmp.Oeq
              | Neq     -> L.build_fcmp L.Fcmp.One
              | Less    -> L.build_fcmp L.Fcmp.Olt
              | Leq     -> L.build_fcmp L.Fcmp.Ole
              | Greater -> L.build_fcmp L.Fcmp.Ogt
              | Geq     -> L.build_fcmp L.Fcmp.Oge
              | _       -> raise (Failure "unimplemented 4")
          ) e1' e2' "bop" builder
      | SUnop (op, e) ->
          let e' = build_expr builder e in
          (
            match op with
                Not -> L.build_not
              | Neg -> L.build_fneg
              | _   -> raise (Failure "unimplemented 5")
          ) e' "tmp" builder
      | SCall ("say", [e]) -> L.build_call printf_func [| format_string_of_dtype (fst e) false false ; (build_expr builder e) |] "" builder
      | SCall ("shout", [e]) -> L.build_call printf_func [| format_string_of_dtype (fst e) false true ; (build_expr builder e) |] "" builder
      | SCall ("inputc", []) -> read_typ Char
      | SCall ("inputn", []) -> read_typ Number
      | SCall (id, params) -> 
          let (fdef, fn') = Hashtbl.find all_funcs id in
          let llargs = List.rev (List.map (build_expr builder) (List.rev params)) in
          let res = if fn'.srtype = None then "" else id ^ "_result" in
          L.build_call fdef (Array.of_list llargs) res builder
      | SElem (id, sc, ind) -> 
          let arr = Hashtbl.find (List.nth !scopes sc) id in
          let loc = build_expr builder ind in
          let ind_op = L.int64_of_const (L.const_fptosi loc i32_t) in
          let ind' = (match ind_op with 
              Some act -> let act' = Int64.to_int act in if act' < 0  then raise (Failure "invalid array index") else act'
            | None -> raise (Failure "invalid array size")
          ) in
          let act_ind = L.build_in_bounds_gep arr [| L.const_int i32_t ind' |] "ind" builder in
          L.build_load act_ind id builder
    in

    let add_terminal builder instr = 
      match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder)
    in

    let rec build_stmt builder = function
        SExpr sexp -> ignore (build_expr builder sexp); builder
      | SInit (id, sexp) -> 
          let typ = fst sexp in
          let local = L.build_alloca (lltype_of_dtype typ) id builder
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
      | SReturn sexp -> 
          ignore (
            if fn.srtype = None then L.build_ret_void builder
            else L.build_ret (build_expr builder sexp) builder
          ); builder
      | _ -> raise (Failure "unimplemented 6")
    and do_block scope bb block = 
      scopes := scope::(!scopes);
      let res = List.fold_left build_stmt (L.builder_at_end context bb) block in
      scopes := List.tl (!scopes);
      res
    in
    
    let builder = List.fold_left build_stmt builder fn.sbody in

    if fn.srtype = None then add_terminal builder (L.build_ret_void)
    else ()

  in List.iter build_func_body sfuncs;
  mdl
