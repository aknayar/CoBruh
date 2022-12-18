module L = Llvm

open Ast
open Sast

let internal_err = "internal error"

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
    | _ -> raise (Failure internal_err) 
  in
  let default_value = function
      Number -> L.const_float (lltype_of_dtype Number) 0.0
    | (Bool | Char) as typ -> L.const_int (lltype_of_dtype typ) 0
    | String -> L.const_pointer_null (lltype_of_dtype String)
    | _ -> raise (Failure internal_err) 
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

    let prd_bbs = ref [] in
    let merge_bbs = ref [] in

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
        | _ -> raise (Failure internal_err)
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

    let rec build_expr builder exp = match exp with
        SNumberLit n -> L.const_float (lltype_of_dtype Number) n
      | SBoolLit b -> L.const_int (lltype_of_dtype Bool) (if b then 1 else 0)
      | SCharLit c -> L.const_int (lltype_of_dtype Char) (Char.code c)
      | SStringLit s -> L.build_global_stringptr s "str" builder
      | SArrayLit (typ, n, contents) -> (
          match n with
              Some n' -> 
                let size = build_expr builder n' in
                let size' = L.build_fptosi size i32_t "default_arr" builder in
                L.build_array_malloc (lltype_of_dtype typ) size' "arr" builder
            | None -> 
                let contents' = Option.get contents in
                let arr = L.build_array_malloc (lltype_of_dtype typ) (L.const_int i32_t (List.length contents')) "arr" builder in
                List.iteri (
                  fun ind item ->
                    let item' = build_expr builder item in
                    let ind' = L.build_in_bounds_gep arr [| L.const_int i32_t ind |] "ind" builder in
                    ignore (L.build_store item' ind' builder)
                ) contents'; arr
        )
      | SId (id, sc) -> L.build_load (Hashtbl.find (List.nth !scopes sc) id) id builder
      | SElem (container, ind) -> 
          let container' = build_expr builder container in
          let loc = build_expr builder ind in
          let ind' = L.build_fptosi loc i32_t "ind" builder in
          let elem = L.build_in_bounds_gep container' [| ind' |] "elem" builder in
          L.build_load elem "res" builder
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
              | IntDiv  -> raise (Failure internal_err)
          ) e1' e2' "bop" builder
      | SUnop (op, e) ->
          let e' = build_expr builder e in
          (
            match op with
                Not -> L.build_not
              | Neg -> L.build_fneg
              | _   -> raise (Failure "unimplemented")
          ) e' "tmp" builder
      | SECall ("say", [e]) -> L.build_call printf_func [| format_string_of_dtype (fst e) false false ; (build_expr builder (snd e)) |] "" builder
      | SECall ("shout", [e]) -> L.build_call printf_func [| format_string_of_dtype (fst e) false true ; (build_expr builder (snd e)) |] "" builder
      | SECall ("inputc", []) -> read_typ Char
      | SECall ("inputn", []) -> read_typ Number
      | SECall (id, params) -> 
          let (fdef, fn') = Hashtbl.find all_funcs id in
          let llargs = List.rev (List.map (fun item -> build_expr builder (snd item)) (List.rev params)) in
          let res = if fn'.srtype = None then "" else id ^ "_result" in
          L.build_call fdef (Array.of_list llargs) res builder
    in

    let add_terminal builder instr = 
      match L.block_terminator (L.insertion_block builder) with
          Some _ -> ()
        | None -> ignore (instr builder)
    in

    let rec build_stmt builder = function
        SInit (typ, id, sexp) -> 
          let local = L.build_alloca (lltype_of_dtype typ) id builder
          in Hashtbl.add (List.hd !scopes) id local;
          let sexp' = build_expr builder sexp in
          ignore (L.build_store sexp' local builder); builder
      | SReassign (lhs, sexp) -> 
          let sexp' = build_expr builder sexp in
          (
            match lhs with
                SId (id, sc) -> ignore (L.build_store sexp' (Hashtbl.find (List.nth !scopes sc) id) builder)
              | SElem (container, ind) -> 
                  let container' = build_expr builder container in
                  let loc = build_expr builder ind in
                  let ind' = L.build_fptosi loc i32_t "ind" builder in
                  let elem = L.build_in_bounds_gep container' [| ind' |] "elem" builder in
                  ignore (L.build_store sexp' elem builder)
              | _ -> raise (Failure internal_err)
          ); builder
      | SIf (prd, if_block, else_block) ->
          let bool_val = build_expr builder prd in
          let merge_bb = L.append_block context "merge_if" the_func in
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
          ignore(prd_bbs := prd_bb::!prd_bbs);
          
          let merge_bb = L.append_block context "merge_loop" the_func in
          ignore(merge_bbs := merge_bb::!merge_bbs);
          
          let block_bb = L.append_block context "loop_block" the_func in
          add_terminal (do_block (Hashtbl.create (List.length block)) block_bb block) (L.build_br prd_bb);
      
          let pred_builder = L.builder_at_end context prd_bb in
          let bool_val = build_expr pred_builder prd in

          prd_bbs := List.tl !prd_bbs;
          merge_bbs := List.tl !merge_bbs;
      
          ignore(L.build_cond_br bool_val block_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb
      | SContinue -> ignore(L.build_br (List.hd !prd_bbs) builder); builder
      | SStop -> ignore(L.build_br (List.hd !merge_bbs) builder); builder
      | SReturn sexp -> 
          ignore (
            if fn.srtype = None then L.build_ret_void builder
            else L.build_ret (build_expr builder (snd sexp)) builder
          ); builder
      | _ -> raise (Failure "unimplemented")
    and do_block scope bb block = 
      scopes := scope::(!scopes);
      let res = List.fold_left build_stmt (L.builder_at_end context bb) block in
      scopes := List.tl (!scopes);
      res
    in
    
    let builder = List.fold_left build_stmt builder fn.sbody in

    if fn.srtype = None then add_terminal builder (L.build_ret_void)
    else add_terminal builder (L.build_unreachable)

  in List.iter build_func_body sfuncs;
  mdl
