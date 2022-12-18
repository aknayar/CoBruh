open Ast
open Sast

let dangling_code_err name = "function " ^ name ^ " has code that appears after return statement"
let duplicate_id_err name = "variable name " ^ name ^ " already exists in scope"
let duplicate_func_err name = "function name " ^ name ^ " already exists"
let duplicate_param_name_err name = "function parameters must have unique names, but " ^ name ^ " appears multiple times"
let inconsistent_array_err exp act = string_of_dtype exp ^ " array got type " ^ string_of_dtype act
let invalid_array_alloc_err name typ = "array allocation for " ^ name ^ " expects number but got " ^ string_of_dtype typ 
let invalid_array_index_err typ = "array index expects number but got " ^ string_of_dtype typ
let invalid_array_indexing_err name typ = "indexing is for arrays, but " ^ name ^ " is " ^ string_of_dtype typ
let invalid_assignment_err name exp act = "variables can be assigned to only one type, but " ^ name ^ " is assigned to both " ^ string_of_dtype exp ^ " and " ^ string_of_dtype act
let invalid_bop_args_err op = "invalid arguments for binary operator " ^ op
let invalid_cond_loop_err typ = "conditional loop expects boolean but got " ^ typ
let invalid_if_err typ = "if expects boolean but got " ^ typ
let invalid_indexing_err typ = "only arrays can be indexed, but " ^ string_of_dtype typ ^ " is indexed"
let invalid_iter_loop_err typs = "iterative loop expects (number, number, number) but got (" ^ String.concat ", " typs ^ ")"
let invalid_unop_args_err op = "invalid argument for unary operator " ^ op
let mismatched_func_args_err name exp act = "function call to " ^ name ^ " expected " ^ exp ^ " but got " ^ act
let mismatched_bop_args_err op = "mismatched arguments for binary operator " ^ op
let mismatched_return_err name = "incorrect function return type for " ^ name
let missing_func_err name = "function " ^ name ^ " does not exist"
let missing_id_err name = "variable " ^ name ^ " does not exist"
let none_arg_err name = "function call to " ^ name ^ " gets passed a none"
let none_assignment_err name = "cannot assign to none, but " ^ name ^ " tries to"
let none_return_err name typ = "function " ^ name ^ " has " ^ typ ^ " return type but does not return"
let nonguaranteed_return_err name = "function " ^ name ^ " is not guaranteed to return"
let reserved_function_name_err name = "function name " ^ name ^ " is reserved"
let return_in_none_err name typ = "function " ^ name ^ " has none return type but returns " ^ typ
let unequal_func_args_count_err name exp act = "function call to " ^ name ^ " expects " ^ exp ^ " arguments but got " ^ act

let internal_err = "internal error"
let unimplemented_err = "unimplemented"
  
let reserved_funcs = [("main", ([], None)); ("say", ([(Any, "arg")], None)); ("shout", ([(Any, "arg")], None));
                      ("inputc", ([], Char)); ("inputn", ([], Number)); ("abs", ([(Number, "n")], Number))]

let check (binds, funcs, stmts): sprogram =

  (**** Global variables ****)
  let globals = Hashtbl.create (List.length binds) in
  List.iter (
    fun (typ, name) -> 
      if Hashtbl.mem globals name then raise (Failure (duplicate_id_err name))
      else Hashtbl.add globals name (
        match typ with
            FixedArray (typ', _) -> Array typ'
          | _ as typ' -> typ'
      )
  ) binds;
  
  (**** Check for reserved functions ****)
  let sfuncs = Hashtbl.create (List.length funcs + List.length reserved_funcs) in
  List.iter (
    fun fn -> 
      if List.exists (fun rfn -> fn.fname = fst rfn) reserved_funcs then raise (Failure (reserved_function_name_err fn.fname))
      else if Hashtbl.mem sfuncs fn.fname then raise (Failure (duplicate_func_err fn.fname))
      else Hashtbl.add sfuncs fn.fname (fn.params, fn.rtype)
  ) funcs;
  let abs = {fname="abs"; params=[(Number, "n")]; rtype=Number; body=[If (Binop (Id ("n"), Less, NumberLit (0.)), [Return (Unop(Neg, Id("n")))], [Return (Id("n"))])]} in
  let funcs = funcs @ [{fname="main"; params=[]; rtype=None; body=stmts}; abs] in
  List.iter (fun rfn -> Hashtbl.add sfuncs (fst rfn) (snd rfn)) reserved_funcs;

  let check_func fn = 
    let scopes = ref [globals] in
    
    let rec check_expr = function
        NumberLit num -> (Number, SNumberLit num)
      | BoolLit bl -> (Bool, SBoolLit bl)
      | CharLit chr -> (Char, SCharLit chr)
      | StringLit str -> (String, SStringLit str)
      | ArrayLit arr -> 
          let sarr = List.map check_expr arr in 
          let typ = fst (List.hd sarr) in
          let res = List.map (
            fun item ->
              if fst item <> typ then raise (Failure (inconsistent_array_err typ (fst item)))
              else snd item
          ) sarr in (Array typ, SArrayLit (typ, None, Some res))
      | Id id -> 
          let rec find_id ind sc = 
            match sc with
                [] -> raise (Failure (missing_id_err id))
              | hd::tl -> if Hashtbl.mem hd id then (Hashtbl.find hd id, SId (id, ind)) else find_id (ind + 1) tl
          in find_id 0 !scopes
      | Elem (exp, ind) -> 
          let (exp_typ, exp_sx) = check_expr exp
          and (ind_typ, ind_sx) = check_expr ind in 
          if ind_typ <> Number then raise (Failure (invalid_array_index_err ind_typ))
          else (
            match exp_typ with 
                Array typ -> (typ, SElem (exp_sx, ind_sx))
              | _ as typ -> raise (Failure (invalid_indexing_err typ))
          ) 
      | Binop (exp1, op, exp2) -> 
          let sexpr1 = check_expr exp1 and (exp2_typ, exp2_sx) = check_expr exp2 in
          let exp1_typ = fst sexpr1 and exp1_sx = ref (snd sexpr1) in
          let tmp_op = ref op in 
          if exp1_typ <> exp2_typ then raise (Failure (mismatched_bop_args_err (string_of_bop op)))
          else let res_type = (
            match op with 
                (Plus | Minus | Times | Div | Mod) when exp1_typ = Number -> Number
              | IntDiv when exp1_typ = Number -> 
                  exp1_sx := SBinop (!exp1_sx, Minus, SBinop(!exp1_sx, Mod, exp2_sx)); 
                  tmp_op := Div; 
                  Number
              | (Eq | Neq | Less | Leq | Greater | Geq) when exp1_typ = Number -> Bool
              | (And | Or) when exp1_typ = Bool -> Bool
              | _ -> raise (Failure (invalid_bop_args_err (string_of_bop op)))
          ) in
          (res_type, SBinop (!exp1_sx, !tmp_op, exp2_sx))
      | Unop (op, exp) ->
          let (exp_typ, exp_sx) = check_expr exp in 
          let res_type = (
            match op with
                Not when exp_typ = Bool -> Bool
              | Neg when exp_typ = Number -> Number
              | Abs -> (
                  match exp_typ with
                      (Number | Array _) -> Number
                    | _ -> raise (Failure (invalid_unop_args_err "magnitude"))
                )
              | _ -> raise (Failure (invalid_unop_args_err (match op with Not -> "not" | Neg -> "negation" | Abs -> "magnitude")))
          ) in
          (res_type, SUnop (op, exp_sx))
      | ECall (id, passed_params) -> 
          let (fn_params, fn_rtype) = 
            if not (Hashtbl.mem sfuncs id) then raise (Failure (missing_func_err id))
            else Hashtbl.find sfuncs id in 
          if List.length passed_params <> List.length fn_params then raise (
            Failure (unequal_func_args_count_err id (string_of_int (List.length fn_params)) (string_of_int (List.length passed_params)))
          )
          else 
            let res = 
              List.map2 (
                fun passed_param fn_param -> 
                  let (passed_dtype, passed_sexpr) = check_expr passed_param in 
                  if passed_dtype = None then raise (Failure (none_arg_err id))
                  else if passed_dtype <> fst fn_param && not (fst fn_param = Any) then raise (
                    Failure (mismatched_func_args_err id (string_of_dtype (fst fn_param)) (string_of_dtype passed_dtype))
                  )
                  else (passed_dtype, passed_sexpr)
              ) passed_params fn_params in (fn_rtype, SECall (id, res))
    in

    let rec check_stmt = function
        Assign (typ, id, exp) -> 
          let (exp_typ, exp_sx) = check_expr exp in 
          if exp_typ = None then raise (Failure (none_assignment_err id))
          else if exp_typ <> typ then raise (Failure (invalid_assignment_err id exp_typ typ))
          else 
            let curr_scope = List.hd !scopes in
            if Hashtbl.mem curr_scope id then
              let prev_dtype = Hashtbl.find curr_scope id in
              if prev_dtype <> typ then raise (Failure (invalid_assignment_err id typ prev_dtype))
              else SReassign (SId (id, 0), exp_sx)
            else let _ = Hashtbl.add curr_scope id typ in SInit (typ, id, exp_sx)
      | InferAssign (item, exp) -> 
          let item' = try Some (check_expr item) with Failure _ -> None
          and (exp_typ, exp_sx) = check_expr exp in 
          (
            match item with
                Id id -> 
                  if exp_typ = None then raise (Failure (none_assignment_err id)) 
                  else (
                    match item' with
                        Some (typ', sid) -> (
                          match sid with
                              SId _ -> 
                                if typ' <> exp_typ then raise (Failure (invalid_assignment_err id typ' exp_typ))
                                else SReassign (sid, exp_sx)
                            | _ -> raise (Failure internal_err)
                        )
                      | None -> let _ = Hashtbl.add (List.hd !scopes) id exp_typ in SInit (exp_typ, id, exp_sx)
                  )
              | Elem (ele, _) ->
                  (
                    match ele with
                      Id id -> 
                        if exp_typ = None then raise (Failure (none_assignment_err id))
                        else (
                          match item' with
                            Some (typ', selem) -> (
                              match selem with
                                  SElem _ -> 
                                    if typ' <> exp_typ then raise (Failure (invalid_assignment_err id typ' exp_typ))
                                    else SReassign (selem, exp_sx)
                                | _ -> raise (Failure internal_err)
                            )
                          | None -> raise (Failure (missing_id_err id))
                        )
                    | _ -> raise (Failure internal_err)
                  )
              | _ -> raise (Failure internal_err)
          )
      | Alloc (typ, n, id) -> 
          let arr_typ = Array typ 
          and (n_typ, n_sx) = check_expr n in 
          if n_typ <> Number then raise (Failure (invalid_array_alloc_err id n_typ))
          else 
            let curr_scope = List.hd !scopes in
            if Hashtbl.mem curr_scope id then
              let prev_typ = Hashtbl.find curr_scope id in
              if prev_typ <> arr_typ then raise (Failure (invalid_assignment_err id prev_typ arr_typ))
              else SReassign (SId (id, 0), SArrayLit (typ, Some n_sx, None))
            else let _ = Hashtbl.add curr_scope id arr_typ in SInit (arr_typ, id, SArrayLit (typ, Some n_sx, None))
      | If (prd, if_block, else_block) -> 
          let (prd_typ, prd_sx) = check_expr prd in
          if prd_typ <> Bool then raise (Failure (invalid_if_err (string_of_dtype prd_typ)))
          else 
            let if_sstmts = check_block (Hashtbl.create (List.length if_block)) if_block in
            let else_sstmts = check_block (Hashtbl.create (List.length else_block)) else_block in
            SIf (prd_sx, if_sstmts, else_sstmts)
      | CondLoop (prd, block) ->
          let (prd_typ, prd_sx) = check_expr prd in
          if prd_typ <> Bool then raise (Failure (invalid_cond_loop_err (string_of_dtype prd_typ)))
          else
            let block_sstmts = check_block (Hashtbl.create (List.length block)) block in
            SCondLoop (prd_sx, block_sstmts)
      | Return exp -> SReturn (check_expr exp)
      | Continue -> SContinue
      | Stop -> SStop
      | SCall (id, params) -> SSCall (snd (check_expr (ECall (id, params))))
    and check_block scope block = 
      let _ = scopes := scope::(!scopes) in
      let res = List.map check_stmt block in
      let _ = scopes := List.tl (!scopes)
      in res
    in

    let body_scope = Hashtbl.create (List.length fn.params + List.length fn.body) in
    let _ = List.iter (
      fun (p_dtype, p_name) -> 
        if Hashtbl.mem body_scope p_name then raise (Failure (duplicate_param_name_err p_name))
        else Hashtbl.add body_scope p_name p_dtype
    ) fn.params in
    let body_sstmts = check_block body_scope fn.body in
    let rec ensure_valid_return block = 
      List.fold_left (
        fun is_dangling s ->
          if is_dangling then raise (Failure (dangling_code_err fn.fname))
          else
            match s with
                SReturn rtyp -> (
                  match fn.rtype with
                      None -> raise (Failure (return_in_none_err fn.fname (string_of_dtype (fst rtyp))))
                    | _ as typ -> if fst rtyp <> typ then raise (Failure (mismatched_return_err fn.fname)) else true
                )
              | SIf (_, if_sstmts, else_sstmts) -> (ensure_valid_return if_sstmts) && (ensure_valid_return else_sstmts)
              | SCondLoop (_, block_sstmts) -> let _ = ensure_valid_return block_sstmts in false
              | _ -> false
      ) false block in
    let has_valid_return = ensure_valid_return body_sstmts in
    if not has_valid_return && fn.rtype <> None then raise (Failure (nonguaranteed_return_err fn.fname))
    else ();
    {
      sfname = fn.fname;
      sparams = fn.params;
      srtype = fn.rtype;
      sbody = body_sstmts;
    }
  in (binds, List.map check_func funcs)
