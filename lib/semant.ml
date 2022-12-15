open Ast
open Sast

let dangling_code_err name = "function " ^ name ^ " has code that appears after return statement"
let duplicate_id_err name = "variable name " ^ name ^ " already exists in scope"
let duplicate_func_err name = "function name " ^ name ^ " already exists"
let duplicate_param_name_err name = "function parameters must have unique names, but " ^ name ^ " appears multiple times"
let invalid_assignment_err name = "variable type does not match assigned value for " ^ name
let invalid_bop_args_err op = "invalid arguments for binary operator " ^ op
let invalid_cond_loop_err typ = "conditional loop must take in boolean, but " ^ typ ^ " is passed"
let invalid_if_err typ = "if must take in boolean, but " ^ typ ^ " is passed"
let invalid_iter_loop_err typs = "iterative loop must take in (number, number, number), but (" ^ String.concat ", " typs ^ ") is passed"
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
let unimplemented_err = "unimplemented"
  
let reserved_funcs = ["main"; "say"]

let check (binds, funcs, stmts): sprogram =

  (**** Global variables ****)
  let globals = Hashtbl.create (List.length binds) in
  List.iter (
    fun (typ, name) -> 
      if Hashtbl.mem globals name then raise (Failure (duplicate_id_err name))
      else Hashtbl.add globals name typ
  ) binds;
  
  (**** Check for reserved functions ****)
  let sfuncs = Hashtbl.create (List.length funcs + 2) in
  List.iter (
    fun fn -> 
      if List.exists ((=) fn.fname) reserved_funcs then raise (Failure (reserved_function_name_err fn.fname))
      else if Hashtbl.mem sfuncs fn.fname then raise (Failure (duplicate_func_err fn.fname))
      else Hashtbl.add sfuncs fn.fname (fn.params, fn.rtype)
  ) funcs;
  Hashtbl.add sfuncs "say" ([(Any, "arg")], None);
  Hashtbl.add sfuncs "shout" ([(Any, "arg")], None);
  Hashtbl.add sfuncs "inputc" ([], Char);
  Hashtbl.add sfuncs "inputn" ([], Number);
  let funcs = funcs @ [{fname="main"; params=[]; rtype=None; body=stmts}] in
  Hashtbl.add sfuncs "main" ([], None);

  let check_func fn = 
    let scopes = ref [globals] in
    
    let rec check_expr = function
        NumberLit num -> (Number, SNumberLit num)
      | BoolLit bl -> (Bool, SBoolLit bl)
      | CharLit chr -> (Char, SCharLit chr)
      | StringLit str -> (String, SStringLit str)
      | Id id -> 
          let rec find_id ind sc = 
            match sc with
              [] -> raise (Failure (missing_id_err id))
            | hd::tl -> if Hashtbl.mem hd id then (Hashtbl.find hd id, SId (id, ind)) else find_id (ind + 1) tl
          in find_id 0 !scopes
      | Binop (exp1, op, exp2) -> 
          let sexpr1 = ref (check_expr exp1) in let dtype1 = fst !sexpr1 in
          let sexpr2 = check_expr exp2 in let dtype2 = fst sexpr2 in
          let tmp_op = ref op in 
          if dtype1 != dtype2 then raise (Failure (mismatched_bop_args_err (string_of_bop op)))
          else let res_type = (
            match op with 
                (Plus | Minus | Times | Div | Mod) when dtype1 = Number -> Number
              | IntDiv when dtype1 = Number -> 
                  sexpr1 := (Number, SBinop (!sexpr1, Minus, (Number, SBinop(!sexpr1, Mod, sexpr2)))); 
                  tmp_op := Div; 
                  Number
              | (Eq | Neq | Less | Leq | Greater | Geq) when dtype1 = Number -> Bool
              | (And | Or) when dtype1 = Bool -> Bool
              | _ -> raise (Failure (invalid_bop_args_err (string_of_bop op)))
          ) in
          (res_type, SBinop (!sexpr1, !tmp_op, sexpr2))
      | Unop (op, exp) ->
          let sexpr' = check_expr exp in let dtype' = fst sexpr' in
          let res_type = (
            match op with
                Not when dtype' = Bool -> Bool
              | Neg when dtype' = Number -> Number
              | _ -> raise (Failure (invalid_unop_args_err (match op with Not -> "not" | Neg -> "negation" | Abs -> "||")))
          ) in
          (res_type, SUnop (op, sexpr'))
      | Call (id, passed_params) -> 
          let (fn_params, fn_rtype) = 
            if not (Hashtbl.mem sfuncs id) then raise (Failure (missing_func_err id))
            else Hashtbl.find sfuncs id in 
          if List.length passed_params != List.length fn_params then raise (
            Failure (unequal_func_args_count_err id  (string_of_int (List.length fn_params)) (string_of_int (List.length passed_params)))
          )
          else List.iter2 (
            fun passed_param fn_param -> 
              let (passed_dtype, _) = check_expr passed_param in 
              if passed_dtype = None then raise (Failure (none_arg_err id))
              else if passed_dtype != fst fn_param && not (fst fn_param = Any) then raise (
                Failure (mismatched_func_args_err id (string_of_dtype (fst fn_param)) (string_of_dtype passed_dtype))
              )
          ) passed_params fn_params;
          (fn_rtype, SCall (id, List.map check_expr passed_params))
      | Elem _ -> raise (Failure unimplemented_err)
    in

    let rec check_stmt = function
        Expr exp -> SExpr (check_expr exp)
      | Assign (typ, id, exp) -> 
          let sexpr' = check_expr exp in 
          if fst sexpr' = None then raise (Failure (none_assignment_err id))
          else if fst sexpr' != typ then raise (Failure (invalid_assignment_err id))
          else
            let curr_scope = List.hd !scopes in
            if Hashtbl.mem curr_scope id then
              let prev_dtype = Hashtbl.find curr_scope id in
              if prev_dtype != typ then raise (Failure (invalid_assignment_err id))
              else SReassign (id, 0, sexpr')
            else let _ = Hashtbl.add curr_scope id (fst sexpr') in SInit (id, sexpr')
      | InferAssign (id, exp) -> 
          let sexpr' = check_expr exp in 
          let curr_dtype = fst sexpr' in
          if curr_dtype = None then raise (Failure (none_assignment_err id))
          else
            let rec find_assign ind sc = 
              match sc with
                  [] -> Hashtbl.add (List.hd !scopes) id (fst sexpr'); SInit (id, sexpr')
                | hd::tl -> 
                    if Hashtbl.mem hd id then 
                      if Hashtbl.find hd id != curr_dtype then raise (Failure (invalid_assignment_err id))
                      else SReassign (id, ind, sexpr') 
                    else find_assign (ind + 1) tl
            in find_assign 0 !scopes
      | Alloc _ -> raise (Failure unimplemented_err)
      | AllocAssign _ -> raise (Failure unimplemented_err)
      | AllocInferAssign _ -> raise (Failure unimplemented_err)
      | If (prd, if_block, else_block) -> 
          let prd_sexpr = check_expr prd in
          if fst prd_sexpr != Bool then raise (Failure (invalid_if_err (string_of_dtype (fst prd_sexpr))))
          else 
            let if_sstmts = check_block (Hashtbl.create (List.length if_block)) if_block in
            let else_sstmts = check_block (Hashtbl.create (List.length else_block)) else_block in
            SIf (prd_sexpr, if_sstmts, else_sstmts)
      | IterLoop (id, st, en, by, loop_block) -> 
          let start_sexpr = check_expr st in
          let end_sexpr = check_expr en in
          let by_sexpr = check_expr by in 
          if fst start_sexpr != Number || fst end_sexpr != Number || fst by_sexpr != Number 
            then raise (Failure (
              invalid_iter_loop_err [string_of_dtype (fst start_sexpr); string_of_dtype (fst end_sexpr); string_of_dtype (fst by_sexpr)]))
          else 
            let block_scope = Hashtbl.create (List.length loop_block + 1) in
            let _ = Hashtbl.add block_scope id Number in
            let block_sstmts = check_block block_scope loop_block in
            SIterLoop (id, start_sexpr, end_sexpr, by_sexpr, block_sstmts)
      | CondLoop (prd, block) ->
          let prd_sexpr = check_expr prd in
          if fst prd_sexpr != Bool then raise (Failure (invalid_cond_loop_err (string_of_dtype (fst prd_sexpr))))
          else
            let block_sstmts = check_block (Hashtbl.create (List.length block)) block in
            SCondLoop (prd_sexpr, block_sstmts)
      | Return exp -> SReturn (check_expr exp)
      | Continue -> SContinue (* TODO implement (only valid in loops) *)
      | Stop -> SStop         (* TODO implement (only valid it loops) *)
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
                    | _ as typ -> if fst rtyp != typ then raise (Failure (mismatched_return_err fn.fname)) else true
                )
              | SIf (_, if_sstmts, else_sstmts) -> (ensure_valid_return if_sstmts) && (ensure_valid_return else_sstmts)
              | SIterLoop (_, _, _, _, block_sstmts) -> let _ = ensure_valid_return block_sstmts in false
              | SCondLoop (_, block_sstmts) -> let _ = ensure_valid_return block_sstmts in false
              | _ -> false
      ) false block in
    let has_valid_return = ensure_valid_return body_sstmts in
    if not has_valid_return && fn.rtype != None then raise (Failure (nonguaranteed_return_err fn.fname))
    else ();
    {
      sfname = fn.fname;
      sparams = fn.params;
      srtype = fn.rtype;
      sbody = body_sstmts;
    }
  in (binds, List.map check_func funcs)
