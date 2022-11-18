open Ast
open Sast

module StringMap = Map.Make(String)

let duplicate_id_err = "variable name already exists in scope"
let missing_id_err = "variable does not exist"
let invalid_assignment_err = "variable type does not match assigned value"
let invalid_if_err = "if must take in a boolean expression"
let invalid_iter_loop_err = "iterative loop must take in numbers"
let invalid_cond_loop_err = "conditional loop must take in a boolean expression"
let duplicate_func_err = "function name already exists"
let missing_func_err = "function does not exist"
let invalid_bop_args_err = "invalid arguments for binary operator"
let mismatched_bop_args_err = "mismatched arguments for binary operator"
let invalid_unop_args_err = "invalid argument for unary operator"
let none_return_err = "function with non-none return type does not return anything"
let mismatched_func_args_err = "mismatched arguments for function call"
let return_in_global_err = "cannot return outside a function" (* TODO: implement *)
let return_in_none_err = "function that returns none cannot have return statement"
let mismatched_return_err = "incorrect function return type"
let missing_return_err = "missing return statement"
let unimplemented_err = "unimplemented"

let check (prog: program): sprogram =
  (* all_funcs contains all function definitions *)
  let all_funcs = Hashtbl.create 10 in
  (* all_scope is a list of hashtables, with each table representing a scope *)
  let all_scopes = ref [Hashtbl.create 10] in

  let get_function func_name = 
    if not (Hashtbl.mem all_funcs func_name) then raise (Failure missing_func_err)
    else Hashtbl.find all_funcs func_name 
  in
  let get_current_scope () (* for implicit decls *) = List.hd !all_scopes in
  let find_scope_with_name_opt name (* for explicit decls *) = List.find_opt (fun scope -> Hashtbl.mem scope name) !all_scopes in
  let find_scope_with_name name = 
    match find_scope_with_name_opt name with 
        Some scope -> scope
      | None -> raise (Failure missing_id_err)
  in
  let find_variable_with_name name (* for finding Ids *) = Hashtbl.find (find_scope_with_name name) name in
  
  let rec check_expr = function
      NumberLit n -> (Number, SNumberLit n)
    | BoolLit b -> (Bool, SBoolLit b)
    | CharLit c -> (Char, SCharLit c)
    | StringLit s -> (String, SStringLit s)
    | Id id -> find_variable_with_name id
    | Binop (e1, op, e2) -> 
        let sexpr1 = check_expr e1 in let (dtype1, expr1) = sexpr1 in
        let sexpr2 = check_expr e2 in let (dtype2, expr2) = sexpr2 in
        if dtype1 != dtype2 then raise (Failure mismatched_bop_args_err)
        else let res_type = (
          match op with 
              (Plus | Minus | Times | IntDiv | Div | Mod) when dtype1 = Number -> Number
            | (Eq | Neq | Less | Leq | Greater | Geq) when dtype1 = Number -> Bool
            | (And | Or) when dtype1 = Bool -> Bool
            | _ -> raise (Failure invalid_bop_args_err)
        ) in
        (res_type, SBinop (sexpr1, op, sexpr2))
    | Unop (op, e) ->
        let sexpr' = check_expr e in let (dtype', expr') = sexpr' in
        let res_type = (
          match op with
              Not when dtype' = Bool -> Bool
            | Neg when dtype' = Number -> Number
            | _ -> raise (Failure invalid_unop_args_err)
        ) in
        (res_type, SUnop (op, sexpr'))
    | Call (id, passed_params) -> 
        let fn = get_function id in (
          match fn.rtype with
              None -> raise (Failure none_return_err)
            | DType res_type -> 
                if List.length passed_params != List.length fn.params then raise (Failure mismatched_func_args_err)
                else if List.exists2 (
                  fun passed_param fn_param -> let (passed_dtype, _) = check_expr passed_param 
                  in passed_dtype != fst fn_param
                ) passed_params fn.params then raise (Failure mismatched_func_args_err)
                else (res_type, SCall (id, (List.map check_expr passed_params)))
        )
    | Elem (id, ind) -> raise (Failure unimplemented_err)


  let name_exists tables name = 
    let (ids, funcs) = tables in (StringMap.mem name ids || StringMap.mem name funcs)
  in let rec find_id name ids_table = match ids_table with
      [] -> raise (Failure missing_id_err)
    | curr_ids::outer_ids -> if StringMap.mem name curr_ids then StringMap.find name curr_ids else find_id name outer_ids

  in let rec check_expr tbs = function
      NumberLit n -> (Number, SNumberLit n)
    | BoolLit b -> (Bool, SBoolLit b)
    | StringLit s -> (String, SStringLit s)
    | CharLit c -> (Char, SCharLit c)
    | Id id -> (find_id id (fst tbs), SId id)
    | Binop (e1, op, e2) -> let (type1, sexpr1) = check_expr tbs e1 in let (type2, sexpr2) = check_expr tbs e2 in
        let final_type = if type1 = type2 then (
          match op with
              (Plus | Minus | Times | IntDiv | Div | Mod) when type1 = Number -> Number
            | (Eq | Neq | Less | Leq | Greater | Geq) when type1 = Number -> Bool
            | (And | Or) when type1 = Bool -> Bool
            | _ -> raise (Failure invalid_bop_args_err)
        ) else raise (Failure mismatched_bop_args_err)
      in (final_type, SBinop ((type1, sexpr1), op, (type2, sexpr2)))
    | Unop (op, e) -> let (expr_type, s_expr) = check_expr tbs e in
        let final_type = (
          match op with
              Not when expr_type = Bool -> Bool
            | Neg when expr_type = Number -> Number
            | _ -> raise (Failure invalid_unop_args_err)
        ) in (final_type, SUnop (op, (expr_type, s_expr)))
    | Call (id, e_list) -> if not (StringMap.mem id (snd tbs)) then raise (Failure missing_func_err)
        else let fn = StringMap.find id (snd tbs) in (
          match fn.rtype with
              None -> raise (Failure none_return_err)
            | DType final_type -> if List.length e_list != List.length fn.params then raise (Failure mismatched_func_args_err)
                else if List.exists2 (fun arg param -> let (arg_type, _) = check_expr tbs arg in arg_type != fst param) e_list fn.params then raise (Failure mismatched_func_args_err) 
                else (final_type, SCall (id, (List.map (check_expr tbs) e_list)))
        )

  in let rec check_stmt tbs stmt = let (all_ids, funcs) = tbs in
    match stmt with
        Expr e -> (tbs, SExpr (check_expr tbs e))
      | Assign (t, id, e) -> if name_exists (List.hd all_ids, funcs) id then raise (Failure duplicate_id_err) 
          else let (expr_type, s_expr) = check_expr tbs e in
          if t != expr_type then raise (Failure invalid_assignment_err)
          else (((StringMap.add id t (List.hd all_ids))::List.tl all_ids, funcs), SAssign (t, id, (expr_type, s_expr)))
      | Reassign (id, e) -> let (expr_type, s_expr) = check_expr tbs e in
          if (find_id id all_ids) != expr_type then raise (Failure invalid_assignment_err)
          else (tbs, SReassign (id, (expr_type, s_expr)))
      | If (e, s1, s2) -> let (expr_type, s_expr) = check_expr tbs e in
          if expr_type != Bool then raise (Failure invalid_if_err)
          else (tbs, SIf ((expr_type, s_expr), snd (check_stmt_list (StringMap.empty::all_ids, funcs) s1), snd (check_stmt_list (StringMap.empty::all_ids, funcs) s2)))
      | IterLoop (id, s, e, b, st) -> (* id already assumed to be number *) 
          let (start_type, start_sexpr) = check_expr tbs s in let (end_type, end_sexpr) = check_expr tbs e in let (by_type, by_expr) = check_expr tbs b in 
          if start_type != Number || end_type != Number || by_type != Number then raise (Failure invalid_iter_loop_err) 
          else (tbs, SIterLoop(id, (start_type, start_sexpr), (end_type, end_sexpr), (by_type, by_expr), snd (check_stmt_list ((StringMap.add id Number StringMap.empty)::all_ids, funcs) st)))
      | CondLoop (e, st) -> let (expr_type, s_expr) = check_expr tbs e in if expr_type != Bool then raise (Failure invalid_cond_loop_err) 
          else (tbs, SCondLoop((expr_type, s_expr), snd (check_stmt_list (StringMap.empty::all_ids, funcs) st)))
      | Return e -> (tbs, SReturn (check_expr tbs e))
  and check_stmt_list tables stmt_list = List.fold_left_map check_stmt tables stmt_list

  in let check_func tbs fn =
    let (all_ids, funcs) = tbs in
    if name_exists (List.hd all_ids, funcs) fn.fname then raise (Failure duplicate_func_err)
    else let sstmt_list = snd (check_stmt_list tbs fn.body) in
    (* TODO: better return checking *)
    let _ = match fn.rtype with
        None -> List.iter (fun s_stmt -> match s_stmt with
              SReturn _ -> raise (Failure return_in_none_err)
            | _ -> () 
          ) sstmt_list 
      | DType r_type -> let return_exists = List.fold_left (fun return_found s_stmt -> match s_stmt with
              SReturn return_sexpr -> if r_type != fst return_sexpr then raise (Failure mismatched_return_err) else true
            | _ -> return_found
          ) false sstmt_list in
          if not return_exists then raise (Failure missing_return_err) else ()
    in ((all_ids, StringMap.add fn.fname fn funcs), {
      sfname = fn.fname;
      sparams = fn.params;
      srtype = fn.rtype;
      sbody = snd (check_stmt_list (StringMap.empty::all_ids, funcs) fn.body);
    })

  in let check_decl tbs = function
      Stmt st -> let result = check_stmt tbs st in (fst result, SStmt (snd result))
    | Func fn -> let result = check_func tbs fn in (fst result, SFunc (snd result))

  in snd (List.fold_left_map check_decl ([StringMap.empty], StringMap.empty) prog)
  
