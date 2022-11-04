open Ast
open Sast

module StringMap = Map.Make(String)

let duplicate_err = "name already exists in scope"
let missing_id_err = "variable does not exist"
let missing_func_err = "function does not exist"
let invalid_bop_args_err = "invalid arguments for binary operator"
let mismatched_bop_args_err = "mismatched arguments for binary operator"
let invalid_unop_args_err = "invalid argument for unary operator"
let none_return_err = "function does not return anything"
let mismatched_func_args_err = "mismatched arguments for function call"
let unimplemented_err = "unimplemented"

let check (prog: program) =
  let name_exists tbs name =
    let (ids, funcs) = tbs in (StringMap.mem name ids || StringMap.mem name funcs)
  in let rec find_id name ids = match ids with
      [] -> raise (Failure missing_id_err)
    | curr_ids::outside_ids -> if StringMap.mem name curr_ids then StringMap.find name curr_ids else find_id name outside_ids
  in let rec check_expr tbs = function
      NumberLit n -> (Number, SNumberLit n)
    | BoolLit b -> (Bool, SBoolLit b)
    | StringLit s -> (String, SStringLit s)
    | CharLit c -> (Char, SCharLit c)
    | Id id -> (find_id id (fst tbs), SId id)
    | Binop (e1, op, e2) -> let (t1, se1) = check_expr tbs e1 in let (t2, se2) = check_expr tbs e2 in
        let t' = if t1 = t2 then (
          match op with
              (Plus | Minus | Times | IntDiv | Div | Mod | Eq | Neq | Less | Leq | Greater | Geq) when t1 = Number -> Number
            | (And | Or) when t1 = Bool -> Bool
            | _ -> raise (Failure invalid_bop_args_err)
        ) else raise (Failure mismatched_bop_args_err)
      in (t', SBinop ((t1, se1), op, (t2, se2)))
    | Unop (op, e) -> let (t, se) = check_expr tbs e in
        let t' = (
          match op with
              Not when t = Bool -> Bool
            | Neg when t = Number -> Number
            | _ -> raise (Failure invalid_unop_args_err)
        ) in (t', SUnop (op, (t, se)))
    | Call (id, e_list) -> if not (StringMap.mem id (snd tbs)) then raise (Failure missing_func_err)
        else let fn = StringMap.find id (snd tbs) in (
          match fn.rtype with
              None -> raise (Failure none_return_err)
            | DType t' -> if List.length e_list != List.length fn.params then raise (Failure mismatched_func_args_err)
                else if List.exists2 (fun e p -> let (t, _) = check_expr tbs e in t != fst p) e_list fn.params then raise (Failure mismatched_func_args_err) 
                else (t', SCall (id, (List.map (check_expr tbs) e_list)))
        )
  in let rec check_stmt tbs stmt = let (all_ids, funcs) = tbs in
    match stmt with
        Assign (t, id, e) -> if name_exists (List.hd all_ids, funcs) id then raise (Failure duplicate_err) 
          else ((StringMap.add id t (List.hd all_ids))::List.tl all_ids, funcs)
      | Reassign (id, e) -> let _ = find_id id all_ids in tbs
      | If (e, s1, s2) -> let _ = check_stmt_list (StringMap.empty::all_ids, funcs) s1 in 
          let _ = check_stmt_list (StringMap.empty::all_ids, funcs) s2 in tbs
      | IterLoop (id, s, e, b, st) -> let _ = check_stmt_list ((StringMap.add id Number StringMap.empty)::all_ids, funcs) st in tbs
      | CondLoop (e, st) -> let _ = check_stmt_list (StringMap.empty::all_ids, funcs) st in tbs
      | _ -> tbs (* no need to check scope *)
  and check_stmt_list tbs stmt_list = List.fold_left check_stmt tbs stmt_list

  in let check_func tbs fn =
    let (all_ids, funcs) = tbs in
    if name_exists (List.hd all_ids, funcs) fn.fname then raise (Failure duplicate_err)
    else let _  = check_stmt_list (StringMap.empty::all_ids, funcs) fn.body in 
    (all_ids, StringMap.add fn.fname fn funcs)

  in let check_decl tbs = function
      Stmt st -> check_stmt tbs st
    | Func fn -> check_func tbs fn
  in List.fold_left check_decl ([StringMap.empty], StringMap.empty) prog
  