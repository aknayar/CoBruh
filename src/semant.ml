open Ast
open Sast

module StringMap = Map.Make(String)

let duplicate_err = "name already exists in scope"
let unimplemented_err = "unimplemented"


(* tuple of var names map list (scopes), func names map (all global scope) *)

let check (prog: program) =
  let name_exists tbs name =
    let (vars, funcs) = tbs in (StringMap.mem name vars || StringMap.mem name funcs)
  in let rec check_stmt tbs stmt = let (all_vars, funcs) = tbs in
    match stmt with
        Assign (t, id, e) -> if name_exists (List.hd all_vars, funcs) id then raise (Failure duplicate_err) 
          else ((StringMap.add id t (List.hd all_vars))::List.tl all_vars, funcs)
      | Reassign (id, e) -> raise (Failure unimplemented_err)
      | If (e, s1, s2) -> let _ = check_stmt_list (StringMap.empty::all_vars, funcs) s1 in 
          let _ = check_stmt_list (StringMap.empty::all_vars, funcs) s2 in tbs
      | IterLoop (id, s, e, b, st) -> let _ = check_stmt_list ((StringMap.add id Number StringMap.empty)::all_vars, funcs) st in tbs
      | CondLoop (e, st) -> let _ = check_stmt_list (StringMap.empty::all_vars, funcs) st in tbs
      | _ -> tbs (* no need to check scope *)
  and check_stmt_list tbs stmt_list = List.fold_left check_stmt tbs stmt_list

  in let check_func tbs fn =
    let (all_vars, funcs) = tbs in
    if name_exists (List.hd all_vars, funcs) fn.fname then raise (Failure duplicate_err)
    else let _  = check_stmt_list (StringMap.empty::all_vars, funcs) fn.body in 
    (all_vars, StringMap.add fn.fname fn.rtype funcs)

  in let check_decl tbs = function
      Stmt st -> check_stmt tbs st
    | Func fn -> let res = check_func tbs fn in res
  in let check_program tbs = List.fold_left check_decl tbs prog
  in check_program ([StringMap.empty], StringMap.empty)
  