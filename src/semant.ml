open Ast
open Sast

module StringMap = Map.Make(String)

let duplicate_err = "name already exists in scope"


(* tuple of var names map list (scopes), func names map (all global scope) *)

let check (prog: program) =
  let name_exists tbs name =
    let (vars, funcs) = tbs in (StringMap.mem name vars || StringMap.mem name funcs)
  in let check_assignment tbs t id e = 
    if name_exists tbs id then raise (Failure duplicate_err) 
    else (StringMap.add id t (fst tbs), snd tbs)
  in let check_func tbs fn =
    if name_exists tbs fn.fname then raise (Failure duplicate_err)
    else (fst tbs, StringMap.add fn.fname fn.rtype (snd tbs))

  in let add_decl_to_scope tbs = function
      Stmt st -> (
        match st with
            Assign (t, id, e) -> check_assignment tbs t id e
          | _ -> tbs
      )
    | Func fn -> check_func tbs fn
  in List.fold_left add_decl_to_scope (StringMap.empty, StringMap.empty) prog 