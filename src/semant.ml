open Ast
open Sast

module StringMap = Map.Make(String)

let duplicate_err = "name already exists in scope"


(* scopes for initialization: make sure name not in current scope *)

let check prog =
  let add_name_to_scope tb name = 
    if StringMap.mem name tb then raise (Failure duplicate_err) 
    else StringMap.add name () tb in
  let add_decl_to_scope symbol_table = function
      Stmt st -> (
        match st with
            Assign (t, id, e) -> add_name_to_scope symbol_table id
          | _ -> symbol_table
      )
    | Func fn -> add_name_to_scope symbol_table fn.fname
  in List.fold_left add_decl_to_scope StringMap.empty prog 