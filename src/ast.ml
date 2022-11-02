type dtype = 
 Number 
 | Bool 
 | Char 
 | String 
 | List of dtype

type bop = 
 Plus 
 | Minus 
 | Times 
 | IntDiv 
 | Div 
 | Mod 
 | Eq 
 | Neq 
 | Less 
 | Leq 
 | Greater 
 | Geq 
 | And 
 | Or

type expr = 
  Asn of string * expr
| NumberLit of float 
| BoolLit of bool 
| StringLit of string 
| CharLit of char 
| Id of string 
| Binop of expr * bop * expr

type stmt = 
  Decl of dtype * string * expr
| Say of expr

type cmd = Expr of expr | Stmt of stmt

type program = cmd list


let do_stmt = function
  | Decl(data_type, name, value) -> print_endline "Assignment"
  | Say(expr) -> (
      match expr with
        | NumberLit n -> 
            let string_of_n = if classify_float (fst (modf n)) == FP_zero then string_of_int (Float.to_int n) else string_of_float n
            in print_endline string_of_n
        | BoolLit b -> if b then print_endline "true" else print_endline "false"
        | StringLit s -> print_endline s
        | CharLit c -> print_endline (Char.escaped c)
        | Id _ -> print_endline "pass"
        | Binop _ -> print_endline "pass"
    )

let do_cmd = function
  | Expr e -> print_endline "pass"
  | Stmt s -> do_stmt s

let do_program (prog: program): unit = List.iter do_cmd prog
