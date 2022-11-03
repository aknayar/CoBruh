type dtype = 
    Number 
  | Bool 
  | Char 
  | String 
  | List of dtype

type func_rtype =
    DType of dtype
  | None

type op = 
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
    NumberLit of float 
  | BoolLit of bool 
  | StringLit of string 
  | CharLit of char 
  | Id of string 
  | Binop of expr * op * expr
  | Call of string * expr list
  
type stmt = 
    Expr of expr
  | Assign of dtype * string * expr
  | Reassign of string * expr
  | If of expr * stmt list * stmt list
  | Loop of string * expr * expr * expr * stmt list
  | Return of expr

type bind = dtype * string (* number x, only appears in function parameters *)

(* 
  define foo(number bar -> string)

  func_def:
    fname: function name
    params: parameters
    rtype: return type
*)
type func_def = {
  fname: string;
  params: bind list;
  rtype: func_rtype;
  body: stmt list;
}

type program = stmt list * func_def list

let rec string_of_dtype = function
    Number -> "number"
  | Bool -> "boolean"
  | Char -> "character"
  | String -> "string"
  | List d -> string_of_dtype d ^ "list"

let string_of_func_rtype = function
    DType typ -> string_of_dtype typ
  | None -> "none"

let string_of_op = function
    Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | IntDiv -> "//"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "=/="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"

let rec string_of_expr = function
    NumberLit n -> if classify_float (fst (modf n)) == FP_zero then string_of_int (Float.to_int n) else string_of_float n
  | BoolLit b -> if b then "true" else "false"
  | CharLit c -> "'" ^ Char.escaped c ^ "'"
  | StringLit s -> "\"" ^ s ^ "\""
  | Id id -> id
  | Binop (e1, op, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2
  | Call (f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
    Assign (t, id, e) -> string_of_dtype t ^ " " ^ id ^ " is " ^ string_of_expr e ^ ".\n"
  | Reassign (id, e) -> id ^ " is " ^ string_of_expr e ^ ".\n"
  | Expr ex -> string_of_expr ex ^ ".\n"
  | If (e, s1, s2) ->  "if " ^ string_of_expr e ^ ": {\n" ^ String.concat "" (List.map string_of_stmt s1) ^ "}\nelse: {\n" ^ String.concat "" (List.map string_of_stmt s2) ^ "}\n"
  | Loop (id, s, e, b, st) -> "loop " ^ id ^ " in " ^ string_of_expr s ^ " to " ^ string_of_expr e ^ " by " ^ string_of_expr b ^ ": {\n" ^ String.concat "" (List.map string_of_stmt st) ^ "}\n"
  | Return ex -> "return " ^ string_of_expr ex ^ ".\n"

let string_of_bind (b: bind) = let (t, id) = b in string_of_dtype t ^ " " ^ id

let string_of_func_params (binds: bind list) = 
  match binds with
      [] -> "none"
    | _ -> String.concat ", " (List.map string_of_bind binds)
  
let string_of_func_def (fn: func_def) = "define " ^ fn.fname 
  ^ " (" ^ string_of_func_params fn.params
  ^ " -> " ^ string_of_func_rtype fn.rtype ^ "): {\n"
  ^ String.concat "" (List.map string_of_stmt fn.body) ^ "}\n"

let string_of_program (prog: program) =
  "Parsed program: \n\n" ^
  String.concat "" (List.map string_of_stmt (fst prog)) ^ "\n" ^
  String.concat "\n" (List.map string_of_func_def (snd prog))
