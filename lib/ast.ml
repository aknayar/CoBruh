type dtype = 
    Number 
  | Bool 
  | Char 
  | String 
  | Array of dtype
  | None
  | Any

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

type uop = 
    Not
  | Neg
  | Abs

type expr = 
    NumberLit of float 
  | BoolLit of bool 
  | CharLit of char 
  | StringLit of string 
  | ArrayLit of expr list
  | Id of string 
  | Elem of string * expr
  | Binop of expr * bop * expr
  | Unop of uop * expr
  | Call of string * expr list
  
type stmt = 
    Expr of expr
  | Assign of dtype * string * expr
  | InferAssign of expr * expr
  | Alloc of dtype * expr * string
  | If of expr * stmt list * stmt list
  | CondLoop of expr * stmt list
  | Return of expr
  | Continue
  | Stop

type bind = dtype * string (* number x *)

(* 
  define foo(number bar -> string)

  func:
    fname: function name
    params: parameters
    rtype: return type
*)
type func = {
  fname: string;
  params: bind list;
  rtype: dtype;
  body: stmt list;
}

type program = bind list * func list * stmt list


let curr_indent_level = ref 0

let rec string_of_dtype = function
    Number -> "number"
  | Bool -> "boolean"
  | Char -> "character"
  | String -> "string"
  | Array typ -> string_of_dtype typ ^ "[]"
  | None -> "none"
  | Any -> "any"

let string_of_bop = function
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
  | ArrayLit a -> "[" ^ String.concat ", " (List.map string_of_expr a) ^ "]"
  | Id id -> id
  | Binop (e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_bop op ^ " " ^ string_of_expr e2
  | Unop (op, e) -> (match op with
      Not -> "not " ^ string_of_expr e
    | Neg -> "-" ^ string_of_expr e
    | Abs -> "|" ^ string_of_expr e ^ "|")
  | Call (f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Elem (l, e) -> l ^ "[" ^ string_of_expr e ^ "]"

let rec string_of_stmt s = 
  let string_of_stmt_raw = function
    Assign (t, id, e) -> string_of_dtype t ^ " " ^ id ^ " is " ^ string_of_expr e ^ "\n"
  | InferAssign (id, e) -> (match id with Id id -> id | Elem (id, ind) -> id ^ "[" ^ string_of_expr ind ^ "]" | _ -> raise (Failure "internal error")) ^ " is " ^ string_of_expr e ^ "\n"
  | Alloc (t, n, id) -> string_of_dtype t ^ "[" ^ string_of_expr n ^ "] " ^ id ^ "\n"
  | Expr ex -> string_of_expr ex ^ "\n"
  | If (e, s1, s2) ->
      let if_str = "if " ^ string_of_expr e ^ ":\n" in
      let _  = curr_indent_level := !curr_indent_level + 1 in
      let if_stmts = String.concat "" (List.map string_of_stmt s1) in
      let _  = curr_indent_level := !curr_indent_level - 1 in 
      let else_str = String.concat "" (List.init (!curr_indent_level) (fun _ -> "  ")) ^ "else:\n" in 
      let _  = curr_indent_level := !curr_indent_level + 1 in
      let else_stmts = String.concat "" (List.map string_of_stmt s2) in
      let _  = curr_indent_level := !curr_indent_level - 1 in
      if_str ^ if_stmts ^ else_str ^ else_stmts
  | CondLoop (e, st) ->
      let loop_str = "loop " ^ string_of_expr e ^ ":\n" in
      let _  = curr_indent_level := !curr_indent_level + 1 in
      let loop_stmts = String.concat "" (List.map string_of_stmt st) in
      let _  = curr_indent_level := !curr_indent_level - 1 in
      loop_str ^ loop_stmts
  | Return ex -> 
      "return " ^ string_of_expr ex ^ "\n"
  | Continue -> "continue\n"
  | Stop -> "stop\n" in
      String.concat "" (List.init (!curr_indent_level) (fun _ -> "  ")) ^ (string_of_stmt_raw s)

let string_of_bind (b: bind) = let (t, id) = b in string_of_dtype t ^ " " ^ id

let string_of_func_params (binds: bind list) = 
  match binds with
      [] -> "none"
    | _ -> String.concat ", " (List.map string_of_bind binds)
  
let string_of_func (fn: func) =
  let func_def = "define " ^ fn.fname 
  ^ " (" ^ string_of_func_params fn.params
  ^ " -> " ^ string_of_dtype fn.rtype ^ "):\n" in
  let _  = curr_indent_level := !curr_indent_level + 1 in
  let func_stmts = String.concat "" (List.map string_of_stmt fn.body) in
  let _  = curr_indent_level := !curr_indent_level - 1 in
  func_def ^ func_stmts

let string_of_program (binds, funcs, stmts) =
  "Parsed program: \n\n" ^
  String.concat "\n" (List.map string_of_bind binds) ^ "\n" ^
  String.concat "" (List.map string_of_func funcs) ^
  String.concat "" (List.map string_of_stmt stmts)
