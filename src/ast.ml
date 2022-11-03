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
  Assign of string * expr
  | NumberLit of float 
  | BoolLit of bool 
  | StringLit of string 
  | CharLit of char 
  | Id of string 
  | Binop of expr * bop * expr
  | Call of string * expr list
  
type stmt = 
  Block of stmt list
| Decl of dtype * string * expr
| Expr of expr
| If of expr * stmt * stmt
| Loop of string * expr * expr * expr * stmt
| While of expr * stmt
| Return of expr

(* int x: name binding *)
type bind = dtype * string
type bind_value = dtype * string * expr

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: dtype;
  fname: string;
  formals: bind list;
  locals: bind_value list;
  body: stmt list;
}

type program = bind_value list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
    Plus -> "+"
  | Minus -> "-"
  | Eq -> "=="
  | Neq -> "=/="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"

let rec string_of_expr = function
    NumberLit(n) -> if classify_float (fst (modf n)) == FP_zero then string_of_int (Float.to_int n) else string_of_float n
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> Char.escaped c
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | Loop(id, s, e, b, st) -> "loop " ^ id ^ " in " ^ string_of_expr s ^ " to " ^ string_of_expr e ^ " by " ^ string_of_expr b ^ string_of_stmt st

let rec string_of_dtype = function
    Number -> "number"
  | Bool -> "boolean"
  | Char -> "character"
  | String -> "string"
  | List(d) -> string_of_dtype d ^ "list"

let string_of_vinit (t, id, e) = string_of_dtype t ^ " " ^ id ^ " " ^ string_of_expr e ^ ";\n"

let string_of_vdecl (t, id) = string_of_dtype t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_dtype fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vinit fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
