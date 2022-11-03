{
  open Parser
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ascii = [' '-'!' '#'-'[' ']'-'~']

let exponent = ('E' | 'e') digit+
let number = digit+ ('.' digit+)? exponent?
let id = letter (letter | digit | '_')*
let character = ''' ascii '''
let string = '"' ascii* '"'

rule token = parse
  [' ' '\r' '\n'] { token lexbuf } (* whitespace *)
| '\t'            { TAB } (* defines scope *)
| '#'             { comment lexbuf } (* comment *)

(* Symbols *)
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACE }
| ']' { RBRACE }
| '.' { PERIOD }
| ',' { COMMA }
| ':' { COLON }
| '|' { PIPE }

(* Operators *)
| "is"  { ASSIGN }
| '+'   { PLUS }
| '-'   { MINUS }
| '*'   { TIMES }
| "//"  { INTDIV }
| '/'   { DIV }
| '%'   { MOD }
| "=="  { EQ }
| "=/=" { NEQ }
| '<'   { LT }
| "<="  { LEQ }
| '>'   { GT }
| ">="  { GEQ }
| "and" { AND }
| "or"  { OR }
| "not" { NOT }

(* Branching *)
| "if"   { IF }
| "else" { ELSE }
| "loop" { LOOP }
| "in"   { IN }
| "to"   { TO }
| "by"   { BY }

(* Functions *)
| "call"   { CALL }
| "define" { DEFINE }
| "->"     { GIVES }
| "return" { RETURN }

(* Builtin Functions *)
| "use" { USE }

(* Data Types *)
| "number"    { NUMBER }
| "boolean"   { BOOL }
| "character" { CHAR }
| "string"    { STRING }
| "list"      { LIST }

(* Literals *)
| "none"           { NONE }
| number as lex    { NUMBERLIT (float_of_string lex) }
| "true"           { BOOLLIT true }
| "false"          { BOOLLIT false }
| string as lex    { STRINGLIT (String.sub lex 1 (String.length lex - 2)) } (* remove quotes from string *)
| character as lex { CHARLIT lex.[1] }
| id as lex        { ID lex }

| eof          {EOF}
| ('"' | ''')  { raise (Failure("Mismatched quotation")) }
| _            { raise (Failure("Illegal character")) }

and comment = parse
  '\n' { token lexbuf } (* comment ends at newline *)
| _    { comment lexbuf }
