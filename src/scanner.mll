{
  open Parser

  let curr_indent_level = ref 0
  let rec count_indents_with_n ws = (String.length ws - 1) / 2 (* should be length of indent defined below *)
  let rec make_dedent_list num_dedents = if num_dedents = 0 then [] else DEDENT::(make_dedent_list (num_dedents - 1))

  let unnecessary_indentation_err = "unnecessary indentation"
  let excess_indent_err = "too many indentations"
  let illegal_char_err = "illegal character"
  let mismatched_quote_err = "mismatched quotation"
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ascii = [' '-'!' '#'-'[' ']'-'~']

let indent = "  "
let eol_ws = '\n' indent*

let exponent = ('E' | 'e') digit+
let number = digit+ ('.' digit+)? exponent?
let id = letter (letter | digit | '_')*
let character = ''' ascii '''
let string = '"' ascii* '"'

rule token = parse
[' ' '\t' '\r'] { token lexbuf } (* whitespace *)
| eol_ws as ws  { let indent_level = count_indents_with_n ws in
                  let indent_diff = indent_level - !curr_indent_level in
                  let _ = (curr_indent_level := indent_level) in
                  if Int.abs indent_diff > 1 then raise (Failure excess_indent_err)
                  else if indent_diff = 1 then [INDENT]
                  else if indent_diff < 0 then make_dedent_list (-indent_diff)
                  else token lexbuf }
| indent+       { raise (Failure unnecessary_indentation_err) }
| '#'           { comment lexbuf } (* comment *)

(* Symbols *)
| '(' { [LPAREN] }
| ')' { [RPAREN] }
| '[' { [LSQUARE] }
| ']' { [RSQUARE] }
| '.' { [PERIOD] }
| ',' { [COMMA] }
| ':' { [COLON] }
| '|' { [PIPE] }

(* Operators *)
| "is"  { [ASSIGN] }
| '+'   { [PLUS] }
| '-'   { [MINUS] }
| '*'   { [TIMES] }
| "//"  { [INTDIV] }
| '/'   { [DIV] }
| '%'   { [MOD] }
| "=="  { [EQ] }
| "=/=" { [NEQ] }
| '<'   { [LT] }
| "<="  { [LEQ] }
| '>'   { [GT] }
| ">="  { [GEQ] }
| "and" { [AND] }
| "or"  { [OR] }
| "not" { [NOT] }

(* Branching *)
| "if"   { [IF] }
| "else" { [ELSE] }
| "loop" { [LOOP] }
| "in"   { [IN] }
| "to"   { [TO] }
| "by"   { [BY] }

(* Functions *)
| "call"   { [CALL] }
| "define" { [DEFINE] }
| "none"   { [NONE] }
| "->"     { [GIVES] }
| "return" { [RETURN] }

(* Builtin Functions *)
| "use" { [USE] }

(* Data Types *)
| "number"    { [NUMBER] }
| "boolean"   { [BOOL] }
| "character" { [CHAR] }
| "string"    { [STRING] }
| "list"      { [LIST] }

(* Literals *)
| number as lex    { [NUMBERLIT (float_of_string lex)] }
| "true"           { [BOOLLIT true] }
| "false"          { [BOOLLIT false] }
| character as lex { [CHARLIT lex.[1]] }
| string as lex    { [STRINGLIT (String.sub lex 1 (String.length lex - 2))] } (* remove quotes from string *)
| id as lex        { [ID lex] }

| eof         { [EOF] }
| ('"' | ''') { raise (Failure(mismatched_quote_err)) }
| _           { raise (Failure(illegal_char_err)) }

and comment = parse
  ('\n' | eof) { token lexbuf } (* comment ends at newline *)
| _            { comment lexbuf }
