{
  open Parser

  let is_first_line = ref true
  let curr_indent_level = ref 0
  let rec count_spaces_with_n ws = match String.split_on_char '\n' ws with 
    | hd::tl -> (match tl with
                  | [hd] -> String.length hd
                  | _  -> 0)
    | _ -> 0
  let rec make_dedent_list num_dedents = if num_dedents = 0 then [] else DEDENT::(make_dedent_list (num_dedents - 1))

  let unnecessary_indentation_err = "unnecessary indentation"
  let excess_indent_err = "too many indentations"
  let illegal_character_err = "illegal character"
  let extra_space_err = "extra space"
  let mismatched_quote_err = "mismatched quotation"
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ascii = [' '-'!' '#'-'[' ']'-'~']

let indent = "  "
let eol = ' '* '\n'
let eol_ws = eol ' '*

let exponent = ('E' | 'e') digit+
let number = digit+ ('.' digit+)? exponent?
let id = letter (letter | digit | '_')*
let character = ''' ascii '''
let string = '"' ascii* '"'

rule token = parse
[' ' '\t' '\r'] { let _ = is_first_line := false in token lexbuf } (* whitespace *)
| eol_ws as ws  { let num_spaces = count_spaces_with_n ws in
                  if num_spaces mod 2 = 1 then raise (Failure extra_space_err)
                  else let indent_level = num_spaces / 2 in
                  let indent_diff = indent_level - !curr_indent_level in
                  let _ = (curr_indent_level := indent_level) in
                  if indent_diff > 1 then raise (Failure excess_indent_err)
                  else if indent_diff = 1 then [INDENT]
                  else if indent_diff < 0 then make_dedent_list (-indent_diff)
                  else token lexbuf }
| indent+       { if !is_first_line then raise (Failure unnecessary_indentation_err) else token lexbuf } (* when the first line is indented *)
| indent* '#'   { comment lexbuf } (* comment *) (* TODO doesn't work after colons *)

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

(* Data Types *)
| "number"    { [NUMBER] }
| "boolean"   { [BOOL] }
| "character" { [CHAR] }
| "string"    { [STRING] }
| "array"     { [ARRAY] }

(* Literals *)
| number as lex    { [NUMBERLIT (float_of_string lex)] }
| "true"           { [BOOLLIT true] }
| "false"          { [BOOLLIT false] }
| character as lex { [CHARLIT lex.[1]] }
| string as lex    { [STRINGLIT (String.sub lex 1 (String.length lex - 2))] } (* remove quotes from string *)
| id as lex        { [ID lex] }

| eof         { [EOF] }
| ('"' | ''') { raise (Failure(mismatched_quote_err)) }
| _           { raise (Failure(illegal_character_err)) }

and comment = parse
  '#' { token lexbuf }
| _   { comment lexbuf }
