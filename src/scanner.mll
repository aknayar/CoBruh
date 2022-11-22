{
  open Parser

  let excess_indent_err = "too many indentations"
  let extra_space_err = "extra space"
  let mismatched_quote_err = "mismatched quotation"
  let illegal_character_err = "illegal character"
  
  let is_start_of_line = ref true
  let curr_scope = ref 0 (* same as number of indents *)
  let new_spacing = ref 0 (* counts number of single spaces at start of line *)

  let set_new_line () = is_start_of_line := true; new_spacing := 0

  let get_scope () = 
    if !new_spacing land 1 = 1 (* if new_spacing is odd *) then raise (Failure extra_space_err) 
    else !new_spacing / 2

  let rec make_dedent_list num_dedents = 
    if num_dedents = 0 then [] 
    else DEDENT::(make_dedent_list (num_dedents - 1))

  let dedent_to_zero () = make_dedent_list !curr_scope

  (* should be called on tokens that may appear at start of line *)
  let make_token_list token = 
    if !is_start_of_line then (
      is_start_of_line := false;
      let new_scope = get_scope () in
      let scope_diff = new_scope - !curr_scope in
      curr_scope := new_scope;
      if scope_diff > 1 then raise (Failure excess_indent_err)
      else if scope_diff = 1 then [INDENT; token]
      else if scope_diff = 0 then [token]
      else (* scope_diff < 0 *) (make_dedent_list (-scope_diff)) @ [token]  
    ) else [token]
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
['\t' '\r'] { token lexbuf } (* whitespace *)
| ' '       { if !is_start_of_line then new_spacing := !new_spacing + 1 else (); token lexbuf }
| '\n'      { let was_start = !is_start_of_line in set_new_line (); if not was_start then EOL::(token lexbuf) else token lexbuf } 
| '#'       { comment lexbuf } (* comment *)

(* Symbols *)
| '(' { make_token_list LPAREN }
| ')' { [RPAREN] }
| '[' { [LSQUARE] }
| ']' { [RSQUARE] }
| ',' { [COMMA] }
| ':' { [COLON] }
| '|' { make_token_list PIPE }

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
| "if"   { make_token_list IF }
| "else" { make_token_list ELSE }
| "loop" { make_token_list LOOP }
| "in"   { [IN] }
| "to"   { [TO] }
| "by"   { [BY] }

(* Functions *)
| "define" { make_token_list DEFINE }
| "none"   { [NONE] }
| "->"     { [GIVES] }
| "return" { make_token_list RETURN }

(* Data Types *)
| "number"    { make_token_list NUMBER }
| "boolean"   { make_token_list BOOL }
| "character" { make_token_list CHAR }
| "string"    { make_token_list STRING }

(* Literals *)
| number as lex    { make_token_list (NUMBERLIT (float_of_string lex)) }
| "true"           { make_token_list (BOOLLIT true) }
| "false"          { make_token_list (BOOLLIT false) }
| character as lex { make_token_list (CHARLIT lex.[1]) }
| string as lex    { make_token_list (STRINGLIT (String.sub lex 1 (String.length lex - 2))) }
| id as lex        { make_token_list (ID lex) }

| eof         { if not !is_start_of_line then EOL::(dedent_to_zero () @ [EOF]) else dedent_to_zero () @ [EOF] }
| ('"' | ''') { raise (Failure(mismatched_quote_err)) }
| _           { raise (Failure(illegal_character_err)) }

and comment = parse
  '\n' { let was_start = !is_start_of_line in set_new_line (); if not was_start then EOL::(token lexbuf) else token lexbuf }
| eof  { if not !is_start_of_line then EOL::(dedent_to_zero () @ [EOF]) else dedent_to_zero () @ [EOF] }
| _    { comment lexbuf }
