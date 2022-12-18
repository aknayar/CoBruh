%{
  open Ast
%}

%token INDENT DEDENT EOL
%token LPAREN RPAREN LSQUARE RSQUARE COMMA COLON PIPE 
%token ASSIGN PLUS MINUS TIMES INTDIV DIV MOD EQ NEQ LT LEQ GT GEQ AND OR NOT
%token IF ELSE LOOP CONTINUE STOP
%token DEFINE NONE GIVES RETURN
%token NUMBER BOOL CHAR STRING 
%token USE
%token TAB
%token EOF
%token <float> NUMBERLIT
%token <bool> BOOLLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token <string> ID

%start program
%type <Ast.program> program

%nonassoc ASSIGN
%left OR
%left AND
%right NOT
%nonassoc EQ NEQ
%nonassoc LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES INTDIV DIV MOD
%nonassoc UMINUS
%nonassoc LSQUARE RSQUARE

%%

program:
  decls stmt_list EOF { (List.rev (fst $1), List.rev (snd $1), $2) }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

vdecl:
  dtype ID EOL { ($1, $2) }

bind:
  dtype ID { ($1, $2) }

params_list:
    bind                   { [$1] }
  | bind COMMA params_list { $1::$3 }

opt_params_list:
    NONE        { [] }
  | params_list { $1 }

func_rtype:
    dtype { $1 }
  | NONE  { None }

fdecl:
  DEFINE ID LPAREN opt_params_list GIVES func_rtype RPAREN COLON EOL INDENT stmt_list DEDENT
  {
    {
      fname=$2;
      params=$4;
      rtype=$6;
      body=$11
    }
  }

fcall:
  ID LPAREN expr_list_opt RPAREN { Call ($1, $3) }

expr:
    atom                      { $1 }
  | LSQUARE atom_list RSQUARE { ArrayLit $2 }
  | ID                        { Id $1 }
  | expr LSQUARE expr RSQUARE { Elem ($1, $3) }
  | expr PLUS expr            { Binop ($1, Plus, $3) }
  | expr MINUS expr           { Binop ($1, Minus, $3) }
  | expr TIMES expr           { Binop ($1, Times, $3) }
  | expr INTDIV expr          { Binop ($1, IntDiv, $3) }
  | expr DIV expr             { Binop ($1, Div, $3) }
  | expr MOD expr             { Binop ($1, Mod, $3) }
  | MINUS expr %prec UMINUS   { Unop (Neg, $2) }
  | PIPE expr PIPE            { Unop (Abs, $2) }
  | expr EQ expr              { Binop ($1, Eq, $3) }
  | expr NEQ expr             { Binop ($1, Neq, $3) }
  | expr LT expr              { Binop ($1, Less, $3) }
  | expr LEQ expr             { Binop ($1, Leq, $3) }
  | expr GT expr              { Binop ($1, Greater, $3) }
  | expr GEQ expr             { Binop ($1, Geq, $3) }
  | expr AND expr             { Binop ($1, And, $3) }
  | expr OR expr              { Binop ($1, Or, $3) }
  | NOT expr                  { Unop (Not, $2) }
  | LPAREN expr RPAREN        { $2 }
  | fcall                     { $1 }

atom:
    NUMBERLIT { NumberLit $1 }
  | BOOLLIT   { BoolLit $1 }
  | CHARLIT   { CharLit $1 }
  | STRINGLIT { StringLit $1 }

atom_list:
    atom                 { [$1] }
  | atom COMMA atom_list { $1::$3 }

expr_list_opt:
    /* nothing */ { [] }
  | expr_list     { $1 }

expr_list:
    expr                 { [$1] }
  | expr COMMA expr_list { $1::$3 }

stmt_list:
    /* nothing */  { [] }
  | stmt stmt_list { $1::$2 }

stmt:
    dtype ID ASSIGN expr EOL                                                         { Assign ($1, $2, $4) } 
  | ID ASSIGN expr EOL                                                               { InferAssign (Id $1, $3) }
  | ID LSQUARE expr RSQUARE ASSIGN expr EOL                                          { InferAssign (Elem (Id $1, $3), $6) }
  | atomic_dtype LSQUARE expr RSQUARE ID EOL                                         { Alloc ($1, $3, $5) }
  | IF expr COLON EOL INDENT stmt_list DEDENT                                        { If ($2, $6, []) }
  | IF expr COLON EOL INDENT stmt_list DEDENT ELSE COLON EOL INDENT stmt_list DEDENT { If ($2, $6, $12) }
  | LOOP expr COLON EOL INDENT stmt_list DEDENT                                      { CondLoop ($2, $6) }
  | RETURN expr EOL                                                                  { Return $2 }
  | CONTINUE EOL                                                                     { Continue }
  | STOP EOL                                                                         { Stop }
  | fcall                                                                            { $1 }

atomic_dtype:
    NUMBER { Number }
  | BOOL   { Bool }
  | CHAR   { Char }
  | STRING { String }

dtype:
    atomic_dtype                 { $1 }
  | atomic_dtype LSQUARE RSQUARE { Array $1 }
