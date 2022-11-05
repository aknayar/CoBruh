%{
  open Ast
%}

%token LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE PERIOD COMMA COLON PIPE 
%token ASSIGN PLUS MINUS TIMES INTDIV DIV MOD EQ NEQ LT LEQ GT GEQ AND OR NOT
%token IF ELSE LOOP IN TO BY
%token CALL DEFINE NONE GIVES RETURN
%token NUMBER BOOL CHAR STRING LIST 
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

%%

program:
  decls EOF { $1 }

/* list of stmts and funcs */
decls:
   /* nothing */ { [] }
 | stmt decls    { (Stmt $1)::$2 }
 | fdecl decls   { (Func $1)::$2 }

bind:
  dtype ID { ($1, $2) }

/* functions with nonempty parameters */
params_list:
    bind                   { [$1] }
  | bind COMMA params_list { $1::$3 }

opt_params_list:
    NONE        { [] }
  | params_list { $1 }

/* define foo(number bar -> string) */
fdecl:
  DEFINE ID LPAREN opt_params_list GIVES func_rtype RPAREN COLON LCURLY stmt_list RCURLY
  {
    {
      fname=$2;
      params=$4;
      rtype=$6;
      body=$10
    }
  }

expr:
    NUMBERLIT                 { NumberLit $1 }
  | BOOLLIT                   { BoolLit $1 }
  | STRINGLIT                 { StringLit $1 }
  | CHARLIT                   { CharLit $1 }
  | ID                        { Id $1 }
  | expr PLUS expr            { Binop ($1, Plus, $3) }
  | expr MINUS expr           { Binop ($1, Minus, $3) }
  | expr TIMES expr           { Binop ($1, Times, $3) }
  | expr INTDIV expr          { Binop ($1, IntDiv, $3) }
  | expr DIV expr             { Binop ($1, Div, $3) }
  | MINUS expr %prec UMINUS   { Unop (Neg, $2) }
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
  | ID LPAREN args_opt RPAREN { Call ($1, $3) }
  | LSQUARE elems_opt RSQUARE { ListLit ($2) } 

args_opt:
    /*nothing*/ { [] }
  | args        { $1 }

args:
    expr            { [$1] }
  | expr COMMA args { $1::$3 }

elems_opt:
    /*nothing*/  { [] }
  | elems        { $1 }

elems:
    expr             {[$1]}
  | expr COMMA elems { $1::$3 }

stmt_list:
    /* nothing */  { [] }
  | stmt stmt_list { $1::$2 }

stmt:
    expr PERIOD                                                              { Expr $1 }
  | dtype ID ASSIGN expr PERIOD                                              { Assign ($1, $2, $4) }
  | ID ASSIGN expr PERIOD                                                    { Reassign ($1, $3) }
  | IF expr COLON LCURLY stmt_list RCURLY ELSE COLON LCURLY stmt_list RCURLY { If ($2, $5, $10) }
  | LOOP ID IN expr TO expr loop_by COLON LCURLY stmt_list RCURLY            { IterLoop ($2, $4, $6, $7, $10) }
  | LOOP expr COLON LCURLY stmt_list RCURLY                                  { CondLoop ($2, $5) }
  | RETURN expr PERIOD                                                       { Return $2 }

loop_by:
    /* nothing */ { NumberLit 1. }
  | BY expr       { $2 }

dtype:
    NUMBER     { Number }
  | BOOL       { Bool }
  | CHAR       { Char }
  | STRING     { String }
  | dtype LIST { List $1 }

/* allows functions to return none */
func_rtype:
    dtype { DType $1 }
  | NONE  { None }
