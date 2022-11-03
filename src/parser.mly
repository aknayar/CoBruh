%{
  open Ast
%}

%token TAB
%token LPAREN RPAREN LCURLY RCURLY LSQUARE RSQUARE PERIOD COMMA COLON PIPE 
%token PLUS MINUS TIMES INTDIV DIV MOD EQ NEQ LT LEQ GT GEQ AND OR NOT ASSIGN
%token IF ELSE LOOP IN TO BY
%token CALL DEFINE GIVES RETURN
%token NUMBER BOOL CHAR STRING LIST
%token NONE
%token USE
%token EOF
%token <float> NUMBERLIT
%token <bool> BOOLLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token <string> ID

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%right NOT
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES INTDIV DIV MOD

%%

program:
  decls EOF { $1 }

/* tuple of (stmts, funcs) */
decls:
   /* nothing */ { ([], []) }
 | stmt decls    { (($1 :: fst $2), snd $2) }
 | fdecl decls   { (fst $2, ($1 :: snd $2)) }

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
  DEFINE ID LPAREN opt_params_list GIVES func_rtype RPAREN LCURLY func_stmt_list RCURLY
  {
    {
      fname=$2;
      params=$4;
      rtype=$6;
      body=$9
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
  | expr LT expr              { Binop ($1, Less, $3) }
  | expr LEQ expr             { Binop ($1, Leq, $3) }
  | expr GT expr              { Binop ($1, Greater, $3) }
  | expr GEQ expr             { Binop ($1, Geq, $3) }
  | expr AND expr             { Binop ($1, And, $3) }
  | expr OR expr              { Binop ($1, Or, $3) }
  | LPAREN expr RPAREN        { $2 }
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }

args_opt:
    /*nothing*/ { [] }
  | args        { $1 }

args:
    expr            { [$1] }
  | expr COMMA args { $1::$3 }

stmt_list:
    /* nothing */  { [] }
  | stmt stmt_list { $1::$2 }

stmt:
    expr PERIOD                                                  { Expr $1 }
  | opt_dtype ID ASSIGN expr PERIOD                              { Assign ($1, $2, $4) }
  | IF expr LCURLY stmt_list RCURLY ELSE LCURLY stmt_list RCURLY { If ($2, $4, $8) }
  | LOOP ID IN expr TO expr loop_by LCURLY stmt_list RCURLY      { Loop ($2, $4, $6, $7, $9) }

loop_by:
    /* nothing */ { NumberLit 1. }
  | BY expr       { $2 }

func_stmt_list:
    /* nothing */            { [] } /* empty function body */
  | func_stmt func_stmt_list { $1::$2 }

/* allows functions to use return keyword */
func_stmt:
    stmt               { Stmt $1 }
  | RETURN expr PERIOD { Return $2 }

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

/* allows for binding and rebinding: number x is 2 vs. x is 3 */
opt_dtype:
    /* nothing */ { Rebind }
  | dtype         { BindType $1 }