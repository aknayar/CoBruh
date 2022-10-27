%{
  open Ast
%}

%token TAB
%token LPAREN RPAREN LBRACE RBRACE PERIOD COMMA COLON PIPE
%token IS PLUS MINUS TIMES INTDIV DIV MOD EQ NEQ LT LEQ GT GEQ AND OR NOT
%token IF ELSE LOOP IN TO BY
%token CALL DEFINE GIVES RETURN
%token NUMBER BOOL CHAR STRING LIST
%token NONE
%token SAY
%token EOF
%token <float> NUMBERLIT
%token <bool> BOOLLIT
%token <char> CHARLIT
%token <string> STRINGLIT
%token <string> ID

%start program_rule
%type <Ast.program> program_rule

%nonassoc IS
%left OR
%left AND
%nonassoc NOT
%left EQ NEQ
%nonassoc LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES INTDIV DIV MOD

%%

program_rule:
  | all_cmds_rule EOF { $1 }

all_cmds_rule:
  | /* nothing */          { [] }
  | cmd_rule all_cmds_rule { $1::$2 }

cmd_rule:
  | expr_rule PERIOD { Expr $1 }
  | stmt_rule PERIOD { Stmt $1 }

expr_rule:
  | NUMBERLIT                  { NumberLit $1 }
  | BOOLLIT                    { BoolLit $1 }
  | STRINGLIT                  { StringLit $1 }
  | CHARLIT                    { CharLit $1 }
  | ID                         { Id $1 }
  | expr_rule PLUS expr_rule   { Binop ($1, Plus, $3) }
  | expr_rule MINUS expr_rule  { Binop ($1, Minus, $3) }
  | expr_rule TIMES expr_rule  { Binop ($1, Times, $3) }
  | expr_rule INTDIV expr_rule { Binop ($1, IntDiv, $3) }
  | expr_rule DIV expr_rule    { Binop ($1, Div, $3) }
  | expr_rule LT expr_rule     { Binop ($1, Less, $3) }
  | expr_rule LEQ expr_rule    { Binop ($1, Leq, $3) }
  | expr_rule GT expr_rule     { Binop ($1, Greater, $3) }
  | expr_rule GEQ expr_rule    { Binop ($1, Geq, $3) }
  | expr_rule AND expr_rule    { Binop ($1, And, $3) }
  | expr_rule OR expr_rule     { Binop ($1, Or, $3) }
  | LPAREN expr_rule RPAREN    { $2 }

stmt_rule:
  | ID IS expr_rule { Asn ($1, $3) }
  | SAY expr_rule   { Say $2 }
  
