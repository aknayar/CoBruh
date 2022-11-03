%{
  open Ast
%}

%token TAB
%token LPAREN RPAREN LBRACE RBRACE PERIOD COMMA COLON PIPE ASSIGN
%token PLUS MINUS TIMES INTDIV DIV MOD EQ NEQ LT LEQ GT GEQ AND OR NOT
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

%nonassoc IS
%left OR
%left AND
%nonassoc NOT
%left EQ NEQ
%nonassoc LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES INTDIV DIV MOD

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | vdecl PERIOD decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl PERIOD vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  dtype ID { ($1, $2) }


/* fdecl */
fdecl:
  DEFINE ID LPAREN formals_list GIVES dtype RPAREN COLON LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=$6;
      fname=$2;
      formals=$4;
      locals=$10;
      body=$11
    }
  }

/* not optional, either none or exists */
formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

expr:
  | NUMBERLIT                 { NumberLit $1 }
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
  | ID ASSIGN expr            { Assign ($1, $3) }
  | LPAREN expr RPAREN        { $2 }
  | ID LPAREN args_opt RPAREN               { Call ($1, $3)  }

// mexpr:
//   mexpr PLUS term 
// | mexpr MINUS term
// | term

// term:
//   term TIMES atom
// | term DIV atom
// | term INTDIV atom
// | atom

// atom: 
//   LITERAL
// | ID

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr args { $1::$2 }


stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
  | LBRACE stmt_list RBRACE                       { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | dtype ID IS expr                              { Decl ($1, $2, $4) }
  | IF expr COLON stmt ELSE  COLON stmt           { If ($2, $4, $7) }
  | LOOP ID IN expr TO expr COLON stmt            { Loop ($2, $4, $6, NumberLit(1.), $8) }
  | LOOP ID IN expr TO expr BY expr COLON stmt    { Loop ($2, $4, $6, $8, $10) }
  /* return */
  | RETURN expr PERIOD                            { Return $2 }
  
dtype:
  | NUMBER     { Number }
  | BOOL       { Bool }
  | CHAR       { Char }
  | STRING     { String }
  | dtype LIST { List $1 }
  