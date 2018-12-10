/* Ocamlyacc parser for MicroC */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LBRACK RBRACK GRAPS GRAPE SQUOT DQUOT UNDS
%token PLUS MINUS TIMES EXP DIVIDE ASSIGN NOT MOD AMP 
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE EACH WHILE FOR FUN 
%token INT NODE EDGE GRAPH STR BOOL LIST DICT
%token <int> INT_LIT
%token <string> FLOAT_LIT
%token <string> STR_LIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left AMP
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left EXP 
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

/* DO ALL Variable Declarations have to come before all STATEMENTS? */

fdecl:
   FUN typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $2;
	 fname = $3;
	 formals = $5;
	 locals = List.rev $8;
	 body = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                      { [($1,$2)] }
  | formal_list COMMA typ ID    { ($3,$4) :: $1 }

typ:
    INT                         { Int }
  | STR                         { Str }
  | NODE LT typ GT              { Node($3) }
  | EDGE LT typ GT              { Edge($3) }
  | GRAPH LT typ COMMA typ GT   { Graph(Node($3), Edge($5)) }
  | BOOL                        { Bool }
  | LIST LT typ GT              { List($3) }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI    { ($1, $2) }

stmt_list:
    stmt           { [$1] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1 }
  | RETURN expr_opt SEMI                    { Return $2} 
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | EACH LPAREN expr RPAREN stmt            { Each($3, $5) }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5) }
  | typ ID ASSIGN expr SEMI                 { DecAsn($1 , $2,  Assign($2, $4))}

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 } 

expr:
    INT_LIT                 { IntLit($1) }
  | FLOAT_LIT               { FloatLit($1) }
  | STR_LIT                 { StrLit($1) }
  | TRUE                    { BoolLit(true) }
  | FALSE                   { BoolLit(false) }
  | ID                      { Id($1) }
  | GRAPS graph_opt GRAPE   { GraphLit($2) }
  | expr PLUS   expr        { Binop($1, Add,   $3) }
  | expr MINUS  expr        { Binop($1, Sub,   $3) }
  | expr TIMES  expr        { Binop($1, Mult,  $3) }
  | expr DIVIDE expr        { Binop($1, Div,   $3) }
  | expr EQ     expr        { Binop($1, Equal, $3) }
  | expr NEQ    expr        { Binop($1, Neq,   $3) }
  | expr LT     expr        { Binop($1, Less,  $3) }
  | expr LEQ    expr        { Binop($1, Leq,   $3) }
  | expr GT     expr        { Binop($1, Greater, $3) }
  | expr GEQ    expr        { Binop($1, Geq,   $3) }
  | expr AND    expr        { Binop($1, And,   $3) }
  | expr OR     expr        { Binop($1, Or,    $3) }
  | expr EXP    expr        { Binop($1, Exp,    $3)}
  | expr MOD    expr        { Binop($1, Mod,    $3)} 
  | expr AMP    expr        { Binop($1, Amp,    $3)} 
  | MINUS expr %prec NEG    { Unop(Neg, $2) }
  | NOT expr                { Unop(Not, $2) }
  /*| typ ID ASSIGN expr       { Assign(snd $1, $4)} */
  | ID ASSIGN expr          { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN      { $2 }
  | LBRACK actuals_opt RBRACK { ListLit($2) }
  | edgeExpr { $1 }  
  | nodeExpr { $1 } 

edgeExpr:
    UNDS expr UNDS GT { DirEdgeLit($2)}        /* Directed Edge */ 
  | UNDS expr UNDS LT {EdgeLit($2) }
   
nodeExpr: 
    SQUOT expr SQUOT       { NodeLit($2) }         /* Node */

actuals_opt:
    /* nothing */ { [] }
  | actuals_list { $1 }

/* comma-separated list */
actuals_list:
    expr { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

/* graph list */
graph_opt: 
  /* nothing */ { [] }
  | graph_list { List.rev $1 }

// The following block means that graph_list can be all nodes
// as path_list can be only a single node 
graph_list:
    path_list { [List.append (List.rev (List.tl (List.rev $1))) ([List.hd (List.rev $1)])] }
  | graph_list COMMA path_list { (List.append (List.rev (List.tl (List.rev $3))) ([List.hd (List.rev $3)])) :: $1 } 

graph_vertex:
    nodeExpr { $1 }
  | ID { Id($1) }

path_list:
    graph_vertex { [($1, Noexpr)] }
  | graph_vertex edgeExpr path_list {($1, $2) :: $3} 
