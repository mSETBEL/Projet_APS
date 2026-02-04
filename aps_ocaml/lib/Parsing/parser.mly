%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}

%token LPAR RPAR 
%token LBRA RBRA
%token SEMICOLON
%token COLON
%token COMMA
%token STAR
%token ARROW

%token CONST
%token FUN
%token REC
%token ECHO
%token IF
%token AND
%token OR
%token BOOL
%token INT

%token <int> NUM
%token <string> IDENT


%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog

%start prog

%%
prog: LBRA cmds RBRA    { $2 }
;

cmds:
  stat                  { [ASTStat $1] }
| def SEMICOLON cmds    { [ASTDef($1, $3)] }
;

stat:
  ECHO expr             { ASTEcho($2) }
;

def:
  CONST IDENT typ expr  { ASTConst($2, $3, $4) }
| FUN IDENT typ LBRA args RBRA expr  { ASTFun($2, $3, $5, $7) }
| FUN REC IDENT typ LBRA args RBRA expr { ASTFunRec($3, $4, $6, $8) }
;
typ:
  BOOL { ASTBool }
| INT  { ASTInt }
| LPAR typs ARROW typ RPAR  { ASTTyps ($2, $4) }

typs:
  typ       { [$1] }
| typ STAR typs { $1::$3 }


expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR	{ ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR	{ ASTAnd($3, $4) }
| LPAR OR expr expr RPAR	{ ASTOr($3, $4) }
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
| LBRA args RBRA expr	{ ASTAbs($2, $4) }

;
arg:
	IDENT COLON typ	{ ASTArg($1, $3) }

;
	
args :
  arg        { [$1] }
| arg COMMA args { $1::$3 }
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

