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
| def SEMICOLON cmds    { [ASTDefs($1, $3)] }
;

stat:
  ECHO expr             { ASTEcho($2) }
;

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
	
	
args:
  arg		{ [$1] }
  arg args		{ $1::$2 }

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

