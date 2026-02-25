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
%token VAR
%token PROC
%token ECHO
%token SET
%token IFS
%token WHILE
%token CALL
%token IF
%token AND
%token OR
%token BOOL
%token INT

%token <int> NUM
%token <string> IDENT


%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmds> prog

%start prog

%%
prog: block { $1 }

block: LBRA cmds RBRA    { $2 }
;

cmds:
  stat                  { ASTEnd $1 }
| def SEMICOLON cmds    { ASTDef($1, $3) }
| stat SEMICOLON cmds   { ASTStat($1, $3) }
;

stat:
  ECHO expr             { ASTEcho($2) }
  | SET IDENT expr       { ASTSet($2, $3) }
  | IFS expr block block         { ASTIfS($2, $3, $4) } 
  | WHILE expr block              { ASTWhile($2, $3) }
  | CALL IDENT exprs              { ASTCall($2, $3) }
;

def:
  CONST IDENT typ expr  { ASTConst($2, $3, $4) }
| FUN IDENT typ LBRA args RBRA expr  { ASTFun($2, $3, $5, $7) }
| FUN REC IDENT typ LBRA args RBRA expr { ASTFunRec($3, $4, $6, $8) }
| VAR IDENT typ { ASTVar($2, $3) }
| PROC IDENT LBRA args RBRA block { ASTProc($2, $4, $6) }
| PROC REC IDENT LBRA args RBRA block { ASTProcRec($3, $5, $7) }
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

