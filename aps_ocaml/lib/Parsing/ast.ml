
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)



type typ = 
    ASTBool
  | ASTInt
  | ASTVec of typ
  | ASTTyps of typ list * typ


type arg =
  ASTArg of string * typ

type argp = 
    ASTArgp of string * typ
  | ASTVarArgp of string * typ

type expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr *expr
  | ASTApp of expr * expr list
  | ASTAbs of arg list * expr
  | ASTAlloc of expr
  | ASTLen of expr
  | ASTNthE of expr * expr
  | ASTVset of expr * expr * expr

type exprp =
    ASTAdr of string
  | ASTExpr of expr


type lval =
    ASTLId of string
  | ASTNth of lval * expr


type stat =
    ASTEcho of expr
  | ASTSet of lval * expr
  | ASTIfS of expr * cmds * cmds
  | ASTWhile of expr * cmds
  | ASTCall of string * exprp list

and def =
    ASTConst of string * typ * expr
  | ASTFun of string * typ * arg list * expr
  | ASTFunRec of string * typ * arg list * expr
  | ASTVar of string * typ
  | ASTProc of string * argp list * cmds
  | ASTProcRec of string * argp list * cmds

and cmds =
    ASTEnd of stat
  | ASTDef of def * cmds
  | ASTStat of stat * cmds
