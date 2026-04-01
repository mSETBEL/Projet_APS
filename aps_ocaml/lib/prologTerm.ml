(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast
open Format

let sep_cma fmt () = fprintf fmt ", "

let pp_lst_cma p = pp_print_list ~pp_sep:sep_cma p

let rec pp_expr fmt e =
  match e with
    | ASTNum n -> fprintf fmt "num(%d)" n
    | ASTId x -> fprintf fmt "ident(%s)" x
    | ASTIf(e1, e2, e3) -> fprintf fmt "if(%a,%a,%a)" pp_expr e1 pp_expr e2 pp_expr e3
    | ASTAnd(e1, e2) -> fprintf fmt "and(%a,%a)" pp_expr e1 pp_expr e2
    | ASTOr(e1, e2) -> fprintf fmt "or(%a,%a)" pp_expr e1 pp_expr e2
    | ASTApp(e, es) -> fprintf fmt "app(%a,[%a])" pp_expr  e  pp_exprs es
    | ASTAbs(args, e) -> fprintf fmt "abs([%a],%a)" pp_args args pp_expr e

and pp_exprs fmt es = pp_lst_cma pp_expr fmt es

and pp_arg fmt (ASTArg(name, ty)) =
  fprintf fmt "arg(%s,%a)" name pp_type ty

and pp_args fmt args = pp_lst_cma pp_arg fmt args

and pp_type fmt t =
  match t with
    ASTBool -> fprintf fmt "bool"
  | ASTInt -> fprintf fmt "int"
  | ASTTyps(tys, ty) -> fprintf fmt "types([%a],%a)" pp_types tys pp_type ty

and pp_types fmt tys = pp_lst_cma pp_type fmt tys


let rec pp_cmds fmt c =
  match c with
  ASTEnd s -> fprintf fmt "end(%a)" pp_stat s
  |ASTDef(d, cs) -> fprintf fmt "dec(%a,%a)" pp_def d pp_cmds cs
  |ASTStat(s, cs) -> fprintf fmt "stat(%a,%a)" pp_stat s pp_cmds cs

and pp_stat fmt s =
  match s with
  ASTEcho e -> fprintf fmt "echo(%a)" pp_expr e
  | ASTSet(x, e) -> fprintf fmt "set(%s,%a)" x pp_expr e
  | ASTIfS(e, bk1, bk2) -> fprintf fmt "ifS(%a,block(%a),block(%a))" pp_expr e pp_cmds bk1 pp_cmds bk2
  | ASTWhile(e, bk) -> fprintf fmt "while(%a,block(%a))" pp_expr e pp_cmds bk
  | ASTCall(x, es) -> fprintf fmt "call(%s,[%a])" x pp_expars es

and pp_expar fmt e =
  match e with
  ASTAdr x -> fprintf fmt "adr(%s)" x
  | ASTExpr e -> pp_expr fmt e

and pp_expars fmt es = pp_lst_cma pp_expar fmt es

and pp_def fmt s =
  match s with
  ASTConst (name, ty, e) ->
    fprintf fmt "const(%s,%a,%a)" name pp_type ty pp_expr e
  | ASTFun (name, ty, args, e) ->
    fprintf fmt "fun(%s,%a,[%a],%a)" name pp_type ty pp_args args pp_expr e
  | ASTFunRec (name, ty, args, e) ->
    fprintf fmt "funrec(%s,%a,[%a],%a)" name pp_type ty pp_args args pp_expr e
  |  ASTVar (name, ty) ->
    fprintf fmt "var(%s,%a)" name pp_type ty
  | ASTProc (name, args, bk) -> 
    fprintf fmt "proc(%s,[%a],block(%a))" name pp_argsp args pp_cmds bk
  | ASTProcRec (name, args, bk) ->
    fprintf fmt "procrec(%s,[%a],block(%a))" name pp_argsp args pp_cmds bk

and pp_argp fmt a =
  match a with
    ASTArgp(name, ty) ->
      fprintf fmt "arg(%s,%a)" name pp_type ty
    | ASTVarArgp(name, ty) ->
      fprintf fmt "(var(%s),%a)" name pp_type ty

and pp_argsp fmt args = pp_lst_cma pp_argp fmt args

let pp_prog fmt p =
  fprintf fmt "prog(block(%a)).\n" pp_cmds p




