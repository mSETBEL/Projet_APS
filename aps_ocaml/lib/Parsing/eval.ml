open Ast

(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: eval.ml                                                     == *)
(* ==  Evaluation de l'arbre de syntaxe abstraite                          == *)
(* ========================================================================== *)

type value = 
    Z of int
  | F of closure
  | FR of recClosure
and closure = expr * string list * env
and recClosure = expr * string * string list * env 
and env = (string * value) list

type outFlux = int list


let pi0 op =
  match op with
    "true" -> 1
  | "false" -> 0
  | _ -> failwith "Error in nullary operator"

let pi1 (op, x) =
  match (op, x) with
    ("not", 1) -> 0
    |("not", 0) -> 1
    | _ -> failwith "Error in unuary operator"

let pi2 (op, x, y) =
  match (op, x, y) with
    ("eq", n1, n2) -> if n1==n2 then 1 else 0
    |("lt", n1, n2) -> if n1 < n2 then 1 else 0
    |("add", n1, n2) -> n1 + n2
    |("sub", n1, n2) -> n1 - n2
    |("mul", n1, n2) -> n1 * n2
    |("div", n1, n2) -> n1 / n2
    | _ -> failwith "Error in binary operator"


let rec get_value env x =
  match env with 
  | [] -> failwith ("Variable "^x^" not found")
  | (y, v) :: rest -> if x = y then v else get_value rest x


let rec build_types args =
  match args with
  | [] -> []
  | (ASTArg (x, _)) :: rest -> x :: build_types rest


let rec ajout_list_env env args vals =
  match (args, vals) with
  | ([], []) -> env
  | (x::rest_x, v::rest_v) -> ajout_list_env ((x, v) :: env) rest_x rest_v
  | _ -> failwith "Mismatch between arguments and values"



(* expressions *)
let rec eval_expr env exp =
  match exp with

  | ASTNum n -> Z n 
  | ASTId x -> get_value env x

  | ASTApp (ASTId "true", []) -> Z 1
  | ASTApp (ASTId "false", []) -> Z 0
  
  | ASTApp (ASTId "not", [e]) -> (match eval_expr env e with
      | Z n -> Z (pi1("not" , n)) 
      | _ -> failwith "Expected an integer for not operator"
    )
  | ASTApp (ASTId x, [e1; e2]) when (x = "eq" || x = "lt" || x = "add" || x = "sub" ||  x = "mul" ||  x = "div") -> 
      (match (eval_expr env e1, eval_expr env e2) with
      | (Z n1, Z n2) -> Z (pi2(x, n1, n2)) 
      | _ -> failwith ("Expected integers for "^x^" operator")
    )
  | ASTAnd (e1, e2) -> (match eval_expr env e1 with
      | Z 1 -> eval_expr env e2
      | Z 0 -> Z 0
      | _ -> failwith "Expected boolean values for and operator")
  | ASTOr (e1, e2) -> (match eval_expr env e1 with
      | Z 1 -> Z 1
      | Z 0 -> eval_expr env e2
      | _ -> failwith "Expected boolean values for or operator")
  | ASTIf (e1, e2, e3) -> (match eval_expr env e1 with
      | Z 1 -> eval_expr env e2
      | Z 0 -> eval_expr env e3
      | _ -> failwith "Expected a boolean value for if condition")
  | ASTAbs (args, e) -> F (e, build_types args, env)
  | ASTApp (e, exprs) -> (match eval_expr env e with
      | F (e', args, env') -> let vals = eval_exprs env exprs in
              let new_env = ajout_list_env env' args vals in
              eval_expr new_env e' 
      | FR (e', x, args, env') ->  let vals = eval_exprs env exprs in
              let new_env = ajout_list_env ((x, FR (e', x, args, env')) :: env') args vals in
              eval_expr new_env e'
      | _ -> failwith "Expected a function in application")

and eval_exprs env exprs =
  match exprs with
  | [] -> []
  | e :: rest -> eval_expr env e :: eval_exprs env rest

(* definitions *)
let eval_def env def =
  match def with
  | ASTConst (x, _, e) -> let v = eval_expr env e in
      (x, v) :: env
  | ASTFun (x, _, args, e) -> let f = F (e, build_types args, env) in
      (x, f) :: env
  | ASTFunRec (x, _, args, e) -> let fr = FR (e, x, build_types args, env) in
      (x, fr) :: env


(* instruction *)
let eval_stat env outFlux s =
  match s with 
  | ASTEcho e -> let x= eval_expr env e in match x with 
      | Z n -> n :: outFlux
      | _ -> failwith "Expected an integer in echo statement"



(* commandes *)
let rec eval_cmds env outFlux cmds =
  match cmds with
  | ASTStat s -> let finalOutFlux = eval_stat env outFlux s in List.rev finalOutFlux (* on l'inverse à la fin*)
  | ASTDef (d, c) -> let new_env = eval_def env d in eval_cmds new_env outFlux c
 





(* programmes *)
let eval_prog cmds =
  eval_cmds [] [] cmds
