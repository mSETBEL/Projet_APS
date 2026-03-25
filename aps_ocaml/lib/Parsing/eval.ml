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
  | A of int
  | P of proClosure
  | PR of recProClosure
and closure = expr * string list * env
and recClosure = expr * string * string list * env
and proClosure = cmds * string list * env 
and recProClosure = cmds * string * string list * env
and env = (string * value) list

and espace = None | Some of value | Any
and mem = (int * espace) list

type outFlux = int list


let alloc mem =
  let new_addr = List.length mem in
  (new_addr, (new_addr, Any) :: mem)

let update mem a v =
  List.map (fun (addr, val') -> if addr = a then (addr, Some v) else (addr, val')) mem

let inDomain mem a =
  List.exists (fun (addr, _) -> addr == a) mem

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


let find mem a =
  match List.find_opt (fun (addr, _) -> addr = a) mem with
  | Some (_, Some v) -> v
  | _ -> failwith "Memory access error: address not found or uninitialized"

(* expressions *)
let rec eval_expr env mem exp =
  match exp with

  | ASTNum n -> Z n 

  | ASTId x -> (match get_value env x with
      | A a -> (find mem a)
      | v -> v)
  

  | ASTApp (ASTId "true", []) -> Z 1
  | ASTApp (ASTId "false", []) -> Z 0
  
  | ASTApp (ASTId "not", [e]) -> (match eval_expr env mem e with
      | Z n -> Z (pi1("not" , n)) 
      | _ -> failwith "Expected an integer for not operator"
    )
  | ASTApp (ASTId x, [e1; e2]) when (x = "eq" || x = "lt" || x = "add" || x = "sub" ||  x = "mul" ||  x = "div") -> 
      (match (eval_expr env mem e1, eval_expr env mem e2) with
      | (Z n1, Z n2) -> Z (pi2(x, n1, n2)) 
      | _ -> failwith ("Expected integers for "^x^" operator")
    )
  | ASTAnd (e1, e2) -> (match eval_expr env mem e1 with
      | Z 1 -> eval_expr env mem e2
      | Z 0 -> Z 0
      | _ -> failwith "Expected boolean values for and operator")
  | ASTOr (e1, e2) -> (match eval_expr env mem e1 with
      | Z 1 -> Z 1
      | Z 0 -> eval_expr env mem e2
      | _ -> failwith "Expected boolean values for or operator")
  | ASTIf (e1, e2, e3) -> (match eval_expr env mem e1 with
      | Z 1 -> eval_expr env mem e2
      | Z 0 -> eval_expr env mem e3
      | _ -> failwith "Expected a boolean value for if condition")
  | ASTAbs (args, e) -> F (e, build_types args, env)

  | ASTApp (e, exprs) -> (match eval_expr env mem e with
      | F (e', args, env') -> let vals = eval_exprs env mem exprs in
              let new_env = ajout_list_env env' args vals in
              eval_expr new_env mem e' 
      | FR (e', x, args, env') ->  let vals = eval_exprs env mem exprs in
              let new_env = ajout_list_env ((x, FR (e', x, args, env')) :: env') args vals in
              eval_expr new_env mem e'
      | _ -> failwith "Expected a function in application")

and eval_exprs env mem exprs =
  match exprs with
  | [] -> []
  | e :: rest -> eval_expr env mem e :: eval_exprs env mem rest

(* definitions *)
let eval_def env mem def =
  match def with
  | ASTConst (x, _, e) -> let v = eval_expr env mem e in
      ((x, v) :: env , mem)
  | ASTFun (x, _, args, e) -> let f = F (e, build_types args, env) in
      ((x, f) :: env , mem)
  | ASTFunRec (x, _, args, e) -> let fr = FR (e, x, build_types args, env) in
      ((x, fr) :: env , mem)
  | ASTVar (x,_) -> let (a, mem') = alloc mem in
    (match (inDomain mem a) with
    | true -> failwith "Address already in use"
    | false -> ((x, A a) :: env , mem'))
    
    
  | ASTProc (x, args, bk) -> let p = P (bk, build_types args, env) in
      ((x, p) :: env , mem)
  | ASTProcRec (x, args, bk) -> let pr = PR (bk, x, build_types args, env) in
      ((x, pr) :: env , mem)
  
(* instruction *)
let rec eval_stat env mem outFlux s =
  match s with 
  | ASTEcho e -> let x= eval_expr env mem e in (match x with 
      | Z n -> (mem, n :: outFlux)
      | _ -> failwith "Expected an integer in echo statement")
  | ASTSet (x, e) -> (match get_value env x with
      | A a -> let v = eval_expr env mem e in
          (update mem a v, outFlux)
      | _ -> failwith ("Expected a variable in set statement for "^x))
  | ASTIfS (e, bk1, bk2) -> (match eval_expr env mem e with
      | Z 1 -> eval_block env mem outFlux bk1
      | Z 0 -> eval_block env mem outFlux bk2
      | _ -> failwith "Expected a boolean value for if condition")

  | ASTWhile (e, bk) -> (match eval_expr env mem e with
      | Z 1 -> let (mem', outFlux') = eval_block env mem outFlux bk in
          eval_stat env mem' outFlux' (ASTWhile (e, bk))
      | Z 0 -> (mem, outFlux)
      | _ -> failwith "Expected a boolean value for while condition")
  
  | ASTCall (x, exprs) -> (match get_value env x with
      | P (bk, args, env') -> let vals = eval_exprs env mem exprs in
              let new_env = ajout_list_env env' args vals in
              eval_block new_env mem outFlux bk
      | PR (bk, x, args, env') -> let vals = eval_exprs env mem exprs in
              let new_env = ajout_list_env ((x, PR (bk, x, args, env')) :: env') args vals in
              eval_block new_env mem outFlux bk
      | _ -> failwith "Expected a procedure in call statement")
          


(* commandes *)
and eval_cmds env mem outFlux cmds =
  match cmds with
  | ASTStat (s, c) -> let (mem', outFlux') = eval_stat env mem outFlux s in eval_cmds env mem' outFlux' c
  | ASTDef (d, c) -> let (env', mem') = eval_def env mem d in eval_cmds env' mem' outFlux c
  | ASTEnd s ->  let (a, finalOutFlux) = eval_stat env mem outFlux s in (a , finalOutFlux) 


(*block*)
and eval_block env mem outFlux cmds =
  eval_cmds env mem outFlux cmds


(* programmes *)
let eval_prog cmds =
  let (mem, outFlux) = eval_block [] [] [] cmds in (mem, List.rev outFlux)


