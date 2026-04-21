(*open Ast

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
  | B of int * int
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

let allocn mem n =
  let new_addr = List.length mem in
  (new_addr, List.init n (fun i -> (new_addr + i, Any)) @ mem)

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


let rec build_vars argsp =
  match argsp with
  | [] -> []
  | (ASTArgp (x, _)) :: rest -> x :: build_vars rest
  | (ASTVarArgp (x, _)) :: rest -> x :: build_vars rest

(* expressions *)
let rec eval_expr env mem exp =
  match exp with

  | ASTNum n -> (Z n, mem)

  | ASTId x -> ( match x with 
      |"true" -> (Z 1, mem)
      |"false" -> (Z 0, mem)
      | _ ->(match get_value env x with
        | A a -> (find mem a, mem)
        | v -> (v, mem)))
    

  | ASTApp (ASTId "true", []) -> (Z 1, mem)
  | ASTApp (ASTId "false", []) -> (Z 0, mem)
  
  | ASTApp (ASTId "not", [e]) -> (match eval_expr env mem e with
      | (Z n, mem') -> (Z (pi1("not", n)), mem')
      | _ -> failwith "Expected an integer for not operator"
    )
  | ASTApp (ASTId x, [e1; e2]) when (x = "eq" || x = "lt" || x = "add" || x = "sub" ||  x = "mul" ||  x = "div") -> 
      (match (eval_expr env mem e1, eval_expr env mem e2) with
      | ((Z n1, mem1), (Z n2, mem2)) when mem1 == mem2 -> (Z (pi2(x, n1, n2)), mem1)
      | _ -> failwith ("Expected integers for "^x^" operator")
    )
  | ASTAnd (e1, e2) -> (match eval_expr env mem e1 with
      | (Z 1, _ ) -> eval_expr env mem e2
      | (Z 0, mem') -> (Z 0, mem')
      | _ -> failwith "Expected boolean values for and operator")
  | ASTOr (e1, e2) -> (match eval_expr env mem e1 with
      | (Z 1, mem') -> (Z 1, mem')
      | (Z 0, _ ) -> eval_expr env mem e2
      | _ -> failwith "Expected boolean values for or operator")
  | ASTIf (e1, e2, e3) -> (match eval_expr env mem e1 with
      | (Z 1, _ ) -> eval_expr env mem e2
      | (Z 0, _ ) -> eval_expr env mem e3
      | _ -> failwith "Expected a boolean value for if condition")
  | ASTAbs (args, e) -> (F (e, build_types args, env), mem)

  | ASTApp (e, exprs) -> (match eval_expr env mem e with
      | (F (e', args, env'), _) -> let vals = eval_exprs env mem exprs in
              let new_env = ajout_list_env env' args vals in
              eval_expr new_env mem e' 
      | (FR (e', x, args, env'), _) ->  let vals = eval_exprs env mem exprs in
              let new_env = ajout_list_env ((x, FR (e', x, args, env')) :: env') args vals in
              eval_expr new_env mem e'
      | _ -> failwith "Expected a function in application")
  | ASTAlloc e -> (match eval_expr env mem e with
      |(Z n, mem')-> let (a, mem'') = allocn mem' n in
          (B (a, n), mem'')
      | _ -> failwith "Expected an integer in alloc expression") 
  | ASTVset (e1, e2, e3) -> (match eval_expr env mem e1 with
      |(B (a, n), mem1) -> (match eval_expr env mem1 e2 with
        |(Z i, mem2) -> let (v, mem3) = eval_expr env mem2 e3 in
           (B (a, n), update mem3 (a+i) v)
        | _ -> failwith "Expected an integer in vset expression")
      | _ -> failwith "Expected a vector in vset expression")
  | ASTNthE (e1, e2) -> (match eval_expr env mem e1 with
      |(B (a, _), mem1) -> (match eval_expr env mem1 e2 with
        |(Z i, mem2) -> (find mem2 (a+i), mem2)
        | _ -> failwith "Expected an integer in nth expression")
      | _ -> failwith "Expected a vector in nth expression")
  | ASTLen e -> (match eval_expr env mem e with
      |(B (_, n), mem') -> (Z n, mem')
      | _ -> failwith "Expected a vector in len expression")
  
and eval_exprs env mem exprs =
  match exprs with
  | [] -> []
  | e :: rest -> let (v, mem') = eval_expr env mem e in v :: eval_exprs env mem' rest

(* definitions *)
let eval_def env mem def =
  match def with
  | ASTConst (x, _, e) -> let (v, mem') = eval_expr env mem e in
      ((x, v) :: env , mem')
  | ASTFun (x, _, args, e) -> let f = F (e, build_types args, env) in
      ((x, f) :: env , mem)
  | ASTFunRec (x, _, args, e) -> let fr = FR (e, x, build_types args, env) in
      ((x, fr) :: env , mem)
  | ASTVar (x,_) -> let (a, mem') = alloc mem in
    (match (inDomain mem a) with
    | true -> failwith "Address already in use"
    | false -> ((x, A a) :: env , mem'))
    
    
  | ASTProc (x, argsp, bk) -> let p = P (bk, build_vars argsp, env) in
      ((x, p) :: env , mem)
  | ASTProcRec (x, argsp, bk) -> let pr = PR (bk, x, build_vars argsp, env) in
      ((x, pr) :: env , mem)
  

(* paramètres d'appel*)
let eval_expar env mem expar =
  match expar with
  | ASTAdr x -> (match get_value env x with
      | A a -> A a
      | _ -> failwith ("Expected a variable for address argument "^x))
  | ASTExpr e -> eval_expr env mem e |> fst

let rec eval_expars env mem expars =
  match expars with
  | [] -> []
  | e :: rest -> eval_expar env mem e :: eval_expars env mem rest
  
(* instruction *)
let rec eval_stat env mem outFlux s =
  match s with 
  | ASTEcho e -> let (x, _)= eval_expr env mem e in (match x with 
      | Z n -> (mem, n :: outFlux)
      | _ -> failwith "Expected an integer in echo statement")
  | ASTSet (lv, e) -> let (v, mem') = eval_expr env mem e in
      let (a, mem'') = eval_lval env mem' lv 
      in (update mem'' a v, outFlux)
  | ASTIfS (e, bk1, bk2) -> (match eval_expr env mem e with
      | (Z 1, _) -> eval_block env mem outFlux bk1
      | (Z 0, _) -> eval_block env mem outFlux bk2
      | _ -> failwith "Expected a boolean value for if condition")

  | ASTWhile (e, bk) -> (match eval_expr env mem e with
      | (Z 1, _) -> let (mem', outFlux') = eval_block env mem outFlux bk in
          eval_stat env mem' outFlux' (ASTWhile (e, bk))
      | (Z 0, _) -> (mem, outFlux)
      | _ -> failwith "Expected a boolean value for while condition")
  
  | ASTCall (x, exprs) -> (match get_value env x with
      | P (bk, args, env') -> let vals = eval_expars env mem exprs in
              let new_env = ajout_list_env env' args vals in
              eval_block new_env mem outFlux bk
      | PR (bk, x, args, env') -> let vals = eval_expars env mem exprs in
              let new_env = ajout_list_env ((x, PR (bk, x, args, env')) :: env') args vals in
              eval_block new_env mem outFlux bk
      | _ -> failwith "Expected a procedure in call statement")
          
and eval_lval env mem lval =
  match lval with
  | ASTLId x -> (match get_value env x with
      | A a -> (a, mem)
      | _ -> failwith ("Expected a variable for lvalue "^x))
  | ASTNth (ASTLId x, e) -> (match get_value env x with
      | B (a, _) -> (match eval_expr env mem e with
          |(Z i, mem') -> (a + i, mem')
          | _ -> failwith ("Expected an integer for nth expression "^x))
      | A addr -> (match eval_expr env mem e with (*si le tableau a été initialisé avec un VAR/SET, alors il est stocké en tant qu'adresse vers un tableau*)
          |(Z i, mem') -> (match find mem' addr with
              | B (a, _) -> (a + i, mem')
              | _ -> failwith ("Expected a vector variable for lvalue "^x))
          | _ -> failwith ("Expected an integer for nth expression "^x))
      | _ -> failwith ("Expected a vector variable for lvalue "^x))
  | ASTNth (lv, e) -> let (a, mem') = eval_lval env mem lv in
      (match find mem' a with
        |B (a',_) -> (match eval_expr env mem' e with
          |(Z i, mem'') -> (a' + i, mem'')
          | _ -> failwith ("Expected an integer for nth expression "))
        | _ -> failwith ("Expected a vector variable for lvalue "))
      

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


*)