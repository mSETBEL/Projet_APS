
type value = 
    Z of int
  | F of closure
  | FR of recClosure
and closure = expr * string list * env
and recClosure = expr * string * string list * env 
and env = (string * value) list

type outFlux = int list

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


let get_value env x =
  match env with 
  | [] -> failwith ("Variable "^x^" not found")
  | (y, v) :: rest -> if x = y then v else get_value rest x


let build_types args =
  match args with
  | [] -> []
  | (ASTArg (_, t)) :: rest -> t :: build_types rest


(* commandes *)
let rec eval_cmds env flux cmds =
  match cmds with
  | ASTStat s -> let flux = eval_stat env flux s in reverse flux 
  | ASTDef (d, cs) -> let new_env = eval_def env p in 
      eval_cmds new_env flux cs
  


(* definitions *)
let eval_def env def =
  match def with
  | ASTConst (x, t, e) -> let v = eval_expr env e in
      (x, v) :: env
  | ASTFun (x, t, args, e) -> let f = F (e, build_types args, env) in
      (x, f) :: env
  | ASTFunRec (x, t, args, e) -> let fr = FR (e, x, build_types args, env) in
      (x, fr) :: env


(* instruction *)
let eval_stat env flux s =
  match s with 
  | ASTEcho e -> let n = eval_expr env e in n::flux (* à inverser à la fin*)

(* expressions *)
let rec eval_expr env exp =
  match exp with
  | ASTBool true -> Z 1
  | ASTBool false -> Z 0
  | ASTNum n -> Z n
  | ASTId x -> get_value env x
  | ASTApp (ASTId "not", [e]) -> (match eval_expr env e with
      | Z n -> Z (pi1("not" , n)) 
      | _ -> failwith "Expected an integer for not operator"
    )
  | ASTApp (ASTId x, [e1; e2]) where x = ("eq" || "lt" || "add" || "sub" ||  "mul" ||  "div") -> 
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
