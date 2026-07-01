open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Print env to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Print env to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
  match e with
  | Number n -> Int_Val n
  | True -> Bool_Val true
  | False -> Bool_Val false
  | Var x -> (try List.assoc x env with Not_found -> raise UndefinedVar)
  | Plus (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
       | Int_Val a, Int_Val b -> Int_Val (a + b)
       | _ -> raise TypeError)
  | Minus (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
       | Int_Val a, Int_Val b -> Int_Val (a - b)
       | _ -> raise TypeError)
  | Times (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
       | Int_Val a, Int_Val b -> Int_Val (a * b)
       | _ -> raise TypeError)
  | Div (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
       | Int_Val a, Int_Val 0 -> raise DivByZeroError
       | Int_Val a, Int_Val b -> Int_Val (a / b)
       | _ -> raise TypeError)
  | Mod (e1, e2) ->
      let v1 = eval_expr e1 env and v2 = eval_expr e2 env in
      (match (v1, v2) with
       | Int_Val a, Int_Val 0 -> raise DivByZeroError
       | Int_Val a, Int_Val b -> Int_Val (a mod b)
       | _ -> raise TypeError)
  | Eq (e1, e2) ->
      (match (eval_expr e1 env, eval_expr e2 env) with
       | Int_Val a, Int_Val b -> Bool_Val (a = b)
       | Bool_Val a, Bool_Val b -> Bool_Val (a = b)
       | _ -> raise TypeError)
  | Leq (e1, e2) ->
      (match (eval_expr e1 env, eval_expr e2 env) with
       | Int_Val a, Int_Val b -> Bool_Val (a <= b)
       | _ -> raise TypeError)
  | Lt (e1, e2) ->
      (match (eval_expr e1 env, eval_expr e2 env) with
       | Int_Val a, Int_Val b -> Bool_Val (a < b)
       | _ -> raise TypeError)
  | Not e1 ->
      (match eval_expr e1 env with
       | Bool_Val b -> Bool_Val (not b)
       | _ -> raise TypeError)
  | And (e1, e2) ->
      (match eval_expr e1 env with
       | Bool_Val false -> Bool_Val false
       | Bool_Val true -> (match eval_expr e2 env with
                           | Bool_Val b -> Bool_Val b
                           | _ -> raise TypeError)
       | _ -> raise TypeError)
  | Or (e1, e2) ->
      (match eval_expr e1 env with
       | Bool_Val true -> Bool_Val true
       | Bool_Val false -> (match eval_expr e2 env with
                            | Bool_Val b -> Bool_Val b
                            | _ -> raise TypeError)
       | _ -> raise TypeError)
  | Fun (x, body) -> Closure (env, x, body)
  | App (f, arg) ->
      (match eval_expr f env with
       | Closure (closure_env, param, body) ->
           let arg_val = eval_expr arg env in
           eval_expr body ((param, arg_val) :: closure_env)
       | _ -> raise TypeError)


(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
  match c with
  | Skip -> env
  | Declare (dtype, name) ->
      let default_val =
        match dtype with
        | Int_Type -> Int_Val 0
        | Bool_Type -> Bool_Val false
        | Lambda_Type -> Closure (env, "_", Number 0)
      in
      (name, default_val) :: env
  | Assg (name, expr) ->
      let value = eval_expr expr env in
      if List.mem_assoc name env then
        (name, value) :: List.remove_assoc name env
      else
        raise UndefinedVar
  | Comp (c1, c2) ->
      let env1 = eval_command c1 env in
      eval_command c2 env1
  | Cond (cond, then_c, else_c) ->
      (match eval_expr cond env with
       | Bool_Val true -> eval_command then_c env
       | Bool_Val false -> eval_command else_c env
       | _ -> raise TypeError)
  | While (cond, body) ->
      let rec loop env =
        match eval_expr cond env with
        | Bool_Val true -> loop (eval_command body env)
        | Bool_Val false -> env
        | _ -> raise TypeError
      in
      loop env
  | For (e, body) ->
      let times =
        match eval_expr e env with
        | Int_Val n -> n
        | _ -> raise TypeError
      in
      let rec run n env =
        if n <= 0 then env else run (n - 1) (eval_command body env)
      in
      run times env
