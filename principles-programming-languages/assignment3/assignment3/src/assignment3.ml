open Ast
open Eval
open ExpressionLibrary

let _ = Printexc.record_backtrace(true)


type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec insert tree x =
  match tree with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node(l, y, r) ->
     if x = y then tree
     else if x < y then Node(insert l x, y, r)
     else Node(l, y, insert r x)

let construct l =
  List.fold_left (fun acc x -> insert acc x) Leaf l

(**********************************)
(* Problem 1: Tree In-order Fold  *)
(**********************************)

let rec fold_inorder f acc t =
  match t with
  | Leaf -> acc
  | Node (l, x, r) ->
      let acc_left = fold_inorder f acc l in
      let acc_here = f acc_left x in
      fold_inorder f acc_here r

(**********************************)
(* Problem 2: BST Remove *)
(**********************************)

let rec min_value t =
  match t with
  | Leaf -> failwith "min_value on empty tree"
  | Node (Leaf, x, _) -> x
  | Node (l, _, _) -> min_value l

let rec remove target t =
  match t with
  | Leaf -> Leaf
  | Node (l, v, r) ->
      if target < v then Node (remove target l, v, r)
      else if target > v then Node (l, v, remove target r)
      else
        (* target = v then delete this node *)
        match l, r with
        | Leaf, Leaf -> Leaf
        | Leaf, _ -> r
        | _, Leaf -> l
        | _ ->
            let succ = min_value r in
            Node (l, succ, remove succ r)


(**********************************)
(* Problem 3: Simplification *)
(**********************************)

(* Type definitions for the abstract syntax tree for expressions already defined in ast.ml *)

(**********************************
    type binop = Add | Sub | Mul

    type expression =
      | Num of float
      | Var
      | Binop of binop * expression * expression
***********************************)


(**********************************
    There are some functions from expressionLibrary that you can use to debug your code.

    `parse: string -> expression` :
        translates a string in infix form (such as `x*x - 3.0*x + 2.5`) into an expression
        (treating `x` as the variable). The parse function parses according to the standard
        order of operations - so `5+x*8` will be read as `5+(x*8)`.
    `to_string: expression -> string` :
        prints expressions in a readable form, using infix notation. This function adds
        parentheses around every binary operation so that the output is completely unambiguous.
    `to_string_wo_paren: expression -> string` :
        prints expressions in a readable form, using infix notation. This function does not
        add any parentheses so it can only be used for expressions in standard forms.
    `make_exp: int -> expression` :
        takes in a length `l` and returns a randomly generated expression of length at most `2l`.
    `rand_exp_str: int -> string` :
        takes in a length `l` and returns a string representation of length at most `2l`.

    For example,

    let _ =
      (* The following code make an expression from a string
         "5*x*x*x + 4*x*x + 3*x + 2 + 1", and then convert the
         expression back to a string, finally it prints out the
         converted string
         *)
      let e = parse ("5*x*x*x + 4*x*x + 3*x + 2 + 1") in
      let s = to_string e in
      print_string (s^"\n")

    let _ =
      (* The following code make a random expression from a string
         and then convert the expression back to a string
         finally it prints out the converted string
         *)
      let e = make_exp 10 in
      let s = to_string e in
      print_string (s^"\n")
***********************************)
type poly = (int * float) list

let add_term d c p =
  if c = 0. then p
  else
    let rec go = function
      | [] -> [ (d, c) ]
      | (d', c') :: rest ->
          if d = d' then
            let c'' = c' +. c in
            if c'' = 0. then rest else (d', c'') :: rest
          else (d', c') :: go rest
    in
    go p

let poly_add p q =
  List.fold_left (fun acc (d, c) -> add_term d c acc) p q

let poly_neg p = List.map (fun (d, c) -> (d, -. c)) p
let poly_sub p q = poly_add p (poly_neg q)

let poly_mul p q =
  List.fold_left
    (fun acc (d1, c1) ->
       List.fold_left
         (fun acc' (d2, c2) -> add_term (d1 + d2) (c1 *. c2) acc')
         acc q)
    [] p

let rec to_poly (e : expression) : poly =
  match e with
  | Num n -> if n = 0. then [] else [ (0, n) ]
  | Var -> [ (1, 1.) ]
  | Binop (Add, e1, e2) -> poly_add (to_poly e1) (to_poly e2)
  | Binop (Sub, e1, e2) -> poly_sub (to_poly e1) (to_poly e2)
  | Binop (Mul, e1, e2) -> poly_mul (to_poly e1) (to_poly e2)

let rec pow_x d =
  if d <= 0 then Num 1.
  else if d = 1 then Var
  else Binop (Mul, Var, pow_x (d - 1))

let poly_to_expr (p : poly) : expression =
  let p =
    p
    |> List.filter (fun (_, c) -> c <> 0.)
    |> List.sort (fun (d1, _) (d2, _) -> compare d2 d1)
  in
  let term (d, c) : expression =
    if d = 0 then Num c
    else Binop (Mul, Num c, pow_x d)
  in
  match p with
  | [] -> Num 0.
  | (d, c) :: rest ->
      let start = term (d, c) in
      List.fold_left
        (fun acc (d, c) ->
          let t = term (d, c) in
          Binop (Add, acc, t))
        start rest

let simplify (e : expression) : expression =
  poly_to_expr (to_poly e)



(************************************)
(* Problem 4: Operational Semantics *)
(************************************)

(************ DO NOT CHANGE ANY CODE FROM HERE **************)
(* Your code for Problem 4 should be implemented in eval.ml *)
(************************************************************)

(* Parse a file of Imp source code *)
let load (filename : string) : Ast.com =
  let ch =
    try open_in filename
    with Sys_error s -> failwith ("Cannot open file: " ^ s) in
  let parse : com =
    try Parser.main Lexer.token (Lexing.from_channel ch)
    with e ->
      let msg = Printexc.to_string e
      and stack = Printexc.get_backtrace () in
      Printf.eprintf "there was an error: %s%s\n" msg stack;
      close_in ch; failwith "Cannot parse program" in
  close_in ch;
  parse

(* Interpret a parsed AST with the eval_command function defined in eval.ml *)
let eval (parsed_ast : Ast.com) : environment =
  let env = [] in
  eval_command parsed_ast env


(*********************)
(* Testing your code *)
(*********************)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (******************************)
  (**** 1. Test fold_inorder ****)
  (******************************)

   let _ =
     try
       assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3])
     with e -> (error_count := !error_count + 1;
     let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s %s" msg stack) in

   let _ =
     try
       assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = 6)
     with e -> (error_count := !error_count + 1;
     let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s %s" msg stack) in

  (******************************)
  (******* 2. Test remove *******)
  (******************************)

   let _ =
     try
       assert (remove 20 (Node (Node (Node (Leaf, 20, Leaf), 30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
               = (Node (Node (Leaf,                  30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf)))));
     with e -> (error_count := !error_count + 1;
     let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s %s" msg stack) in

   let _ =
     try
       assert (remove 30 (Node (Node (Leaf,                  30, Node (Leaf, 40, Leaf)), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
               = (Node (Node (Leaf,                  40, Leaf                 ), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf)))));
     with e -> (error_count := !error_count + 1;
     let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s %s" msg stack) in


     let _ =
       try
         assert (remove 50 (Node (Node (Leaf,                  40, Leaf                 ), 50, Node (Node (Leaf, 60, Leaf), 70, Node (Leaf, 80, Leaf))))
                 = (Node (Node (Leaf,                  40, Leaf                 ), 60, Node (Leaf,                  70, Node (Leaf, 80, Leaf)))))
       with e -> (error_count := !error_count + 1;
       let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
       Printf.eprintf "there was an error: %s %s" msg stack) in

   (********************************)
   (******* 3. Test simplify *******)
   (********************************)

   let _ =
     try
       assert (to_string_wo_paren (simplify (parse "3*x*x + 2*x - 5 + 4*x*x - 7*x")) = "7.*x*x+-5.*x+-5.");
     with e -> (error_count := !error_count + 1;
     let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s %s" msg stack) in


   let _ =
     try
       assert (to_string_wo_paren (simplify (parse "3*x*x + 8*x + 2*x*x - 5 - 13*x")) = "5.*x*x+-5.*x+-5.");
     with e -> (error_count := !error_count + 1;
     let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s %s" msg stack) in

   let _ =
     try
       assert (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)")) = "1.*x*x*x+-6.*x*x+5.*x");
     with e -> (error_count := !error_count + 1;
     let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s %s" msg stack) in

   let _ =
     try
       assert (to_string_wo_paren (simplify (parse "(x-1)*x*(x-5)*(4*x*x*x-11+66*x)")) = "4.*x*x*x*x*x*x+-24.*x*x*x*x*x+86.*x*x*x*x+-407.*x*x*x+396.*x*x+-55.*x");
     with e -> (error_count := !error_count + 1;
     let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s %s" msg stack) in

   let _ =
     try
       assert (to_string_wo_paren (simplify (parse "x - x")) = "0.");
     with e -> (error_count := !error_count + 1;
     let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s %s" msg stack) in

   let _ =
     try
       assert (to_string_wo_paren (simplify (parse "x + x + 0")) = "2.*x");
     with e -> (error_count := !error_count + 1;
     let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s %s" msg stack) in

   let _ =
     try
       assert (to_string_wo_paren (simplify (parse "0")) = "0.")
     with e -> (error_count := !error_count + 1;
     let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
     Printf.eprintf "there was an error: %s %s" msg stack) in



  (***************************)
  (**** 4. Test eval_expr ****)
  (***************************)

  let env1 = [("x", Int_Val 4); ("y", Bool_Val false)] in

  let _ =
    try
      (* eval_expr (x + 4) = 8 when x = 4 in the environment *)
      assert (Int_Val 8 = eval_expr (Plus ((Var "x"), (Number 4))) env1);
      (* eval_expr (50 - 8) = 42 *)
      assert (Int_Val 42 = eval_expr (Minus (Number 50, Number 8)) [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (5 * -2) = -10  *)
      assert (Int_Val (-10) = eval_expr (Times (Number 5, Number (-2))) []);
      (* eval_expr (70 / 7) = 10 *)
      assert (Int_Val 10 = eval_expr (Div (Number 70, Number 7)) []);
      (* eval_expr (3 % 2) = 1 *)
      assert (Int_Val 1 = eval_expr (Mod (Number 3, Number 2)) [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (false || y) = false when y = false in the environment *)
      assert (Bool_Val false = eval_expr (Or (False, Var "y")) env1);
      (* eval_expr (false && true) = false *)
      assert (Bool_Val false = eval_expr (And (False, True)) []);
      (* eval_expr (not true) = false *)
      assert (Bool_Val false = eval_expr (Not (True)) env1)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (x = 10) = false when x = 4 in the environment *)
      assert (Bool_Val false = eval_expr (Eq (Var "x", Number 10)) env1);
      (* eval_expr (x < 10) = true when x = 4 in the environment *)
      assert (Bool_Val true = eval_expr (Lt (Var "x", Number 10)) env1);
      (* eval_expr (1 <= 0) = false *)
      assert (Bool_Val false = eval_expr (Leq (Number 1, Number 0)) [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr ((fun x -> x + 3) 5) = 8 *)
      let e1 = Fun ("x", Plus (Var "x", Number 3)) in
      let e2 = Number 5 in
      assert (Int_Val 8 = eval_expr (App (e1, e2)) [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (fun z -> z - 2) ((fun x -> x + 1) 2) = 1 *)
      let e1 = Fun ("x", Plus (Var "x", Number 1)) in
      let e2 = Number 2 in
      let e3 = App (e1, e2) in
      let e4 = Fun ("z", Minus (Var "z", Number 2)) in
      let e5 = App (e4, e3) in
      assert (Int_Val 1 = eval_expr e5 [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (fun x -> fun y -> x) 2 3 =  2 *)
      let e1 = Fun ("x", Fun ("y", Var "x")) in
      let e2 = App (App (e1, Number 2), Number 3) in
      assert (Int_Val 2 = eval_expr e2 [])
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  (****************************)
  (*** 5. Test eval_command ***)
  (****************************)

  let _ =
    try
      let parsed_ast = load ("programs/aexp-add.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- x => 10\n\
         - y => 15\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/aexp-combined.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- w => -13\n\
         - x => 1\n\
         - y => 2\n\
         - z => 3\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/bexp-combined.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- res1 => 1\n\
         - res10 => 0\n\
         - res11 => 0\n\
         - res12 => 0\n\
         - res13 => 1\n\
         - res14 => 1\n\
         - res15 => 1\n\
         - res16 => 0\n\
         - res2 => 0\n\
         - res3 => 1\n\
         - res4 => 0\n\
         - res5 => 0\n\
         - res6 => 1\n\
         - res7 => 0\n\
         - res8 => 0\n\
         - res9 => 1\n\
         - w => 5\n\
         - x => 3\n\
         - y => 5\n\
         - z => -3\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/cond.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- n1 => 255\n\
         - n2 => -5\n\
         - res1 => 1\n\
         - res2 => 255\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/fact.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- f => 120\n\
         - n => 1\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/fib.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- f0 => 5\n\
         - f1 => 8\n\
         - k => 6\n\
         - n => 5\n\
         - res => 8\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/for.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- i => 101\n\
         - n => 101\n\
         - sum => 5151\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/palindrome.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- n => 135\n\
         - res => 1\n\
         - res2 => 0\n\
         - reverse => 123454321\n\
         - reverse2 => 531\n\
         - temp => 0\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/while.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- n => 0\n\
         - sum => 5050\n");
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      let parsed_ast = load ("programs/lambda.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert (
      "- a => 0\n\
       - b => 5\n\
       - x => 10\n"
      = result)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (*
        eval
          (int result;
          lambda f;
          lambda g;
          f := fun x -> x + 3;
          g := f;
          result := g 5;)
        =
          "- result => 8\n"
      *)
      let e1 = Fun ("x", Plus (Var "x", Number 3)) in
      let p0 = Declare (Int_Type, "result") in
      let p1 = Declare (Lambda_Type, "f") in
      let p2 = Declare (Lambda_Type, "g") in
      let p3 = Assg ("f", e1) in
      let p4 = Assg ("g", Var "f") in
      let p5 = Assg ("result", App (Var "g", Number 5)) in
      let p =  Comp (p0, Comp (p1, Comp (p2, Comp (p3, Comp (p4, p5))))) in
      let result = print_env_str(eval p) in
      assert (
      "- result => 8\n"
      = result)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (*
        eval
          (int result;
          lambda f;
          f := (fun x -> fun y -> fun w -> w * (x + y)) 3 4;
          result := f 5;)
        =
          "- result => 35\n"
      *)
      let e1 = Fun ("x", Fun ("y", Fun ("w", Times (Var "w", Plus (Var "x", Var "y"))))) in
      let e2 = App (App (e1, Number 3), Number 4) in
      let p0 = Declare (Int_Type, "result") in
      let p1 = Declare (Lambda_Type, "f") in
      let p2 = Assg ("f", e2) in
      let p3 = Assg ("result", App (Var "f", Number 5)) in
      let p =  Comp (p0, Comp (p1, Comp (p2, p3))) in
      let result = print_env_str(eval p) in
      assert (
      "- result => 35\n"
      = result)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (*
        eval
          (int result;
          lambda f;
          lambda g;
          f := fun x -> fun y -> x - y;
          g := f 2;
          int x;
          x := 3;
          result := g 4;
          )
        =
          "- result => -2\n\
          - x => 3\n"
      *)
      let e1 = Fun ("x", Fun ("y", Minus (Var "x", Var "y"))) in
      let e2 = App (Var "f", Number 2) in
      let p0 = Declare (Int_Type, "result") in
      let p1 = Declare (Lambda_Type, "f") in
      let p2 = Declare (Lambda_Type, "g") in
      let p3 = Assg ("f", e1) in
      let p4 = Assg ("g", e2) in
      let p5 = Declare (Int_Type, "x") in
      let p6 = Assg ("x", Number 3) in
      let p7 = Assg ("result", App (Var "g", Number 4)) in
      let p =  Comp (p0, Comp (p1, Comp (p2, Comp (p3, Comp (p4, Comp (p5, Comp (p6, p7))))))) in
      let result = print_env_str(eval p) in
      assert (
      "- result => -2\n\
       - x => 3\n"
      = result)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (*
        eval
          (int result;
          lambda f;
          lambda g;
          f := fun f -> fun x -> f (f x);
          g := fun x -> x + 3;
          result := f g 5;)
        =
          "- result => 11\n"
      *)
      let e1 = Fun ("f", Fun ("x", App (Var "f", App (Var "f", Var "x")))) in
      let e2 = Fun ("x", Plus (Var "x", Number 3)) in
      let p0 = Declare (Int_Type, "result") in
      let p1 = Declare (Lambda_Type, "f") in
      let p2 = Declare (Lambda_Type, "g") in
      let p3 = Assg ("f", e1) in
      let p4 = Assg ("g", e2) in
      let p5 = Assg ("result", App (App (Var "f", Var "g"), Number 5)) in
      let p =  Comp (p0, Comp (p1, Comp (p2, Comp (p3, Comp (p4, p5))))) in
      let result = print_env_str(eval p) in
      assert (
      "- result => 11\n"
      = result)
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  (******************************)
  (*** 3. Test Error Handling ***)
  (******************************)

  let _ =
    try
      (*
        eval
          (int result;
          lambda f;
          lambda g;
          f := fun x -> x + 1;
          g := f;
          result := g (fun i -> i + 1) 1;
          )
        =
          TypeError
      *)
      let e1 = Fun ("x", Plus (Var "x", Number 1)) in
      let p0 = Declare (Int_Type, "result") in
      let p1 = Declare (Lambda_Type, "f") in
      let p2 = Declare (Lambda_Type, "g") in
      let p3 = Assg ("f", e1) in
      let p4 = Assg ("g", Var "f") in
      let p5 = Assg ("result", App (App (Var "g", Fun("i", Plus (Var "i", Number 1))), Number 1)) in
      let p =  Comp (p0, Comp (p1, Comp (p2, Comp (p3, Comp (p4, p5))))) in
      try
        ignore (eval_command p []);
        assert false
      with
        | TypeError -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in


  let env1 : environment = [("x", Int_Val(1)); ("p", Bool_Val(false))] in
  let env2 : environment = [("x", Int_Val(1)); ("p", Bool_Val(false)); ("y", Int_Val(6)); ("q", Bool_Val(true))] in

  let _ =
    try
      (* eval_expr (1 + p) =  TypeError when p = false in the environment *)
      try
        ignore(eval_expr (Plus (Number 1, Var "p")) env1);
        assert false (* this line shouldn't be reached! *)
      with
        | TypeError -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (q && True < y) = TypeError *)
      try
        ignore(eval_expr (And (Var "q", Lt (True, Var "y"))) env2);
        assert false (* this line shouldn't be reached! *)
      with
        | TypeError -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (x / 0) = DivByZeroError *)
      try
        ignore(eval_expr (Div (Var "x", Number 0)) env2);
        assert false (* this line shouldn't be reached! *)
      with
        | DivByZeroError -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (* eval_expr (x % y) = UndefinedVar in the environment without any definition of y *)
      try
        ignore(eval_expr (Mod (Var "x", Var "y")) env1);
        assert false (* this line shouldn't be reached! *)
      with
        | UndefinedVar -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in


  let _ =
    try
      (*
        eval
          (int f;
          n := 5;
          f := 1;
          while (f <= 1) {
            f = f * 5;
            n = n - 1;
          })
        =
          UndefinedVar
      *)
      let p =
          Comp(Declare (Int_Type, "f"),
              Comp (Assg ("n", Number 5),
              Comp (Assg ("f", Number 1),
                  While (Leq (Var "f", Number 1),
                      Comp(Assg ("f", Times (Var "f", Number 5)),
                           Assg ("n", Minus (Var "n", Number 1))))))) in
      try
        ignore (eval_command p []);
        assert false (* this line shouldn't be reached! *)
      with
        | UndefinedVar -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  let _ =
    try
      (*
        eval
          (int f;
          f := 1;
          while (f) {
            f = f * 5;
          })
        =
          TypeError
      *)
      let p =
          Comp(Declare (Int_Type, "f"),
              Comp (Assg ("f", Number 1),
                  While (Var "f",
                      Assg ("f", Times (Var "f", Number 5))))) in
      try
        ignore (eval_command p []);
        assert false (* this line shouldn't be reached! *)
      with
        | TypeError -> ()
        | e -> assert false
    with e -> (error_count := !error_count + 1;
    let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s %s" msg stack) in

  if !error_count = 0 then  Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 38 programming questions are incorrect.\n") (!error_count)

let _ = main()
