open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) =
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) =
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
  match lookahead toks with
  | Some Tok_Let -> parse_let toks
  | Some Tok_If -> parse_if toks
  | Some Tok_Fun -> parse_fun toks
  | _ -> parse_or toks

and parse_let toks =
  let toks = match_token toks Tok_Let in
  let (is_rec, toks) = 
    match lookahead toks with
    | Some Tok_Rec -> (true, match_token toks Tok_Rec)
    | _ -> (false, toks) 
  in
  match lookahead toks with
  | Some (Tok_ID id) ->
      let toks = match_token toks (Tok_ID id) in
      let toks = match_token toks Tok_Equal in
      let (toks, e1) = parse_expr toks in
      let toks = match_token toks Tok_In in
      let (toks, e2) = parse_expr toks in
      (toks, Let(id, is_rec, e1, e2))
  | _ -> raise (InvalidInputException "Expected ID in let expression")

and parse_fun toks =
  let toks = match_token toks Tok_Fun in
  match lookahead toks with
  | Some (Tok_ID id) ->
      let toks = match_token toks (Tok_ID id) in
      let toks = match_token toks Tok_Arrow in
      let (toks, body) = parse_expr toks in
      (toks, Fun(id, body))
  | _ -> raise (InvalidInputException "Expected ID in fun expression")

and parse_if toks =
  let toks = match_token toks Tok_If in
  let (toks, e1) = parse_expr toks in
  let toks = match_token toks Tok_Then in
  let (toks, e2) = parse_expr toks in
  let toks = match_token toks Tok_Else in
  let (toks, e3) = parse_expr toks in
  (toks, If(e1, e2, e3))

and parse_or toks =
  let (toks, e1) = parse_and toks in
  match lookahead toks with
  | Some Tok_Or ->
      let toks = match_token toks Tok_Or in
      let (toks, e2) = parse_or toks in
      (toks, Binop(Or, e1, e2))
  | _ -> (toks, e1)

and parse_and toks =
  let (toks, e1) = parse_equality toks in
  match lookahead toks with
  | Some Tok_And ->
      let toks = match_token toks Tok_And in
      let (toks, e2) = parse_and toks in
      (toks, Binop(And, e1, e2))
  | _ -> (toks, e1)

and parse_equality toks =
  let (toks, e1) = parse_relational toks in
  match lookahead toks with
  | Some Tok_Equal ->
      let toks = match_token toks Tok_Equal in
      let (toks, e2) = parse_equality toks in
      (toks, Binop(Equal, e1, e2))
  | Some Tok_NotEqual ->
      let toks = match_token toks Tok_NotEqual in
      let (toks, e2) = parse_equality toks in
      (toks, Binop(NotEqual, e1, e2))
  | _ -> (toks, e1)

and parse_relational toks =
  let (toks, e1) = parse_additive toks in
  match lookahead toks with
  | Some Tok_Less ->
      let toks = match_token toks Tok_Less in
      let (toks, e2) = parse_relational toks in
      (toks, Binop(Less, e1, e2))
  | Some Tok_Greater ->
      let toks = match_token toks Tok_Greater in
      let (toks, e2) = parse_relational toks in
      (toks, Binop(Greater, e1, e2))
  | Some Tok_LessEqual ->
      let toks = match_token toks Tok_LessEqual in
      let (toks, e2) = parse_relational toks in
      (toks, Binop(LessEqual, e1, e2))
  | Some Tok_GreaterEqual ->
      let toks = match_token toks Tok_GreaterEqual in
      let (toks, e2) = parse_relational toks in
      (toks, Binop(GreaterEqual, e1, e2))
  | _ -> (toks, e1)

and parse_additive toks =
  let (toks, e1) = parse_multiplicative toks in
  match lookahead toks with
  | Some Tok_Add ->
      let toks = match_token toks Tok_Add in
      let (toks, e2) = parse_additive toks in
      (toks, Binop(Add, e1, e2))
  | Some Tok_Sub ->
      let toks = match_token toks Tok_Sub in
      let (toks, e2) = parse_additive toks in
      (toks, Binop(Sub, e1, e2))
  | _ -> (toks, e1)

and parse_multiplicative toks =
  let (toks, e1) = parse_concat toks in
  match lookahead toks with
  | Some Tok_Mult ->
      let toks = match_token toks Tok_Mult in
      let (toks, e2) = parse_multiplicative toks in
      (toks, Binop(Mult, e1, e2))
  | Some Tok_Div ->
      let toks = match_token toks Tok_Div in
      let (toks, e2) = parse_multiplicative toks in
      (toks, Binop(Div, e1, e2))
  | _ -> (toks, e1)

and parse_concat toks =
  let (toks, e1) = parse_unary toks in
  match lookahead toks with
  | Some Tok_Concat ->
      let toks = match_token toks Tok_Concat in
      let (toks, e2) = parse_concat toks in
      (toks, Binop(Concat, e1, e2))
  | _ -> (toks, e1)

and parse_unary toks =
  match lookahead toks with
  | Some Tok_Not ->
      let toks = match_token toks Tok_Not in
      let (toks, e) = parse_unary toks in
      (toks, Not(e))
  | _ -> parse_function_call toks

and parse_function_call toks =
  let (toks, e1) = parse_primary toks in
  let rec parse_args current_toks current_expr =
    match lookahead current_toks with
    | Some (Tok_Int _) | Some (Tok_Bool _) | Some (Tok_String _) 
    | Some (Tok_ID _) | Some Tok_LParen ->
        let (next_toks, arg) = parse_primary current_toks in
        parse_args next_toks (FunctionCall(current_expr, arg))
    | _ -> (current_toks, current_expr)
  in
  parse_args toks e1

and parse_primary toks =
  match lookahead toks with
  | Some (Tok_Int i) -> (match_token toks (Tok_Int i), Int i)
  | Some (Tok_Bool b) -> (match_token toks (Tok_Bool b), Bool b)
  | Some (Tok_String s) -> (match_token toks (Tok_String s), String s)
  | Some (Tok_ID id) -> (match_token toks (Tok_ID id), ID id)
  | Some Tok_LParen ->
      let toks = match_token toks Tok_LParen in
      let (toks, e) = parse_expr toks in
      let toks = match_token toks Tok_RParen in
      (toks, e)
  | _ -> raise (InvalidInputException "Expected primary expression")
