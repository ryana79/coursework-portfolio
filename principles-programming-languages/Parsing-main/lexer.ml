open TokenTypes
open String

(*type token =
  | Tok_RParen
  | Tok_LParen
  | Tok_Equal
  | Tok_NotEqual
  | Tok_Greater
  | Tok_Less
  | Tok_GreaterEqual
  | Tok_LessEqual
  | Tok_Or
  | Tok_And
  | Tok_Not
  | Tok_If
  | Tok_Then
  | Tok_Else
  | Tok_Add
  | Tok_Sub
  | Tok_Mult
  | Tok_Div
  | Tok_Concat
  | Tok_Let
  | Tok_Rec
  | Tok_In
  | Tok_Def
  | Tok_Fun
  | Tok_Arrow
  | Tok_Int of int
  | Tok_Bool of bool
  | Tok_String of string
  | Tok_ID of string
  | Tok_DoubleSemi*)


(* We provide the regular expressions that may be useful to your code *)

let re_rparen = Str.regexp ")";;
let re_lparen = Str.regexp "(";;
let re_equal = Str.regexp "=";;
let re_not_equal = Str.regexp "<>";;
let re_greater = Str.regexp ">";;
let re_less = Str.regexp "<";;
let re_greater_equal = Str.regexp ">=";;
let re_less_equal = Str.regexp "<=";;
let re_or = Str.regexp "||";;
let re_and = Str.regexp "&&";;
let re_not = Str.regexp "not";;
let re_if = Str.regexp "if";;
let re_then = Str.regexp "then";;
let re_else = Str.regexp "else";;
let re_add = Str.regexp "+";;
let re_sub = Str.regexp "-";;
let re_mult = Str.regexp "*";;
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^";;
let re_let = Str.regexp "let";;
let re_rec = Str.regexp "rec";;
let re_in = Str.regexp "in";;
let re_def = Str.regexp "def";;
let re_fun = Str.regexp "fun";;
let re_arrow = Str.regexp "->";;
let re_pos_int = Str.regexp "[0-9]+";;
let re_neg_int = Str.regexp "(-[0-9]+)";;
let re_true = Str.regexp "true";;
let re_false = Str.regexp "false";;
let re_string = Str.regexp "\"[^\"]*\"";;
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*";;
let re_double_semi = Str.regexp ";;";;
let re_whitespace = Str.regexp "[ \t\n]+";;

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input =
  let length = String.length input in
  let rec tok pos =
    if pos >= length then []
    else if Str.string_match re_whitespace input pos then
      tok (pos + String.length (Str.matched_string input))
    
    (* check for negative int BEFORE subtraction or LPAREN *)
    else if Str.string_match re_neg_int input pos then
      let s = Str.matched_string input in
      (* Strip parens: "(-123)" -> "-123" *)
      let num_str = String.sub s 1 (String.length s - 2) in
      Tok_Int (int_of_string num_str) :: tok (pos + String.length s)

    (* check for string literals *)
    else if Str.string_match re_string input pos then
      let s = Str.matched_string input in
      (* Strip quotes: "foo" -> foo *)
      let content = String.sub s 1 (String.length s - 2) in
      Tok_String content :: tok (pos + String.length s)

    (* check for IDs and Keywords *)
    else if Str.string_match re_id input pos then
      let s = Str.matched_string input in
      let token = match s with
        | "true" -> Tok_Bool true
        | "false" -> Tok_Bool false
        | "not" -> Tok_Not
        | "if" -> Tok_If
        | "then" -> Tok_Then
        | "else" -> Tok_Else
        | "let" -> Tok_Let
        | "rec" -> Tok_Rec
        | "in" -> Tok_In
        | "def" -> Tok_Def
        | "fun" -> Tok_Fun
        | _ -> Tok_ID s
      in
      token :: tok (pos + String.length s)

    else if Str.string_match re_arrow input pos then Tok_Arrow :: tok (pos + 2)
    else if Str.string_match re_double_semi input pos then Tok_DoubleSemi :: tok (pos + 2)
    else if Str.string_match re_greater_equal input pos then Tok_GreaterEqual :: tok (pos + 2)
    else if Str.string_match re_less_equal input pos then Tok_LessEqual :: tok (pos + 2)
    else if Str.string_match re_not_equal input pos then Tok_NotEqual :: tok (pos + 2)
    else if Str.string_match re_or input pos then Tok_Or :: tok (pos + 2)
    else if Str.string_match re_and input pos then Tok_And :: tok (pos + 2)
    
    else if Str.string_match re_lparen input pos then Tok_LParen :: tok (pos + 1)
    else if Str.string_match re_rparen input pos then Tok_RParen :: tok (pos + 1)
    else if Str.string_match re_equal input pos then Tok_Equal :: tok (pos + 1)
    else if Str.string_match re_greater input pos then Tok_Greater :: tok (pos + 1)
    else if Str.string_match re_less input pos then Tok_Less :: tok (pos + 1)
    else if Str.string_match re_add input pos then Tok_Add :: tok (pos + 1)
    else if Str.string_match re_sub input pos then Tok_Sub :: tok (pos + 1)
    else if Str.string_match re_mult input pos then Tok_Mult :: tok (pos + 1)
    else if Str.string_match re_div input pos then Tok_Div :: tok (pos + 1)
    else if Str.string_match re_concat input pos then Tok_Concat :: tok (pos + 1)
    
    else if Str.string_match re_pos_int input pos then
      let s = Str.matched_string input in
      Tok_Int (int_of_string s) :: tok (pos + String.length s)

    else
      raise (InvalidInputException (Printf.sprintf "Lexing error at position %d" pos))
  in
  tok 0
