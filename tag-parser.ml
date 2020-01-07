#use "reader.ml";;
open Reader;;

type constant = 
(*
quoted and unquoted: Pair(Symbol(name), Pair(sexpr, Nil())
booleans
chars
numbers
strings
tag def -> field is always a const. if the field is quote, it should not appear in the Const we generate.
tag ref
*)
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;

module type TAG_PARSER = 
sig
  val tag_parse_expression : sexpr -> expr
  val tag_parse_expressions : sexpr list -> expr list
end;; 
(* signature TAG_PARSER *)

module Tag_Parser 
(* : TAG_PARSER  *)
= struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "unquote";
   "unquote-splicing"];;  

(* work on the tag parser starts here *)

let rec convert_pairs_to_list sexpr = match sexpr with
| Pair(car, cdr) -> List.append [car] (convert_pairs_to_list cdr)
| Nil -> []
| other -> [other]

let rec convert_pairs_to_str_list sexpr = 
  let flat_list = convert_pairs_to_list sexpr in
  List.map (fun e -> match e with
  |Symbol(str)->str
  |_ -> raise X_syntax_error)
   flat_list;;

let rec check_arg_reoccurrence lst = 
  let rec check_if_args_exists arg rest = match rest with
  | car :: cdr -> if car = arg then true else check_if_args_exists arg cdr
  | [] -> false in
match lst with
| car :: cdr -> if (check_if_args_exists car cdr) then true else check_arg_reoccurrence cdr
| [] -> false


let rec is_proper_list sexpr = match sexpr with
    | Pair(car, cdr) -> is_proper_list cdr
    | Nil -> true
    | _ -> false

let get_first_element lst = match lst with
    | car :: cdr -> car
    | [] -> "empty list";;

let get_last_element lst = 
    let rev_lst = List.rev lst in match rev_lst with
    | car :: cdr -> car
    | [] -> "empty list";;

let rec remove_last_element lst = match lst with
    | [x] -> []
    | car :: cdr -> List.append [car] (remove_last_element cdr)
    | [] -> [];;

let rec remove_first_element lst = match lst with
| [x] -> []
| car :: cdr -> cdr
| [] -> [];;

let tag_parse_var str = 
      let is_reserved_word = List.mem str reserved_word_list in
          if is_reserved_word then raise X_syntax_error else Var(str);;

let rec ribs_to_var_list ribs = match ribs with
| Pair(Symbol v, _) -> Pair(Symbol v, Nil)
| Pair(Pair(Symbol v, _), rest) -> Pair(Symbol v, (ribs_to_var_list rest))
| Nil -> Nil
| _ -> raise X_syntax_error;;

let rec ribs_to_val_list ribs = match ribs with
| Pair(Symbol _,Pair( sexpr , Nil)) -> Pair(sexpr, Nil)
| Pair(Pair(Symbol _,Pair(sexpr,Nil)), rest) -> Pair(sexpr, (ribs_to_val_list rest))
| Nil -> Nil
| _ -> raise X_syntax_error;;

let rec change_to_whatever ribs = match ribs with
| Pair(Symbol e,Pair( sexpr , Nil)) ->  Pair(Symbol e, Pair(Symbol("quote"), Pair(Symbol("whatever"), Nil)))
| Pair(Pair(Symbol e,Pair(sexpr,Nil)), rest) -> 
  Pair(Pair(Symbol e,Pair(Pair(Symbol("quote"), Pair(Symbol("whatever"), Nil)),Nil)), (change_to_whatever rest))
| Nil -> Nil
| _ -> raise X_syntax_error;;

let rec wrap_ribs_in_set ribs body = match ribs with
|Pair(Pair(var,Pair(sexpr_val, Nil)),Nil) -> Pair(Pair(Symbol "set!", Pair(var, Pair(sexpr_val, Nil))), body)
|Pair(Pair(var,Pair(sexpr_val,Nil)), rest) -> Pair(Pair(Symbol "set!", Pair(var, Pair(sexpr_val, Nil))), (wrap_ribs_in_set rest body))
|Nil -> body
| _ -> raise X_syntax_error;;


let get_first_pair pairs = match pairs with
    | Nil -> Nil
    | Pair(car, cdr) -> car
    | _ -> raise X_syntax_error

let remove_first_pair pairs = match pairs with
    | Nil -> Nil
    | Pair(car, cdr) -> cdr
    | _ -> raise X_syntax_error

let rec parse_exp sexpr = match sexpr with
(*constants*)
  | Nil -> Const(Void)
  | Bool(e) -> Const(Sexpr(Bool(e)))
  | Char(e) -> Const(Sexpr(Char(e)))
  | Number(e) -> Const(Sexpr(Number(e)))
  | String(e) -> Const(Sexpr(String(e)))
  | Pair(Symbol("quote"), Pair(e, Nil)) -> Const(Sexpr(e))
  | Pair(Symbol("quasiquote"), Pair(e, Nil)) -> parse_exp (expand_quasiquote e)
  | TagRef(e) -> Const(Sexpr(TagRef(e)))
  | TaggedSexpr(e,Pair(Symbol "quote", Pair(x, Nil))) -> Const(Sexpr(TaggedSexpr(e, x)))
  | TaggedSexpr(e,Bool(x)) -> Const(Sexpr(TaggedSexpr(e, Bool(x))))
  | TaggedSexpr(e,Char(x)) -> Const(Sexpr(TaggedSexpr(e, Char(x))))
  | TaggedSexpr(e,Number(x)) -> Const(Sexpr(TaggedSexpr(e, Number(x))))
  | TaggedSexpr(e,String(x)) -> Const(Sexpr(TaggedSexpr(e, String(x))))
  | TaggedSexpr(e,TagRef(x)) -> Const(Sexpr(TaggedSexpr(e, TagRef(x))))
(*variables*)
  | Symbol(e) -> tag_parse_var e
(*conditionals*)        
  | Pair(Symbol("if"), Pair(test, Pair(dit, dif))) -> tag_parse_if test dit dif

  | Pair(Symbol("cond"), ribs) -> parse_exp (expand_cond ribs)
(*lambdas*)
  | Pair(Symbol("lambda"), Pair(args, body)) -> tag_parse_lambda args body
  | Pair(Symbol "let", Pair(ribs, body)) -> parse_exp (expand_let ribs body)
  | Pair(Symbol "let*", Pair(ribs, body)) -> parse_exp (expand_let_star ribs body)
  | Pair(Symbol "letrec", Pair(ribs, body)) -> parse_exp (expand_letrec ribs body)
  (* Const(Sexpr((expand_let ribs body))) *)
  (*or*)
  | Pair(Symbol "or", bool_pairs) -> tag_parse_or bool_pairs
  | Pair(Symbol "and", bool_pairs) -> parse_exp (expand_and bool_pairs)
  (*MIT define*)
  | Pair(Symbol "define", Pair(Pair(name, args), body)) -> parse_exp (expand_mit_def name args body)
  (*define*)
  | Pair(Symbol "define", Pair(name, sexpr)) -> tag_parse_define name sexpr
  (*set*)
  | Pair(Symbol "set!", Pair(name, Pair(sexpr, Nil))) -> Set((parse_exp name), (parse_exp sexpr))
  (*sequence*)
  | Pair(Symbol "begin", seq) -> tag_parse_seq_explicit seq
  (*applic*)
  | Pair(proc_sexpr, sexprs) ->  tag_parse_applic proc_sexpr sexprs
  | _ -> raise X_syntax_error

  and tag_parse_if test dit dif = match dif with
  | Nil -> If(parse_exp test, parse_exp dit, Const(Void))
  | Pair(sexpr, Nil) -> If (parse_exp test, parse_exp dit, parse_exp sexpr)
  |_ -> raise X_syntax_error

  and tag_parse_define name sexpr = match sexpr with
  | Nil ->  Def((parse_exp name), Const(Void))
  | Pair(a, Nil)-> Def((parse_exp name), (parse_exp a))
  | other -> raise X_syntax_error

  and tag_parse_lambda args body =
  let body_seq = (tag_parse_seq_implicit body) in
   match args with
      | Pair(car, cdr) -> let str_list = convert_pairs_to_str_list args in 
        if (check_arg_reoccurrence str_list) then raise X_syntax_error;
        if (is_proper_list args) then LambdaSimple(str_list, body_seq) else 
        let lst_without_last_element = remove_last_element str_list in 
        let last_element = get_last_element str_list in 
        LambdaOpt(lst_without_last_element, last_element, body_seq)
      | Symbol(str) -> LambdaOpt([], str, body_seq)
      | Nil -> LambdaSimple([], body_seq)
      | _ -> raise X_syntax_error

  and tag_parse_or bool_pairs = match bool_pairs with
  | Nil -> Const(Sexpr (Bool false))
  | Pair (a,Nil) -> Const(Sexpr (a))
  | Pair (a,b) -> let bool_list = List.map parse_exp (convert_pairs_to_list bool_pairs) in
      Or(bool_list)
  | _ -> raise X_syntax_error

  and tag_parse_seq_explicit seq = match seq with 
  | Nil -> Const Void
  | Pair(a, Nil) -> parse_exp a
  | Pair(a, b) -> let seq_expr = List.map parse_exp (convert_pairs_to_list seq) in Seq(seq_expr)
  | _ -> raise X_syntax_error
  
   and tag_parse_seq_implicit seq = match seq with 
  | Pair(a, Nil) -> parse_exp a
  | Pair(a, b) -> let seq_expr = List.map parse_exp (convert_pairs_to_list seq) in Seq(seq_expr)
  | _ -> raise X_syntax_error

  and tag_parse_applic proc_sexpr sexprs =   
    let proc_expr = parse_exp proc_sexpr in
    let exprs = 
    List.map parse_exp (convert_pairs_to_list sexprs) in
    (* [Const(Sexpr(sexprs))] in     *)
    Applic(proc_expr, exprs)

  and expand_quasiquote e = match e with
  | Pair(Symbol("unquote"), Pair(sexpr, Nil)) -> sexpr
  | Pair(Symbol("unquote-splicing"), Pair(sexpr, Nil)) -> raise X_syntax_error
  | Pair(Pair(Symbol("unquote-splicing"), Pair(sexpr, Nil)), x) -> Pair(Symbol("append"),Pair(sexpr,Pair((expand_quasiquote x),Nil)))
  | Pair(x, Pair(Symbol("unquote-splicing"), Pair(sexpr, Nil))) -> Pair(Symbol("cons"),Pair((expand_quasiquote x),Pair(sexpr,Nil)))
  | Pair(x, y) -> Pair(Symbol("cons"),Pair((expand_quasiquote x), Pair((expand_quasiquote y),Nil)))
  | Nil -> Pair(Symbol("quote"), Pair(Nil, Nil))
  | Symbol(x) -> Pair(Symbol("quote"), Pair(Symbol(x), Nil))
  | other -> other

  and expand_cond ribs = match ribs with
  (*3rd form*)
  | Pair(Pair(Symbol "else",seq), _) -> (Pair(Symbol("begin"),seq))
  (*2nd form*)
  | Pair(Pair(test, Pair(Symbol("=>"), Pair(expr_f, Nil))), Nil) -> 
      Pair(Symbol "let", Pair(Pair(Pair(Symbol "value", Pair(test, Nil)),
       Pair(Pair(Symbol "f", Pair(Pair(Symbol "lambda", Pair(Nil, Pair(expr_f, Nil))), Nil)), Nil)),
        Pair(Pair(Symbol "if", Pair(Symbol "value", Pair(Pair(Pair(Symbol "f", Nil),
         Pair(Symbol "value", Nil)), Nil))), Nil))) 

  | Pair(Pair(test, Pair(Symbol("=>"), Pair(expr_f, Nil))), rest) -> let expanded_ribs = (expand_cond rest) in
   Pair(Symbol "let", Pair(Pair(Pair(Symbol "value", Pair(test, Nil)), 
   Pair(Pair(Symbol "f", Pair(Pair(Symbol "lambda", Pair(Nil, Pair(expr_f, Nil))), Nil)),
    Pair(Pair(Symbol "rest", Pair(Pair(Symbol "lambda", Pair(Nil, Pair(expanded_ribs, Nil))), Nil)), Nil))),
     Pair(Pair(Symbol "if", Pair(Symbol "value", Pair(Pair(Pair(Symbol "f", Nil), Pair(Symbol "value", Nil)),
      Pair(Pair(Symbol "rest", Nil), Nil)))), Nil))) 
  (*1st form*)
  | Pair(Pair(test, seq), Nil) -> Pair(Symbol("if"), Pair(test, Pair(Pair(Symbol("begin"),seq), Nil)))
  | Pair(Pair(test, seq), rest) -> let expanded_ribs = (expand_cond rest) in
    (Pair(Symbol("if"), Pair(test , Pair(Pair(Symbol("begin"),seq) , Pair(expanded_ribs , Nil)))))
  | _ -> raise X_syntax_error  

  and expand_let ribs body = 
  let var_list = ribs_to_var_list ribs in 
  let val_list = ribs_to_val_list ribs in
  let lambda_sexpr = Pair(Symbol("lambda"), Pair(var_list, body)) in
  Pair(lambda_sexpr, val_list)

  and expand_let_star ribs body = 
    let rec handle_let_star_body rec_ribs rec_body vars vals =
          match rec_ribs with
        | Nil -> expand_let rec_ribs rec_body
        | Pair(rib, Nil) -> expand_let rec_ribs rec_body
        | Pair(rib, rest) ->
        (
          let first_var = Pair((get_first_pair vars),Nil) in 
          let first_val = Pair((get_first_pair vals),Nil) in
          let rest_vars = remove_first_pair vars in
          let rest_vals = remove_first_pair vals in
          let body = handle_let_star_body rest body rest_vars rest_vals in
          let lambda_sexpr = Pair(Pair(Symbol("lambda"), Pair(first_var, Pair(body, Nil))),first_val) in
          lambda_sexpr
      )
        | _ -> raise X_syntax_error in
    let var_list = ribs_to_var_list ribs in 
    let val_list = ribs_to_val_list ribs in
    handle_let_star_body ribs body var_list val_list

  and expand_letrec ribs body = match ribs with
  | Nil -> expand_let ribs body
  | Pair(rib, rest) -> 
    let new_ribs = change_to_whatever ribs in
    let set_body = wrap_ribs_in_set ribs body in
    (* let l_body = Pair(Symbol "let", Pair(Nil, body)) in *)
    let form = Pair(Symbol "let", Pair(new_ribs,set_body)) in
    (* let complete_form = Pair(Symbol "let", Pair(new_ribs,new_body)) in *)
    form
  | _ -> raise X_syntax_error

  and expand_and bool_pairs = match bool_pairs with
  | Nil -> Bool(true)
  | Pair(expr,Nil) -> expr
  | Pair(expr1, rest) -> let dit = (expand_and rest) in
    Pair(Symbol("if"), Pair(expr1, Pair(dit, Pair(Bool(false), Nil))))
  | _ -> raise X_syntax_error

  and expand_mit_def name args body = 
  let lambda_form = Pair(Symbol "lambda", Pair(args, body)) in
  let form = Pair(Symbol "define", Pair(name, Pair(lambda_form,Nil))) in form
;;



let tag_parse_expression sexpr = parse_exp sexpr;;

let tag_parse_expressions sexpr = List.map parse_exp sexpr;;

  
end;; (* struct Tag_Parser *)