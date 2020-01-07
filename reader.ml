#use "pc.ml";;
open PC;;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Int of int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr
  | TaggedSexpr of string * sexpr
  | TagRef of string;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Int n1), Number(Int n2) -> n1 = n2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | TaggedSexpr(name1, expr1), TaggedSexpr(name2, expr2) -> (name1 = name2) && (sexpr_eq expr1 expr2) 
  | TagRef(name1), TagRef(name2) -> name1 = name2
  | _ -> false;;
  
module Reader 
: sig
  val read_sexpr : string -> sexpr
  val read_sexprs : string -> sexpr list
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;
  
(*abstract parsers*)
(* abstract parser that skips nt_right and nt_left results from left and right of nt *)
let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (function (_, e) -> e) in
  let nt = caten nt nt_right in
  let nt = pack nt (function (e, _) -> e) in
  nt;;

(* 3.2.1 Whitespaces *)
let nt_whitespace = const (fun ch -> ch <= ' ');;
(* parser that skips whitesapes from left and right of nt *)
let make_spaced nt = make_paired (star nt_whitespace) (star nt_whitespace) nt;;

(* 3.2.2 Line comments *)
let nt_comment_line = 
    let nt_semicolon = char ';' in
    let nt_eol = char (char_of_int 10) in
    let nt_all_but_eol = diff nt_any nt_eol in
    let nt_end_of_comment = disj nt_eol (pack nt_end_of_input (fun (dummy) -> 'd')) in
    let nt = caten nt_semicolon (star nt_all_but_eol) in
    let nt = caten nt nt_end_of_comment in
    let nt = pack nt (fun e -> Nil) in
    make_spaced nt;;
    

(*3.3.1 Boolean*)
let nt_boolean =
    let nt_hashtag = char '#' in
    let nt_f = word_ci "f" in
    let nt_t = word_ci "t" in
    let nt_false = pack nt_f (fun f -> false) in
    let nt_true = pack nt_t (fun t -> true) in
    let nt = disj nt_false nt_true in
    let nt = caten nt_hashtag nt in
    (* let nt = pack nt (function (_, false) ->                     |TaggedSexpr(some_name, some_sexpr) -> if (name == some_name) && sexpr_eq some_sexpr sexpr
(Bool false)) in  *)
    let nt = pack nt (function (_0x, e) -> (Bool e)) in
nt;;

(*3.3.3 Symbol*)
let digit = range '0' '9';;

let nt_symbol =
    let lowercase_letters = range 'a' 'z' in
    let uppercase_letters = range 'A' 'Z' in
    let punctuation = disj_list [char ':';char '!'; char '$'; char '^'; char '*'; char '-'; char '_'; char '='; char '+'; char '<'; char '>'; char '/'; char '?'] in    
    let norm_uppercase = pack uppercase_letters lowercase_ascii in
    let nt = disj_list [lowercase_letters; norm_uppercase; punctuation; digit] in
    let nt = plus nt in
    let nt = pack nt (fun e -> let str = list_to_string e in Symbol(str)) in
    nt;;

(*3.3.2 Number*)
let not_digit = const (fun ch -> ch < '0' || ch > '9');;

(*int*)
let nt_int = 
    let nt_body = pack (not_followed_by (plus digit) (char '.')) (function e -> int_of_string ((list_to_string e))) in
    let nt_plus_op = char '+' in
    let nt_minus_op = char '-' in
    let nt_op = disj nt_minus_op nt_plus_op in
    let nt_signed = pack (caten nt_op nt_body) 
    (function (op,num) -> if (op = '-') then (-1)*(num) else num) in
    let nt = disj nt_signed nt_body in
    nt;;


let nt_int_packed = 
    let nt = not_followed_by nt_int nt_symbol in
    pack nt (fun e -> Int(e));;
 
(*float*)
let nt_float =
    let nt_dot = char '.' in
    let nt_form = caten (plus digit) (caten nt_dot (plus digit)) in
    let nt_body = pack nt_form 
    (function (a,(b, c)) -> float_of_string ((list_to_string a) ^ "." ^ (list_to_string c))) in
    let nt_plus_op = char '+' in
    let nt_minus_op = char '-' in
    let nt_op = disj nt_minus_op nt_plus_op in
    let nt_signed = pack (caten nt_op nt_body) 
    (function (op,num) -> if (op = '-') then (-1.0)*.(num) else num) in
    let nt = disj nt_signed nt_body in
    nt;;
    (*number*)

let nt_float_packed = 
    let nt_symbols_not_e = diff nt_symbol (word_ci "e") in
    let nt = not_followed_by nt_float nt_symbols_not_e in
    pack nt (fun e -> Float(e));;
    
    (*number*)

(*4.1 Scientific notation*)
let nt_scientific_notation = 
    let nt_int_to_float = pack nt_int (fun e -> float_of_int e) in
    let nt = disj nt_int_to_float nt_float in
    let nt_e = word_ci "e" in
    let nt = caten nt (caten nt_e nt_int_to_float) in
    (* let nt = not_followed_by nt nt_symbol in *)
    let nt = pack nt (fun (num, (e, exp)) -> let n = num *. (10. ** exp) in Float(n)) in
    nt;;


let make_nt_digit ch_from ch_to displacement =
    let nt = const (fun ch -> ch_from <= ch && ch <= ch_to) in
    let nt = pack nt (let delta = (Char.code ch_from) - displacement in
		      fun ch -> (Char.code ch) - delta) in
    nt;;

let nt_radix_cal radix =
  let nt = disj (make_nt_digit '0' '9' 0)
		(make_nt_digit 'a' 'z' 10) in
  let nt = disj nt (make_nt_digit 'A' 'Z' 10) in
  let nt = plus nt in
  let nt = pack nt (fun digits ->
		    List.fold_left (fun a b -> radix * a + b) 0 digits) in
  nt;;

let nt_radix_cal_float radix =
  let nt = disj (make_nt_digit '0' '9' 0)
		(make_nt_digit 'a' 'z' 10) in
  let nt = disj nt (make_nt_digit 'A' 'Z' 10) in
  let nt = pack nt (fun e -> float_of_int e) in
  let nt = plus nt in
  (* let nt = pack nt (fun digits -> (1./.(float_of_int radix))) in   *)
  let nt = pack nt (fun digits -> List.fold_right (fun a b -> (1./.(float_of_int radix)) *. (b +. a)) digits 0.) in  
  nt;;

let nt_radix_range = disj_list [digit; range 'a' 'z'; range 'A' 'Z'] ;;

let nt_radix_identifier = 
    let nt_hashtag = char '#' in
    let nt_r = disj (char 'r') (char 'R') in
    let body_until_r = caten nt_hashtag (caten nt_int nt_r) in

    let nt_plus_op = char '+' in
    let nt_minus_op = char '-' in
    let nt_op = disj nt_minus_op nt_plus_op in
    let nt_signed = caten body_until_r nt_op in 
    let nt_signed = pack nt_signed (fun ((_hash, (base,r)), op) -> (op,(_hash, (base,r)))) in

    let nt = disj nt_signed (pack body_until_r (fun (_hash, (base,r)) -> ('+',(_hash, (base,r))))) in
    let nt = pack nt (fun (op, (_hash, (base,_r))) -> (op, base)) in
    nt;;

let nt_int_radix = 
    let nt = not_followed_by (plus nt_radix_range) (char '.') in
    let nt = caten nt_radix_identifier nt in
    let nt = pack nt (fun ((op,base), num) -> (op, (nt_radix_cal base num))) in
    let nt = pack nt (fun (op, (num,_)) -> if (op = '-') then (-1)*(num) else num) in
    let nt = pack nt (fun (num) -> Int(num)) in
    nt;;

let nt_float_radix = 
    let nt_form = caten (plus nt_radix_range) (caten (char '.') (plus nt_radix_range)) in
    let nt = pack nt_form (fun (a, (_, b)) -> (a,b)) in
    let nt = caten nt_radix_identifier nt in
    let nt = pack nt (fun ((op,base), (left, right)) -> 
    let converted_left = nt_radix_cal base left  in
    let converted_right = nt_radix_cal_float base right in
    (op,(converted_left, converted_right))) in
    let nt = pack nt (fun (op,((e1,_),(e2,_))) -> 
        let converted = (float_of_int e1) +. e2 in
        (op, converted)) in
    let nt = pack nt (fun (op, num) -> if (op = '-') then (-1.)*.(num) else num) in
    let nt = pack nt (fun num -> Float(num)) in
    nt;;

let nt_number = 
    let nt = disj_list [nt_float_radix;nt_int_radix;nt_scientific_notation;nt_float_packed; nt_int_packed] in
    let nt = pack nt (function e -> Number(e)) in
    nt;;

(*3.3.4 String*)
let nt_string = 
    let string_literal_char = diff nt_any (disj (char (char_of_int 34)) (char (char_of_int 92))) in
    let string_meta_char = disj_list [
        pack (word "\\\\") (fun e -> char_of_int 92);
        pack (word "\\\"") (fun e -> char_of_int 34);
        pack (word "\\t") (fun e -> char_of_int 9);
        pack (word "\\f") (fun e -> char_of_int 12);
        pack (word "\\n") (fun e -> char_of_int 10);
        pack (word "\\r") (fun e -> char_of_int 13)
        ] in
    let nt_body = disj string_literal_char string_meta_char in
    let nt_double_quote = char (char_of_int (34)) in
    let nt = caten nt_double_quote (caten (star nt_body) nt_double_quote) in
    let nt = pack nt (fun (_, (e, _)) -> String(list_to_string e)) in
    nt;;

(*3.3.5 Char*)
let nt_char =
    let char_prefix = caten (char '#') (char '\\') in
    let visible_simple_char = const (fun ch -> (int_of_char ch) > 32) in
    let named_char = disj_list [
        pack (word_ci "nul") (fun e -> char_of_int 0);
        pack (word_ci "newline") (fun e -> char_of_int 10);
        pack (word_ci "return") (fun e -> char_of_int 13);
        pack (word_ci "tab") (fun e -> char_of_int 9);
        pack (word_ci "page") (fun e -> char_of_int 12);
        pack (word_ci "space") (fun e -> char_of_int 32)
        ] in
    let nt = disj named_char visible_simple_char in
    let nt = pack (caten char_prefix nt) (fun (_, e) -> Char(e)) in
    nt;;

let nt_teg_ref_identifier= 
    let prefix = word "#{" in
    let postfix = word "}" in
    let symbol_name = pack nt_symbol (
        function e -> match e with
        | Symbol(name) -> name
        | other_sexpr -> raise X_no_match ) in
    let nt = caten prefix (caten (symbol_name) postfix) in
    let nt = pack nt (fun (_, (name, _)) -> name) in
    nt;;

let rec fun1 acc_list original_list = 
    match original_list with
    |Pair(sexpr1, sexpr2) -> let a = (List.append (fun1 acc_list sexpr1) acc_list) in let b = (List.append (fun1 acc_list sexpr2) acc_list) in List.append a b
    |TaggedSexpr(name, sexpr) -> List.append [TaggedSexpr(name, sexpr)] acc_list
    |other -> acc_list

let fun2 lst = 
    let rec same_tag_exists_in_list compare_name tag_list = match tag_list with
        | TaggedSexpr(name, sexpr) :: rest_of_list -> if name = compare_name then true else same_tag_exists_in_list compare_name rest_of_list 
        | other -> false in

    let rec choose_tag tag_list =
        match tag_list with
        | TaggedSexpr(name, sexpr) :: rest_of_list -> if (same_tag_exists_in_list name rest_of_list) then false else choose_tag rest_of_list
        | other -> true in

        choose_tag lst;;


let check_if_valid_list paired_list =
let lst = [] in
let tag_list = fun1 lst paired_list in
let is_valid = fun2 tag_list in
         if is_valid then paired_list else raise X_this_should_not_happen;;
(* sexp *)

let rec nt_sexpr str = 
    let sexpr_disj = disj_list [
            nt_boolean;
            nt_char;
            nt_number;
            nt_string;
            nt_symbol;
            nt_nil;
            nt_list;
            nt_dotted_list;
            nt_quote;
            nt_quasi_quote;
            nt_unquote;
            nt_unquote_and_splice;
            nt_tagged_sexpr;
            nt_tag_ref
           ] in
    (make_spaced_or_commented sexpr_disj) str
    and nt_list s = 
        let prefix = char '(' in
        let postfix = char ')' in
        let body = star nt_sexpr in
        let nt = caten prefix (caten body postfix) in
        let nt = pack nt (
            function (_,(e,_)) -> match e with
            |[] -> Nil
            |lst -> List.fold_right (fun sexpr1 sexpr2 -> Pair(sexpr1, sexpr2)) lst Nil
        ) in
        let nt = (pack nt check_if_valid_list) in
         nt s
    and nt_dotted_list s = 
        let prefix = char '(' in
        let postfix = char ')' in
        let nt_dot = char '.' in
        let body = caten (plus nt_sexpr) (caten nt_dot nt_sexpr) in
        let nt = caten prefix (caten body postfix) in
        let nt = pack nt (
            function (_,(e,_)) -> match e with
            |(a, (_, b)) -> List.fold_right (fun sexpr1 sexpr2 -> Pair(sexpr1, sexpr2)) a b
        )in
        let nt = (pack nt check_if_valid_list) in
            nt s
    and nt_quote s = 
        let prefix = word "'" in
        let nt = caten prefix nt_sexpr in
        pack nt (function (_, e) -> Pair(Symbol("quote"), Pair(e, Nil)))
        s
    and nt_quasi_quote s = 
        let prefix = word "`" in
        let nt = caten prefix nt_sexpr in
        pack nt (function (_, e) -> Pair(Symbol("quasiquote"), Pair(e, Nil)))
        s
    and nt_unquote s = 
        let prefix = word "," in
        let nt = caten prefix nt_sexpr in
        pack nt (function (_, e) -> Pair(Symbol("unquote"), Pair(e, Nil)))
        s
    and nt_unquote_and_splice s = 
        let prefix = word ",@" in
        let nt = caten prefix nt_sexpr in
        pack nt (function (_, e) -> Pair(Symbol("unquote-splicing"), Pair(e, Nil)))
        s
    and nt_tag_ref s =
        let nt = pack nt_teg_ref_identifier (fun name -> TagRef(name)) in
        nt s
    and nt_tagged_sexpr s =
        let eq_sign = word "=" in
        let nt = caten nt_teg_ref_identifier eq_sign in
        let nt = caten nt nt_sexpr in
        let nt = pack nt (fun ((name,_),sexpr) -> TaggedSexpr(name, sexpr)) in
        let nt = pack nt (fun e -> 
            let rec check_if_valid original_name rec_sexpr = match rec_sexpr with
                |TaggedSexpr(some_name, some_sexpr) -> 
                    if some_name = original_name 
                        then false
                        else (check_if_valid original_name some_sexpr)
                |Pair(sexpr1, sexpr2) -> 
                    (check_if_valid original_name sexpr1) &&
                    (check_if_valid original_name sexpr2) 
                |other -> true in

            match e with
            | TaggedSexpr(name, sexpr) -> let valid = check_if_valid name sexpr in
            if valid then TaggedSexpr(name, sexpr) else raise X_this_should_not_happen
            | other -> raise X_no_match ) in
            nt s
    and nt_comment_sexpr s =
        let prefix = word "#;" in
        let body = nt_sexpr in
        let nt = caten prefix body in
        (pack nt (fun e -> Nil)) 
        s
    and nt_nil s = 
        let prefix = char '(' in
        let postfix = char ')' in
        let body = disj_list [nt_comment_line; (pack nt_whitespace (fun e -> Nil));nt_comment_sexpr] in
        let nt = caten prefix (caten (star body) postfix) in
        let nt = pack nt (fun e -> Nil) in
        nt s
    and make_spaced_or_commented s = 
        (* let nt_not_last_comment_sexpr = not_followed_by (pack nt_comment_sexpr (fun e -> Nil)) (pack (nt_end_of_input) (fun e -> Nil)) in *)
        let whitespace_or_comment = disj_list [(pack nt_whitespace (fun e -> Nil));nt_comment_line;nt_comment_sexpr] in
        let nt1 nt = make_paired (star whitespace_or_comment) (star whitespace_or_comment) nt in
        nt1 s;;


let read_sexpr string =
  let (sexpr, s) = (nt_sexpr (string_to_list string)) in
  if (s = [])
  then sexpr
  else raise X_no_match;;


let read_sexprs string =
    let (sexpr_list, s) = ((star nt_sexpr) (string_to_list string)) in
    sexpr_list;;

end;; (* struct Reader *)
