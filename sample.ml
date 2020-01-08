#use "code-gen.ml";;
#use "compiler.ml";;

open Code_Gen;;
  (* type counter2 = { get : unit -> int;
                     incr : int -> unit };;

let rec make_consts_tbl_single_sexpr sexpr  = 
        let index =
          let n = ref (-1) in
          { get = (fun () -> !n);
            incr = (fun (add) -> n:= !n +add) } in

    match sexpr with
    | Void -> [(Void,(0, "MAKE_VOID"))]
    | Nil -> [(Sexpr(Nil),(1, "MAKE_NIL"))]
    | Bool(false) -> [(Sexpr(Bool(false)), (2, "MAKE_BOOL(0)"))]
    | Bool(true) -> [(Sexpr(Bool(true)), (4, "MAKE_BOOL(1)"))]
    | Number of number
    | Char of char
    | String of string
    | Symbol of string
    | Pair of sexpr * sexpr
    | TaggedSexpr of string * sexpr
    | TagRef of string
        
          ;;


  let make_consts_tbl asts =
      let rec make_consts_tbl_rec asts_rec =
        match asts_rec with
        |car::cdr -> let consts_list_rec = 
            (match car with
                |Const'(Sexpr(sexpr)) -> make_consts_tbl_single_sexpr sexpr
                |other -> []
            ) in 
            consts_list_rec @ make_consts_tbl_rec cdr
        |[] -> [] in
        make_consts_tbl_rec asts;; *)

let rec const_eq s1 s2 =
  match s1, s2 with
  | Sexpr(Bool(b1)), Sexpr(Bool(b2)) -> b1 = b2
  | Sexpr(Nil), Sexpr(Nil) -> true
  | Sexpr(Number(Float f1)), Sexpr(Number(Float f2)) -> abs_float(f1 -. f2) < 0.001
  | Sexpr(Number(Int n1)), Sexpr(Number(Int n2)) -> n1 = n2
  | Sexpr(Char(c1)), Sexpr(Char(c2)) -> c1 = c2
  | Sexpr(String(s1)), Sexpr(String(s2)) -> s1 = s2
  | Sexpr(Symbol(s1)), Sexpr(Symbol(s2)) -> s1 = s2
  | Sexpr(Pair(car1, cdr1)), Sexpr(Pair(car2, cdr2)) -> (const_eq (Sexpr car1) (Sexpr car2)) && (const_eq (Sexpr cdr1) (Sexpr cdr2))
  | Sexpr(TaggedSexpr(name1, expr1)), Sexpr(TaggedSexpr(name2, expr2)) -> (name1 = name2) && (const_eq (Sexpr expr1) (Sexpr expr2)) 
  | Sexpr(TagRef(name1)), Sexpr(TagRef(name2)) -> name1 = name2
  | Void, Void -> true
  | _ -> false;;


(*① Scan the AST (one recursive pass) & collect the sexprs in all Const records*)
let rec expr_to_sexpr_list expr = 
    let rec handle_expr_list expr_list = match expr_list with
        |[] -> []
        |car :: cdr -> let list_rec = expr_to_sexpr_list car in list_rec @ handle_expr_list cdr in

    match expr with
    | Const'(sexpr) -> [sexpr]
    | If'(test, dit, dif) -> (expr_to_sexpr_list test)@(expr_to_sexpr_list dit)@(expr_to_sexpr_list dif)
    | Seq'(expr_list) -> handle_expr_list expr_list
    | Set'(expr_var, expr_val) -> (expr_to_sexpr_list expr_var)@(expr_to_sexpr_list expr_val)
    | Def'(expr_var, expr_val) -> (expr_to_sexpr_list expr_var)@(expr_to_sexpr_list expr_val)
    | Or'(expr_list) -> handle_expr_list expr_list
    | Applic'(expr, expr_list) -> (expr_to_sexpr_list expr)@(handle_expr_list expr_list)
    | ApplicTP'(expr, expr_list) -> (expr_to_sexpr_list expr)@(handle_expr_list expr_list)
    | LambdaSimple'(arg_list, body) -> expr_to_sexpr_list body
    | LambdaOpt'(arg_list, opt_arg, body) -> expr_to_sexpr_list body
    | other -> [];;

let rec exprs_to_sexpr_list expr_list = match expr_list with
|[] -> []
|car :: cdr -> let sexpr_list = expr_to_sexpr_list car in sexpr_list @ exprs_to_sexpr_list cdr;;

(* ② Convert the list to a set (removing duplicates) *)
let rec remove_sexpr_duplicates sexpr sexpr_list =  
match sexpr_list with
|[] -> []
|car :: cdr -> if const_eq car sexpr
    then remove_sexpr_duplicates sexpr cdr
    else [car]@remove_sexpr_duplicates sexpr cdr;;

let rec convert_sexpr_list_to_set sexpr_list = match sexpr_list with
| [] -> []
|car :: cdr -> let list_rec = remove_sexpr_duplicates car cdr in [car]@(convert_sexpr_list_to_set list_rec)
;;

(* ③ Expand the list to include all sub-constants *)
let rec expand_sub_sexpr sexpr = match sexpr with
| Sexpr(Pair(car, cdr)) -> (expand_sub_sexpr (Sexpr car))@(expand_sub_sexpr (Sexpr cdr))@[Sexpr(Pair(car, cdr))]
| Sexpr(TaggedSexpr(name1, expr1)) -> (expand_sub_sexpr (Sexpr expr1))@[Sexpr(TaggedSexpr(name1, expr1))]
| Sexpr(Symbol(str)) -> (expand_sub_sexpr (Sexpr(String(str)))) @[Sexpr(Symbol(str))]
| other -> [other];;

let rec expand_sexpr_list sexpr_list = match sexpr_list with
|[] -> []
|car :: cdr -> let list_rec = expand_sub_sexpr car in list_rec@(expand_sexpr_list cdr);;

(* ⑤ Go over the list, from first to last, and create the
constants-table: *)
(* ① For each sexpr in the list, create a 3-tuple: *)
type counter2 = { get : int -> int;};;
let rec make_consts_tbl_single_sexpr sexpr acc_const_table index = 
        let get_string_size str = 9 + (String.length str) in
        let get_sexpr_address sexpr = 
            (string_of_int (fst (List.assoc sexpr acc_const_table))) in
    match sexpr with
    | Void -> [(Void,(0, "MAKE_VOID"))]
    | Sexpr(Nil) -> Printf.printf "index: %d\n" (index.get (0));[(sexpr,(1, "MAKE_NIL"))]
    | Sexpr(Bool(false)) -> [(sexpr, (2, "MAKE_BOOL(0)"))]
    | Sexpr(Bool(true)) -> [(sexpr, (4, "MAKE_BOOL(1)"))]
    | Sexpr(Number(Float f)) -> [(sexpr, (index.get (9), "MAKE_LITERAL_FLOAT("^(string_of_float f)^")"))]
    | Sexpr(Number(Int n)) -> Printf.printf "index: %d\n" (index.get (0)); [(sexpr, (index.get (9), "MAKE_LITERAL_INTEGER("^(string_of_int n)^")"))]    
    | Sexpr(Char(char)) -> [(sexpr, (index.get (2), "MAKE_LITERAL_CHAR("^(Char.escaped char)^")"))]   
    | Sexpr(String(str)) -> [(sexpr, (index.get (get_string_size str), "MAKE_LITERAL_STRING(\""^str^"\")"))]   
    | Sexpr(Symbol(str)) -> [(sexpr, (index.get (9), "MAKE_LITERAL_SYMBOL(consts+"^(get_sexpr_address (Sexpr(String(str))))^")"))]  
    | Sexpr(Pair(sexpr1, sexpr2)) -> Printf.printf "index: %d\n" (index.get (0)); [(sexpr, (index.get (17), "MAKE_LITERAL_PAIR(consts+"^(get_sexpr_address (Sexpr(sexpr1)))^", consts+"^(get_sexpr_address (Sexpr(sexpr2)))^")"))]
    | Sexpr(TaggedSexpr(str, sexpr1)) -> []
    | Sexpr(TagRef(str)) -> []
          ;;
let rec make_consts_table sexpr_list const_table = 
        let index =
          let n = ref 5 in
          { get = (fun (add) -> let old_value = !n in n:= !n +add; old_value);
          } in 

match sexpr_list with
|[] -> []
|car :: cdr -> let list_rec = make_consts_tbl_single_sexpr car const_table index in list_rec @ (make_consts_table cdr list_rec);;




let asts = string_to_asts "(lambda (a) '(5 5))";;

let sexpr_list = convert_sexpr_list_to_set 
                    (expand_sexpr_list 
                        (convert_sexpr_list_to_set 
                            (exprs_to_sexpr_list asts)));;

make_consts_table sexpr_list [];;