#use "tag-parser.ml";;

type var = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | Const' of constant
  | Var' of var
  | Box' of var
  | BoxGet' of var
  | BoxSet' of var * expr'
  | If' of expr' * expr' * expr'
  | Seq' of expr' list
  | Set' of expr' * expr'
  | Def' of expr' * expr'
  | Or' of expr' list
  | LambdaSimple' of string list * expr'
  | LambdaOpt' of string list * string * expr'
  | Applic' of expr' * (expr' list)
  | ApplicTP' of expr' * (expr' list);;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | Const' Void, Const' Void -> true
  | Const'(Sexpr s1), Const'(Sexpr s2) -> sexpr_eq s1 s2
  | Var'(VarFree v1), Var'(VarFree v2) -> String.equal v1 v2
  | Var'(VarParam (v1,mn1)), Var'(VarParam (v2,mn2)) -> String.equal v1 v2 && mn1 = mn2
  | Var'(VarBound (v1,mj1,mn1)), Var'(VarBound (v2,mj2,mn2)) -> String.equal v1 v2 && mj1 = mj2  && mn1 = mn2
  | If'(t1, th1, el1), If'(t2, th2, el2) -> (expr'_eq t1 t2) &&
                                            (expr'_eq th1 th2) &&
                                              (expr'_eq el1 el2)
  | (Seq'(l1), Seq'(l2)
  | Or'(l1), Or'(l2)) -> List.for_all2 expr'_eq l1 l2
  | (Set'(var1, val1), Set'(var2, val2)
  | Def'(var1, val1), Def'(var2, val2)) -> (expr'_eq var1 var2) &&
                                             (expr'_eq val1 val2)
  | LambdaSimple'(vars1, body1), LambdaSimple'(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr'_eq body1 body2)
  | LambdaOpt'(vars1, var1, body1), LambdaOpt'(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr'_eq body1 body2)
  | Applic'(e1, args1), Applic'(e2, args2)
  | ApplicTP'(e1, args1), ApplicTP'(e2, args2) ->
	 (expr'_eq e1 e2) &&
	   (List.for_all2 expr'_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;

module type SEMANTICS = 
sig
  val run_semantics : expr -> expr'
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
end;;

module Semantics 
: SEMANTICS 
= struct
      
let rec annotate_bound_major string_var arg_lists major_level = 
  let rec annotate_bound_minor string_var arg_list minor_level = match arg_list with
  | car :: cdr -> if String.equal car string_var then Some(minor_level) else annotate_bound_minor string_var cdr (minor_level + 1)
  | _ -> None in

match arg_lists with
| [arg_list :: cdr] -> let minor = annotate_bound_minor string_var arg_list 0 in (match minor with 
   | Some(minor_level) -> if (major_level < 0) then Var'(VarParam(string_var, minor_level)) else Var'(VarBound(string_var, major_level, minor_level))
   | None -> annotate_bound_major string_var [cdr] (major_level + 1))
| _ -> Var'(VarFree(string_var))
 ;;

let rec annotate_lexical_rec e = match e with
  | Const(constant) -> Const'(constant)
  | Var(string) -> Var'(VarFree(string))
  | If(test, dit, dif) -> If'(annotate_lexical_rec test, annotate_lexical_rec dit, annotate_lexical_rec dif)
  | Seq(expr_list) -> Seq'(List.map annotate_lexical_rec expr_list)
  | Set(expr_var, expr_val) -> Set'(annotate_lexical_rec expr_var,annotate_lexical_rec expr_val)
  | Def(expr_var, expr_val) -> Def'(annotate_lexical_rec expr_var,annotate_lexical_rec expr_val)
  | Or(expr_list) -> Or'(List.map annotate_lexical_rec expr_list)
  | Applic(expr, expr_list) -> Applic'(annotate_lexical_rec expr,List.map annotate_lexical_rec expr_list)
  | LambdaSimple(arg_list, body) -> let annotated_body = (annotate_lambda_simple [arg_list] body) in LambdaSimple'(arg_list,annotated_body)
  | LambdaOpt(arg_list, opt_arg, body) -> let annotated_body = annotate_lambda_simple [arg_list@[opt_arg]] body in 
  LambdaOpt'(arg_list, opt_arg, annotated_body)

  and annotate_lambda_simple arg_lists body = match body with
  | Const(constant) -> Const'(constant)
  | Var(str) -> annotate_bound_major str [arg_lists] (-1)
  | If(test, dit, dif) -> If'(annotate_lambda_simple arg_lists test, annotate_lambda_simple arg_lists dit, annotate_lambda_simple arg_lists dif)
  | Seq(expr_list) -> Seq'(List.map (annotate_lambda_simple arg_lists) expr_list)
  | Set(expr_var, expr_val) -> Set'(annotate_lambda_simple arg_lists expr_var,annotate_lambda_simple arg_lists expr_val)
  | Def(expr_var, expr_val) -> Def'(annotate_lambda_simple arg_lists expr_var,annotate_lambda_simple arg_lists expr_val)
  | Or(expr_list) -> Or'(List.map (annotate_lambda_simple arg_lists) expr_list)
  | Applic(expr, expr_list) -> Applic'(annotate_lambda_simple arg_lists expr,List.map (annotate_lambda_simple arg_lists) expr_list)
  | LambdaSimple(arg_list, inner_body) -> let annotated_body = annotate_lambda_simple ([arg_list]@arg_lists) inner_body in LambdaSimple'(arg_list,annotated_body)
  | LambdaOpt(arg_list, opt_arg, inner_body) -> let annotated_body = annotate_lambda_simple ([arg_list @ [opt_arg]]@arg_lists) inner_body 
  in  LambdaOpt'(arg_list, opt_arg, annotated_body)
  ;;

let rec annotate_tail_rec in_tp e = match e with
  | If'(test, dit, dif) -> If'(annotate_tail_rec false test, annotate_tail_rec in_tp dit, annotate_tail_rec in_tp dif)
  | Seq'(expr_list) -> annotate_seq in_tp expr_list
  | Set'(expr_var, expr_val) -> Set'(annotate_tail_rec false expr_var,annotate_tail_rec false expr_val)
  | Def'(expr_var, expr_val) -> Def'(annotate_tail_rec false expr_var,annotate_tail_rec false expr_val)
  | Or'(expr_list) -> annotate_or in_tp expr_list
  | Applic'(expr, expr_list) -> if in_tp then ApplicTP'(annotate_tail_rec false expr,List.map (annotate_tail_rec false) expr_list) else Applic'(annotate_tail_rec false expr,List.map (annotate_tail_rec false) expr_list)
  | LambdaSimple'(arg_list, body) -> LambdaSimple'(arg_list, annotate_tail_rec true body)
  | LambdaOpt'(arg_list, opt_arg, body) -> LambdaOpt'(arg_list, opt_arg, annotate_tail_rec true body)
  | other -> other

and annotate_seq in_tp expr_list = (match (List.rev expr_list) with 
    | car :: cdr -> Seq'((List.map (annotate_tail_rec false) (List.rev cdr)) @ [(annotate_tail_rec in_tp car)])
    | _ -> Seq'((List.map (annotate_tail_rec false) expr_list)))

and annotate_or in_tp expr_list = (match (List.rev expr_list) with 
    | car :: cdr -> Or'((List.map (annotate_tail_rec false) (List.rev cdr)) @ [(annotate_tail_rec in_tp car)])
    | _ -> Or'((List.map (annotate_tail_rec false) expr_list)))


let rec append_list l1 l2 =
    match l1 with
    | [] -> l2
    | h::t -> if List.mem h l2 then append_list t l2
              else append_list t (h::l2);;

let append_inner_lists l1 l2 = match l1, l2 with
| left1::[right1], left2::[right2] -> [append_list left1 left2;append_list right1 right2]
|(l1, []) -> l1
|([], l2) -> l2
| other -> [];;

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l;;

  type counter = { get : unit -> int;
                     incr : unit -> unit };;

let find_fathers minor arg body = 

  let father_id =
    let n = ref 0 in
    { get = (fun () -> !n);
      incr = (fun () -> n:= !n +1) } in

  let rec find_rw_fathers level arg body = 
    (* Printf.printf "father %d level %d arg %s\n" (father_id.get ()) level arg; *)
    match body with
    | Var'(VarParam(str, minor)) -> if level = (-1) 
      then if String.equal str arg then [[-1];[]] else [[];[]]
      else [[];[]]

    | Var'(VarBound(str, major, minor)) -> if level = major
      then if String.equal str arg then [[(father_id.get ())];[]] else [[];[]]
      else [[];[]]

    | Set'(var, expr) -> 
      (match var, expr with
      | Var'(VarParam(str1, minor1)), Var'(VarParam(str2, minor2)) -> if level = (-1)
        then if String.equal str1 arg 
            then if String.equal str2 arg then [[-1];[-1]] else [[];[-1]]
            else if String.equal str2 arg then [[-1];[]] else [[];[]]
        else [[];[]]
      | Var'(VarBound(str1, major1, minor1)), Var'(VarBound(str2, major2, minor2)) -> 
        if level = major1
              (*write matches level*)
              then if String.equal str1 arg 
                    (*write matches name*)
                    then if level = major2 
                        (*read matches level*)
                        then if String.equal str2 arg then [[(father_id.get ())];[(father_id.get ())]] else [[];[(father_id.get ())]]
                        (*read doesn't matches level*)
                        else [[];[(father_id.get ())]]
                    (*write doesn't name*)
                    else if level = major2 
                        (*read matches level*)
                        then if String.equal str2 arg then [[(father_id.get ())];[]] else [[];[]]
                        (*read doesn't matches level*)
                        else [[];[]]
              (*write doesn't matches level*)
              else if level = major2 
                        (*read matches level*)
                        then if String.equal str2 arg then [[(father_id.get ())];[]] else [[];[]]
                        (*read doesn't matches level*)
                        else [[];[]]
      | Var'(VarBound(str1, major, minor1)), Var'(VarParam(str2, minor2)) ->  if level = major
            (*write matches level*)
            then if String.equal str1 arg 
                  (*write matches name*)
                  then [[];[(father_id.get ())]]
                  (*write doesn't match name*)
                  else [[];[]]
            (*write doesn't matches level*)
            else [[];[]]
      | Var'(VarParam(str1, minor1)), Var'(VarBound(str2, major, minor2)) -> if level = major
            (*read matches level*)
            then if String.equal str2 arg 
                  (*read matches name*)
                  then [[(father_id.get ())];[]]
                  (*read doesn't match name*)
                  else [[];[]]
            (*read doesn't matches level*)
            else [[];[]]
      | Var'(VarParam(str, minor)), not_var -> if level = -1
        (*write matches level*)
        then if String.equal str arg 
              (*write matches name*)
              then let result = find_rw_fathers level arg not_var in match result with
              | read_list::[write_list] -> [read_list;[-1]@write_list]
              | other -> other
              (*write doesn't matches name*)
              else find_rw_fathers level arg not_var
        (*write doesn't matches level*)
        else find_rw_fathers level arg not_var
      | Var'(VarBound(str, major,minor)), not_var -> 
      if level = major
        (*write matches level*)
        then if String.equal str arg 
              (*write matches name*)
              then let result = find_rw_fathers level arg not_var in  match result with
              | read_list::[write_list] -> [read_list;[(father_id.get ())]@write_list]
              | other -> other
              (*write doesn't matches name*)
              else find_rw_fathers level arg not_var
        (*write doesn't matches level*)
        else find_rw_fathers level arg not_var
      | Var'(VarFree(str)), expr -> find_rw_fathers level arg expr
      | other -> [[];[]]
      )

    | If'(test, dit, dif) -> 
      let fathers_test = find_rw_fathers level arg test in
      let fathers_dit = find_rw_fathers level arg dit in
      let fathers_dif = find_rw_fathers level arg dif in
      (match fathers_test, fathers_dit, fathers_dif with
      | [read_test;write_test],[read_dit;write_dit],[read_dif;write_dif] ->  [read_test@read_dit@read_dif;write_test@write_dit@write_dif]
      | other -> [[];[]])

    | Seq'(expr_list) -> 
      let expr_list_rec = List.map (find_rw_fathers level arg) expr_list in
      List.fold_right append_inner_lists expr_list_rec []
    | Or'(expr_list) -> 
      let expr_list_rec = List.map (find_rw_fathers level arg) expr_list in
      List.fold_right append_inner_lists expr_list_rec []
    | Applic'(expr, expr_list) -> 
      let expr_rec = find_rw_fathers level arg expr in
      let expr_list_rec = List.map (find_rw_fathers level arg) expr_list in
      let expr_list_appended = List.fold_right append_inner_lists expr_list_rec [] in
      append_inner_lists expr_rec expr_list_appended
    | ApplicTP'(expr, expr_list) -> 
      let expr_rec = find_rw_fathers level arg expr in
      let expr_list_rec = List.map (find_rw_fathers level arg) expr_list in
      let expr_list_appended = List.fold_right append_inner_lists expr_list_rec [] in
      append_inner_lists expr_rec expr_list_appended
    | LambdaSimple'(arg_list, inner_body) -> if level = (-1) then father_id.incr (); find_rw_fathers (level + 1) arg inner_body
    | LambdaOpt'(arg_list, opt_arg, inner_body) -> if level = (-1) then father_id.incr (); find_rw_fathers (level + 1) arg inner_body
    | Def'(expr_var, expr_val) -> raise X_syntax_error
    | other -> [[];[]]  in

  find_rw_fathers (-1) arg body;;

  
let rec box_get_set_body level arg body = 
match body with
    | Var'(VarParam(str, minor)) -> if level = (-1) 
      then if String.equal str arg then BoxGet'(VarParam(str, minor)) else body
      else body

    | Var'(VarBound(str, major, minor)) -> 
    if level = major
      then if String.equal str arg then BoxGet'(VarBound(str, major, minor)) else body
      else body

    | Set'(var, expr) -> 
      (match var, expr with
      | Var'(VarParam(str1, minor1)), Var'(VarParam(str2, minor2)) -> if level = (-1)
        then if String.equal str1 arg 
            then if String.equal str2 arg then BoxSet'(VarParam(str1, minor1), BoxGet'(VarParam(str2, minor2))) else BoxSet'(VarParam(str1, minor1), expr)
            else if String.equal str2 arg then Set'(var, BoxGet'(VarParam(str2, minor2))) else body
        else body
      | Var'(VarBound(str1, major1, minor1)), Var'(VarBound(str2, major2, minor2)) -> 
        if level = major1
              (*write matches level*)
              then if String.equal str1 arg 
                    (*write matches name*)
                    then if level = major2 
                        (*read matches level*)
                        then if String.equal str2 arg then BoxSet'(VarBound(str1, major1, minor1), BoxGet'(VarBound(str2, major2, minor2))) else BoxSet'(VarBound(str1, major1, minor1), expr)
                        (*read doesn't matches level*)
                        else BoxSet'(VarBound(str1, major1, minor1), expr)
                    (*write doesn't match name*)
                    else if level = major2 
                        (*read matches level*)
                        then if String.equal str2 arg then Set'(var, BoxGet'(VarBound(str2, major2, minor2))) else body
                        (*read doesn't matches level*)
                        else body
              (*write doesn't matches level*)
              else if level = major2 
                        (*read matches level*)
                        then if String.equal str2 arg then Set'(var, BoxGet'(VarBound(str2, major2, minor2))) else body
                        (*read doesn't matches level*)
                        else body
      | Var'(VarBound(str1, major, minor1)), Var'(VarParam(str2, minor2)) ->  if level = major
            (*write matches level*)
            then if String.equal str1 arg 
                  (*write matches name*)
                  then BoxSet'(VarBound(str1, major, minor1), expr)
                  (*write doesn't match name*)
                  else body
            (*write doesn't matches level*)
            else body
      | Var'(VarParam(str1, minor1)), Var'(VarBound(str2, major, minor2)) -> if level = major
            (*read matches level*)
            then if String.equal str2 arg 
                  (*read matches name*)
                  then Set'(var, BoxGet'(VarBound(str2, major, minor2)))
                  (*read doesn't match name*)
                  else body
            (*read doesn't matches level*)
            else body
      | Var'(VarParam(str, minor)), not_var -> 
        let result_body = box_get_set_body level arg not_var in
        if level = -1
        (*write matches level*)
        then if String.equal str arg 
              (*write matches name*)
              then BoxSet'(VarParam(str, minor), result_body)
              (*write doesn't matches name*)
              else Set'(var, result_body)
        (*write doesn't matches level*)
        else Set'(var, result_body)
      | Var'(VarBound(str, major,minor)), not_var -> 
        let result_body = box_get_set_body level arg not_var in
        if level = major
        (*write matches level*)
        then if String.equal str arg 
              (*write matches name*)
              then BoxSet'(VarBound(str, major,minor), result_body)
              (*write doesn't matches name*)
              else Set'(var, result_body)
        (*write doesn't matches level*)
        else Set'(var, result_body)
     
      | Var'(VarFree(str)), expr -> Set'(Var'(VarFree(str)), box_get_set_body level arg expr)
      | other -> body
      )
    | BoxSet'(var, expr) -> BoxSet'(var, box_get_set_body level arg expr)
          
          
            
    | If'(test, dit, dif) -> If'(box_get_set_body level arg test, box_get_set_body level arg dit, box_get_set_body level arg dif)
    | Seq'(expr_list) -> Seq'(List.map (box_get_set_body level arg) expr_list)
    | Or'(expr_list) -> Or'(List.map (box_get_set_body level arg) expr_list )
    | Applic'(expr, expr_list) -> Applic'((box_get_set_body level arg) expr,List.map (box_get_set_body level arg) expr_list)
    | ApplicTP'(expr, expr_list) -> ApplicTP'((box_get_set_body level arg) expr,List.map (box_get_set_body level arg) expr_list)
    | LambdaSimple'(arg_list, inner_body) -> LambdaSimple'(arg_list, (box_get_set_body (level+1) arg) inner_body)
    | LambdaOpt'(arg_list, opt_arg, inner_body) -> LambdaOpt'(arg_list, opt_arg, (box_get_set_body (level+1) arg) inner_body)
    | Def'(expr_var, expr_val) -> raise X_syntax_error
    | other -> body;;

let handle_arg minor arg body = 
    let need_boxing read_list write_list = 
    let rec need_boxing_rec ls1 ls2 = 
    (match ls1, ls2 with
      | [], [] -> false
      | [] , other -> true
      | other , [] -> true
      | arg1::rest1, arg2::rest2 -> if (List.mem arg1 ls2) 
                                    (*arg1 is in l2*)
                                        (*arg1 is in l2 AND arg2 is in l1*)
                                        then (false || need_boxing_rec rest1 (List.filter (fun a -> a!=arg1) ls2)) 
                                        (*arg1 is in l2 AND arg2 is NOT in l1*)
                                        else true ) in
    match read_list, write_list with
    | [] , other -> false
    | other , [] -> false
    | non_empty1, non_empty2 -> need_boxing_rec non_empty1 non_empty2 in

  let fathers = find_fathers minor arg body in

    match fathers with
  | [read_list;write_list] ->  if need_boxing read_list write_list then true else false
  | other -> false

let box_set_lambda arg_list body =

  let rec args_to_set_box_list minor arg_list body = match arg_list with
    | car :: cdr -> 
    let needs_boxing = handle_arg minor car body in if needs_boxing 
      then
          let set_box_car = Set'(Var'(VarParam(car, minor)), Box'((VarParam(car, minor)))) in
          let set_box_cdr = args_to_set_box_list (minor+1) cdr body in ([set_box_car] @ set_box_cdr)

      else args_to_set_box_list (minor+1) cdr body
    | [] -> [] in
  

  let rec box_get_set_all_args minor arg_list rec_body = match arg_list with
    | car :: cdr -> 
    let needs_boxing = handle_arg minor car body in if needs_boxing 
      then let new_body = box_get_set_body (-1) car rec_body in box_get_set_all_args (minor + 1) cdr new_body
      else box_get_set_all_args (minor + 1) cdr rec_body
    | [] -> rec_body in

  let set_box_list = args_to_set_box_list 0 arg_list body in
  let new_body = box_get_set_all_args 0 arg_list body in
  match set_box_list with
  | [] -> body
  | list -> Seq'(set_box_list@[new_body])

   ;;

let rec box_set_rec e = match e with
  | If'(test, dit, dif) -> If'(box_set_rec test, box_set_rec dit, box_set_rec dif)
  | Seq'(expr_list) -> Seq'(List.map box_set_rec expr_list)
  | Set'(expr_var, expr_val) -> Set'(box_set_rec expr_var,box_set_rec expr_val)
  | Def'(expr_var, expr_val) -> Def'(box_set_rec expr_var,box_set_rec expr_val)
  | Or'(expr_list) -> Or'(List.map box_set_rec expr_list)
  | Applic'(expr, expr_list) -> Applic'(box_set_rec expr, List.map box_set_rec expr_list) 
  | ApplicTP'(expr, expr_list) -> ApplicTP'(box_set_rec expr, List.map box_set_rec expr_list) 
  | LambdaSimple'(arg_list, body) -> LambdaSimple'(arg_list,box_set_rec (box_set_lambda arg_list body))
  | LambdaOpt'(arg_list, opt_arg, body) -> LambdaOpt'(arg_list, opt_arg, box_set_rec (box_set_lambda (arg_list@[opt_arg]) body))
  | other -> other;; 

let annotate_lexical_addresses e = annotate_lexical_rec e;;

let annotate_tail_calls e = annotate_tail_rec false e;;
  
let box_set e = box_set_rec e;;

let run_semantics expr =
  box_set
    (annotate_tail_calls
       (annotate_lexical_addresses expr));;

end;; (* struct Semantics *)