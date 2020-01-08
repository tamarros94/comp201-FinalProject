#use "semantic-analyser.ml";;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "SOB_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* This signature represents the idea of outputing assembly code as a string
     for a single AST', given the full constants and fvars tables. 
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
end;;

module Code_Gen : CODE_GEN = struct

  type counter = { get : unit -> int;
                     incr : unit -> unit };;

  (* Create Fvar Table *)
  let rec make_fvars_tbl_single_expr expr  = 
        let index =
          let n = ref (-1) in
          { get = (fun () -> !n);
            incr = (fun () -> n:= !n +1) } in

        let rec handle_expr_list expr_list = match expr_list with
        |[] -> []
        |car :: cdr -> let fvars_list_rec = make_fvars_tbl_single_expr car in fvars_list_rec @ handle_expr_list cdr in

          match expr with
          | Var'(VarFree(str)) -> index.incr (); [(str,index.get ())]
          | If'(test, dit, dif) -> (make_fvars_tbl_single_expr test)@(make_fvars_tbl_single_expr dit)@(make_fvars_tbl_single_expr dif)
          | Seq'(expr_list) -> handle_expr_list expr_list
          | Set'(expr_var, expr_val) -> (make_fvars_tbl_single_expr expr_var)@(make_fvars_tbl_single_expr expr_val)
          | Def'(expr_var, expr_val) -> (make_fvars_tbl_single_expr expr_var)@(make_fvars_tbl_single_expr expr_val)
          | Or'(expr_list) -> handle_expr_list expr_list
          | Applic'(expr, expr_list) -> (make_fvars_tbl_single_expr expr)@(handle_expr_list expr_list)
          | ApplicTP'(expr, expr_list) -> (make_fvars_tbl_single_expr expr)@(handle_expr_list expr_list)
          | LambdaSimple'(arg_list, body) -> make_fvars_tbl_single_expr body
          | LambdaOpt'(arg_list, opt_arg, body) -> make_fvars_tbl_single_expr body
          | other -> []
          ;;

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

(* Create Consts Table *)
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


  let make_fvars_tbl asts =
      let rec make_fvars_tbl_rec asts_rec =
        match asts_rec with
        |car::cdr -> let fvars_list_rec = make_fvars_tbl_single_expr car in fvars_list_rec @ make_fvars_tbl_rec cdr
        |[] -> [] in
        make_fvars_tbl_rec asts;;






  let make_consts_tbl asts = raise X_not_yet_implemented;;
  let generate consts fvars e = raise X_not_yet_implemented;;

(* 
  let fvar_index =
    let n = ref -1 in
    { get = (fun () -> !n);
      incr = (fun () -> n:= !n +1) } in

  let make_fvars_tbl_single_expr expr fvars_list = match expr with
  | Var'(VarFree(str)) -> fvar_index.incr; fvars_list@[(str,fvar_index.get)] *)

end;;

