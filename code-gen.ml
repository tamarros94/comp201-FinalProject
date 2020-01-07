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

