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

module Code_Gen
 (* : CODE_GEN  *)
 = struct

  type counter = { get : unit -> int;
                     incr : unit -> unit };;
            
  type counter2 = { get_and_inc: int -> int;
                    };;

  (* Create Fvar Table *)
  let rec make_fvars_tbl_single_expr index expr  = 

        let rec handle_expr_list expr_list = match expr_list with
        |[] -> []
        |car :: cdr -> let fvars_list_rec = make_fvars_tbl_single_expr index car in fvars_list_rec @ handle_expr_list cdr in

          match expr with
          | If'(test, dit, dif) -> (make_fvars_tbl_single_expr index test)@(make_fvars_tbl_single_expr index dit)@(make_fvars_tbl_single_expr index dif)
          | Seq'(expr_list) -> handle_expr_list expr_list
          | Set'(expr_var, expr_val) -> (make_fvars_tbl_single_expr index expr_var)@(make_fvars_tbl_single_expr index expr_val)
          | Def'(Var'(VarFree(str)), expr_val) -> index.incr (); [(str,index.get ())]@(make_fvars_tbl_single_expr index expr_val)
          | Def'(expr_var, expr_val) -> (make_fvars_tbl_single_expr index expr_var)@(make_fvars_tbl_single_expr index expr_val)
          | Or'(expr_list) -> handle_expr_list expr_list
          | Applic'(expr, expr_list) -> (make_fvars_tbl_single_expr index expr)@(handle_expr_list expr_list)
          | ApplicTP'(expr, expr_list) -> (make_fvars_tbl_single_expr index expr)@(handle_expr_list expr_list)
          | LambdaSimple'(arg_list, body) -> make_fvars_tbl_single_expr index body
          | LambdaOpt'(arg_list, opt_arg, body) -> make_fvars_tbl_single_expr index body
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

let rec print_sexpr sexpr = match sexpr with
    | Sexpr(Nil) -> Printf.printf "Sexpr(Nil)"
    | Sexpr(Bool(false)) -> Printf.printf "Sexpr(Bool(false))"
    | Sexpr(Bool(true)) -> Printf.printf "Sexpr(Bool(true))"
    | Sexpr(Number(Float f)) -> Printf.printf "Sexpr(Number(Float %f))" f
    | Sexpr(Number(Int n)) -> Printf.printf "Sexpr(Number(Int %d))" n
    | Sexpr(Char(char)) ->   Printf.printf "Sexpr(Char(%c))" char
    | Sexpr(String(str)) -> Printf.printf "Sexpr(String(%s))" str
    | Sexpr(Symbol(str)) -> Printf.printf "Sexpr(Symbol(%s))" str
    | Sexpr(Pair(sexpr1, sexpr2)) -> Printf.printf "Sexpr(Pair(";print_sexpr (Sexpr(sexpr1)); Printf.printf ", " ; print_sexpr (Sexpr(sexpr2)); Printf.printf "))" 
    | Sexpr(TaggedSexpr(str, sexpr1)) -> Printf.printf "TaggedSexpr(%s, " str; print_sexpr (Sexpr(sexpr1));Printf.printf ")";
    | Sexpr(TagRef(str)) -> Printf.printf "TagRef(%s)" str 
    | other -> Printf.printf "other" 
          ;;

let rec rename_tagged_sexpr index sexpr = 
match sexpr with
| Sexpr(TaggedSexpr(name1, expr1)) -> 
let expr_rename = rename_tagged_sexpr index (Sexpr(expr1)) in
  (match expr_rename with
  | Sexpr(expr) -> Sexpr(TaggedSexpr(name1^(string_of_int (index.get ())), expr))
  | other -> Sexpr(Nil))
| Sexpr(TagRef(name1)) ->  Sexpr(TagRef(name1^(string_of_int (index.get ()))))
| Sexpr(Pair(car, cdr)) -> 
let car_rename = rename_tagged_sexpr index (Sexpr(car)) in
let cdr_rename = rename_tagged_sexpr index (Sexpr(cdr)) in
  (match car_rename, cdr_rename with
  | Sexpr(s1), Sexpr(s2) -> Sexpr(Pair(s1, s2))
  | other1, other2 -> Sexpr(Nil))
| other -> other;;

let rename_tagged_sexprs sexpr_list =
    let index =
        let n = ref 0 in
        { get = (fun () -> !n);
          incr = (fun () -> n:= !n +1) } in

    let rec iterate_list sexprs = 
     match sexprs with
    | [] -> []
    | car :: cdr ->
    let renamed = [rename_tagged_sexpr index car] in index.incr ();
    renamed@(iterate_list cdr) in
    iterate_list sexpr_list;;


(* ③+4 Expand the list to include all sub-constants *)
let rec expand_sub_sexpr index sexpr = match sexpr with
| Sexpr(Pair(car, cdr)) -> 
let new_car = (expand_sub_sexpr index (Sexpr car)) in
let new_cdr = (expand_sub_sexpr index (Sexpr cdr)) in
new_car@new_cdr@[Sexpr(Pair(car, cdr))]
| Sexpr(TaggedSexpr(name1, expr1)) -> 
(expand_sub_sexpr index (Sexpr expr1))@[Sexpr(TaggedSexpr(name1, expr1))]
| Sexpr(TagRef(name1)) -> [Sexpr(TagRef(name1))]
| Sexpr(Symbol(str)) -> (expand_sub_sexpr index (Sexpr(String(str)))) @[Sexpr(Symbol(str))]
| other -> [other];;

let expand_sexpr_list sexpr_list = 
  let index =
        let n = ref 0 in
        { get = (fun () -> !n);
          incr = (fun () -> n:= !n +1) } in
let rec expand_sexpr sexprs = 
  match sexprs with
  |[] -> []
  |car :: cdr -> let list_rec = expand_sub_sexpr index car in index.incr (); list_rec@(expand_sexpr cdr) in
  expand_sexpr sexpr_list;;

(* ⑤ Go over the list, from first to last, and create the
constants-table: *)
(* ① For each sexpr in the list, create a 3-tuple: *)
let rec collect_tag_defs_single tag_defs_collection sexpr = 
    (* Printf.printf "collect_tag_defs_single: "; print_sexpr sexpr; Printf.printf "\n"; *)

match sexpr with
| Sexpr(TaggedSexpr(name, expr)) -> [name, (Sexpr(expr))]
| Sexpr(Pair(car, cdr)) -> (collect_tag_defs_single tag_defs_collection (Sexpr(car)))@(collect_tag_defs_single tag_defs_collection (Sexpr(cdr)))
| other -> [];;


let rec collect_tag_defs tag_defs_collection sexpr_list = match sexpr_list with
| [] -> []
|car :: cdr -> let list_rec = collect_tag_defs_single tag_defs_collection car in list_rec@(collect_tag_defs tag_defs_collection cdr) ;;

let get_string_size str = 9 + (String.length str) ;;
let rec get_sexpr_address tag_defs_collection first_pass acc_const_table sexpr = 
    (* Printf.printf "get_sexpr_address: "; print_sexpr sexpr; Printf.printf "\n"; *)
    match sexpr with
    | Sexpr(TagRef(str)) ->  if first_pass then "dummy" else
    let tagged_sexpr = List.assoc str tag_defs_collection in
     get_sexpr_address tag_defs_collection first_pass acc_const_table tagged_sexpr
    | Sexpr(TaggedSexpr(str, sexpr1)) -> get_sexpr_address tag_defs_collection first_pass acc_const_table (Sexpr(sexpr1))
    | other -> (string_of_int (fst (List.assoc sexpr acc_const_table))) ;;

let rec make_consts_tbl_single_sexpr first_pass tag_defs_collection sexpr acc_const_table index = 
    (* (if first_pass then Printf.printf "first_pass\n" else Printf.printf "second_pass\n");
    Printf.printf "make_consts_tbl_single_sexpr: "; print_sexpr sexpr; Printf.printf "\n"; *)
    match sexpr with
    | Sexpr(Number(Float f)) -> [(sexpr, (index.get_and_inc (9), "MAKE_LITERAL_FLOAT("^(string_of_float f)^")"))]
    | Sexpr(Number(Int n)) -> [(sexpr, (index.get_and_inc (9), "MAKE_LITERAL_INTEGER("^(string_of_int n)^")"))]    
    | Sexpr(Char(char)) -> [(sexpr, (index.get_and_inc (2), "MAKE_LITERAL_CHAR("^(Char.escaped char)^")"))]   
    | Sexpr(String(str)) -> [(sexpr, (index.get_and_inc (get_string_size str), "MAKE_LITERAL_STRING \""^str^"\""))]   
    | Sexpr(Symbol(str)) -> [(sexpr, (index.get_and_inc (9), "MAKE_LITERAL_SYMBOL(const_tbl+"^(get_sexpr_address tag_defs_collection first_pass acc_const_table (Sexpr(String(str))))^")"))]  
    | Sexpr(Pair(sexpr1, sexpr2)) -> [(sexpr, (index.get_and_inc (17), "MAKE_LITERAL_PAIR(const_tbl+"^(get_sexpr_address tag_defs_collection first_pass acc_const_table (Sexpr(sexpr1)))^", const_tbl+"^(get_sexpr_address tag_defs_collection first_pass acc_const_table (Sexpr(sexpr2)))^")"))]
    | Sexpr(TaggedSexpr(str, sexpr1)) -> make_consts_tbl_single_sexpr first_pass tag_defs_collection (Sexpr(sexpr1)) acc_const_table index
    (* | Sexpr(TagRef(str)) -> [(sexpr, (index.get_and_inc (9), "consts+"^(get_sexpr_address tag_defs_collection first_pass acc_const_table sexpr)))]    *)
        | other -> [];;
  


let rec print_list = function 
[] -> Printf.printf "()\n"
| e::l -> print_sexpr e ; print_string " ; " ; print_list l;;

let make_consts_table tag_defs_collection sexpr_list = 
     
          
        let rec turn_sexpr_list_to_const_table first_pass sexpr_list_rec const_table index_rec = 
        match sexpr_list_rec with
        |[] -> []
        |car :: cdr -> let list_rec = (make_consts_tbl_single_sexpr first_pass tag_defs_collection car const_table index_rec) in
        list_rec @ (turn_sexpr_list_to_const_table first_pass cdr (const_table@list_rec) index_rec) in

     let index =
          let n = ref 6 in
          { get_and_inc = (fun (add) -> let old = !n in n:=!n + add; old)} in

    let first_pass_const_table = turn_sexpr_list_to_const_table true sexpr_list     
    [(Void, (0, "MAKE_VOID"));
    (Sexpr(Nil), (1, "MAKE_NIL"));
    (Sexpr(Bool false), (2, "MAKE_BOOL(0)"));
    (Sexpr(Bool true), (4, "MAKE_BOOL(1)"));]
     index in
      let index =
          let n = ref 6 in
          { get_and_inc = (fun (add) -> let old = !n in n:=!n + add; old)} in
     let first_pass_const_table = [(Void, (0, "MAKE_VOID"));
    (Sexpr(Nil), (1, "MAKE_NIL"));
    (Sexpr(Bool false), (2, "MAKE_BOOL(0)"));
    (Sexpr(Bool true), (4, "MAKE_BOOL(1)"));]@first_pass_const_table in
     let second_pass_const_table = turn_sexpr_list_to_const_table false sexpr_list first_pass_const_table index

     in 
      [(Void, (0, "MAKE_VOID"));
    (Sexpr(Nil), (1, "MAKE_NIL"));
    (Sexpr(Bool false), (2, "MAKE_BOOL(0)"));
    (Sexpr(Bool true), (4, "MAKE_BOOL(1)"));]@second_pass_const_table;;
  







  let make_fvars_tbl asts =
    let index =
            let n = ref (-1) in
            { get = (fun () -> !n);
              incr = (fun () -> n:= !n +1) } in
      let rec make_fvars_tbl_rec asts_rec =
        match asts_rec with
        |car::cdr -> let fvars_list_rec = make_fvars_tbl_single_expr index car in fvars_list_rec @ make_fvars_tbl_rec cdr
        |[] -> [] in
        
        make_fvars_tbl_rec asts;;

  let make_consts_tbl asts =

      let sexpr_set = (convert_sexpr_list_to_set 
                        (expand_sexpr_list 
                         (rename_tagged_sexprs
                            (convert_sexpr_list_to_set 
                                (exprs_to_sexpr_list asts))))) in
      let tag_defs_collection = collect_tag_defs [] sexpr_set in
      
       (* tag_defs_collection;; *)
      make_consts_table tag_defs_collection sexpr_set;;



  let generate_const consts const = 
    let const_address = (string_of_int (fst (List.assoc const consts))) in
    ";GENERATE CONST:\n" ^
    " mov rax, const_tbl+" ^ const_address ^ "\n ;<end const> \n";;

  let generate_param_get minor = 
    ";GENERATE PARAM GET:\n" ^
    "mov rax, qword [rbp + "^ (string_of_int (8 * (4 + minor))) ^"] \n ;<end param get> \n";;

  let generate_bound_get major minor = 
    ";GENERATE BOUND GET:\n" ^
    "mov rax, qword [rbp + 8 ∗ 2]
    mov rax, qword [rax + 8 ∗ "^ string_of_int major ^"]
    mov rax, qword [rax + 8 ∗ "^ string_of_int minor ^"] \n ;<end bound get> \n";;

  let generate_fvar fvars v = 
    let label_in_fvar_table = (string_of_int (List.assoc v fvars)) in
    ";GENERATE FVAR:\n" ^
    "mov rax, qword ["^label_in_fvar_table^"] \n ;<end fvar> \n";;

  let label_index =
    let n = ref 0 in
    { get = (fun () -> !n);
      incr = (fun () -> n:= !n +1) } 

  let rec generate_wrap env_size consts fvars e = match e with
    | Const'(const) -> generate_const consts const
    | Var'(VarParam(_, minor)) -> generate_param_get minor
    | Set'(Var'(VarParam(_, minor)),expr) -> generate_param_set env_size consts fvars minor expr
    | Var'(VarBound(_, major, minor)) -> generate_bound_get major minor
    | Set'(Var'(VarBound(_,major,minor)),expr) -> generate_bound_set env_size consts fvars major minor expr
    | Var'(VarFree(v)) -> generate_fvar fvars v
    | Set'(Var'(VarFree(v)),expr) -> generate_fvar_set env_size consts fvars v expr
    | Def'(Var'(var),expr) -> generate_def env_size consts fvars var expr
    | Seq'(expr_list) -> generate_seq env_size consts fvars expr_list
    | Or'(expr_list) -> generate_or env_size consts fvars expr_list
    | If'(expr1,expr2,expr3) -> generate_if env_size consts fvars expr1 expr2 expr3
    | BoxGet'(var) -> generate_box_get env_size consts fvars var
    | BoxSet'(var,expr) -> generate_box_set env_size consts fvars var expr
    | Box'(var) -> generate_box env_size consts fvars var
    | LambdaSimple'(string_list,body) -> generate_simple_lambda (env_size+1) consts fvars string_list body
    (* | LambdaOpt'(string_list,string,expr) *)
    | Applic'(expr,expr_list) -> generate_applic env_size consts fvars expr expr_list
    (* | ApplicTP'(expr,expr_list) *)
    | other -> ""
  and generate_param_set env_size consts fvars minor expr =
    let generated_expr = generate_wrap env_size consts fvars expr in
    ";GENERATE PARAM SET:\n" ^
    generated_expr ^
    "mov qword [rbp + 8 ∗ (4 + " ^ string_of_int minor ^")], rax
    mov rax, SOB_VOID_ADDRESS \n ;<end param set> \n"
  and generate_bound_set env_size consts fvars major minor expr =
    let generated_expr = generate_wrap env_size consts fvars expr in
    ";GENERATE BOUND SET:\n" ^
    generated_expr ^
    "mov rbx, qword [rbp + 8 ∗ 2]
    mov rbx, qword [rbx + 8 ∗ " ^ string_of_int major ^"]
    mov qword [rbx + 8 ∗ " ^ string_of_int minor ^"], rax
    mov rax, SOB_VOID_ADDRESS \n ;<end bound set> \n"
  and generate_fvar_set env_size consts fvars v expr =
    let generated_expr = generate_wrap env_size consts fvars expr in
    let index_in_fvar_table = (string_of_int (List.assoc v fvars)) in
    ";GENERATE FVAR SET:\n" ^
    generated_expr ^
    "mov qword ["^index_in_fvar_table^"], rax
    mov rax, SOB_VOID_ADDRESS \n ;<end fvar set> \n"
  and generate_seq env_size consts fvars expr_list =
    ";GENERATE SEQUENCE:\n" ^
    (List.fold_right (fun a b -> (generate_wrap env_size consts fvars) a ^  b) expr_list "")
    ^ "; <end sequence> \n"
  and generate_or env_size consts fvars expr_list =
    label_index.incr ();
    let curr_index = label_index.get () in
    let or_fold_fun = (fun a b -> 
    ((generate_wrap env_size consts fvars) a) ^
    "cmp rax, SOB_FALSE_ADDRESS
    jne Lexit"^string_of_int curr_index^" \n\n" ^  b) in
    ";GENERATE OR:\n" ^
    List.fold_right or_fold_fun expr_list ""
    ^"Lexit" ^(string_of_int curr_index) ^ ":\n ;<end or> \n"
  and generate_if env_size consts fvars test dit dif = 
    label_index.incr ();
    let curr_index = label_index.get () in
    let generated_test = generate_wrap env_size consts fvars test in
    let generated_dit = generate_wrap env_size consts fvars dit in
    let generated_dif = generate_wrap env_size consts fvars dif in
    ";GENERATE IF:\n" ^
    generated_test ^
    "cmp rax, SOB_FALSE_ADDRESS
    je Lelse"^string_of_int curr_index ^ "\n" ^
    generated_dit ^ "\n" ^
    "jmp Lexit"^string_of_int curr_index^"\n" ^
    "Lelse"^string_of_int curr_index^":\n" ^
    generated_dif ^ "\n" ^
    "Lexit"^string_of_int curr_index^":\n ;<end if> \n"
  and  generate_box_get env_size consts fvars var = 
    let generated_var = generate_wrap env_size consts fvars (Var'(var)) in
    ";GENERATE BOX GET:\n" ^
    generated_var ^
    "mov rax, qword [rax] \n ;<end box get> \n"
  and  generate_box_set env_size consts fvars var expr = 
    let generated_expr = generate_wrap env_size consts fvars expr in
    let generated_var = generate_wrap env_size consts fvars (Var'(var)) in
    ";GENERATE BOX SET:\n" ^
    generated_expr ^
    "push rax \n" ^
    generated_var ^
    "pop qword [rax]
    mov rax, SOB_VOID_ADDRESS \n ;<end box set> \n"
  and generate_box env_size consts fvars var = 
  let generated_var = generate_wrap env_size consts fvars (Var'(var)) in
  "malloc r8, 8 \n" ^
   generated_var ^
   "mov qword [r8], rax
   mov rax, r8 \n ;<end box> \n"
  and generate_def env_size consts fvars var expr = 
  ";GENERATE DEFINE\n" ^
  (match var with
    |VarBound(_,major,minor) -> generate_bound_set env_size consts fvars major minor expr
    |VarParam(_, minor) -> generate_param_set env_size consts fvars minor expr
    |VarFree(v) -> generate_fvar_set env_size consts fvars v expr
  ) ^ "\n ;<end define> \n"
  and generate_simple_lambda env_size consts fvars string_list body =
    label_index.incr ();
    if env_size=0 then generate_first_simple_lambda env_size consts fvars string_list body else
    let curr_index = label_index.get () in 
    let generated_body = generate_wrap env_size consts fvars body in
    let allocate_env_code =
    "MALLOC rax, " ^ (string_of_int (8*env_size)) ^ "\n" in
    let extend_env_code = 
    "mov r9, rax ;r9 points at env[0]
    mov r8, qword [rbp+16] ;r8 points at the source env
    add rax, 8 ; rax points at env[1]
    mov rcx, " ^ (string_of_int env_size) ^ "
    cmp rcx, 1
    je end_extend_env_loop_" ^ (string_of_int curr_index) ^ "
    
    extend_env_loop_" ^ (string_of_int curr_index) ^ ":
    mov r10, qword [r8]
    mov qword [rax], r10
    add r8, 8
    add rax, 8
    loop extend_env_loop_" ^ (string_of_int curr_index) ^ "
    
    end_extend_env_loop_" ^ (string_of_int curr_index) ^ ":\n"
     in
    let copy_params_to_env = 
    "
    push r9
    mov rcx, qword [rbp+8*3] 
    inc rcx ;include magic params
    shl rcx, 3 ;rcx = size of params list (including magic)
    MALLOC rax, rcx ;allocate room for param list
    mov qword [rax], SOB_NIL_ADDRESS ;param list is empty by default
    pop r9
    mov qword [r9], rax ;place pointer to param list in env[0]

    shr rcx, 3 ;rcx holds num of params
    cmp rcx, 0              
    je end_copy_param_loop_"^(string_of_int curr_index) ^"
    mov r10, rbp
    add r10, 8*4 ;r10 points to beginning of param list on stack

    copy_param_loop_"^(string_of_int curr_index) ^":
    mov r11, qword [r10]
    mov qword [rax], r11
    add r10, 8
    add rax, 8
    loop copy_param_loop_"^(string_of_int curr_index) ^"

    end_copy_param_loop_"^(string_of_int curr_index) ^": \n" in
    let body_label = 
    "lambda_body_"^(string_of_int curr_index) ^":
    push rbp
    mov rbp,rsp \n" ^
    generated_body ^ "
    leave
    ret\n" in
    let closure_code = 
    "MAKE_CLOSURE(rax, r9,lambda_body_"^(string_of_int curr_index)^")
    jmp end_lambda_body_"^(string_of_int curr_index)^"\n" ^
    body_label ^
    "end_lambda_body_"^(string_of_int curr_index)^":\n" in
    allocate_env_code ^ extend_env_code ^ copy_params_to_env ^ closure_code ^ "\n ;<end simple lambda> \n"
  and generate_first_simple_lambda env_size consts fvars string_list body = 
    let curr_index = label_index.get () in
    let generated_body = generate_wrap env_size consts fvars body in
    let body_label = 
    "lambda_body_"^(string_of_int curr_index) ^":
    push rbp
    mov rbp,rsp \n" ^
    generated_body ^ "
    leave
    ret\n" in
    ";GENERATE FIRST SIMPLE LAMBDA:\n" ^
    "MAKE_CLOSURE(rax, SOB_NIL_ADDRESS,lambda_body_"^(string_of_int curr_index)^")
    jmp end_lambda_body_"^(string_of_int curr_index)^"\n" ^
    body_label ^
    "end_lambda_body_"^(string_of_int curr_index)^": \n ;<end first simple lambda> \n"
  and generate_applic env_size consts fvars expr expr_list = 

    let push_magic = 
    "mov rax, SOB_NIL_ADDRESS
    push rax \n" in
    let push_generated_expr_list =    
     (List.fold_right (fun expr acc -> ((generate_wrap env_size consts fvars) expr) ^ "push rax \n" ^ acc) expr_list "") in
    let push_args_num = "push " ^ string_of_int (List.length expr_list) ^ "\n" in
    let generated_expr = generate_wrap env_size consts fvars expr in
    (* let validate_closure = 
    "xor r8, r8
    mov r8b, word [rax]
    cmp r8b, T_CLOSURE
    jne end_applic_" ^ string_of_int curr_index ^ "\n" in *)
    let push_env = 
    "CLOSURE_ENV r9, rax
    push r9 \n" in
    let execute_code = 
    "CLOSURE_CODE r10, rax
    call r10\n" in
    let clean_stack = 
    "add rsp , 8*1 ; pop env
    pop rbx ; pop arg count
    shl rbx , 3 ; rbx = rbx * 8
    add rsp , rbx; pop args\n ;<end applic> \n" in
    ";GENERATE APPLIC\n" ^ push_magic ^ push_generated_expr_list ^ push_args_num ^ generated_expr
    ^ push_env ^ execute_code ^ clean_stack 

      ;;



  let generate consts fvars e = generate_wrap (-1) consts fvars e;;

end;;

