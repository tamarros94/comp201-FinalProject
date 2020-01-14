#use "code-gen.ml";;

open Code_Gen;;
let string_to_asts s = List.map Semantics.run_semantics
                         (Tag_Parser.tag_parse_expressions
                            (Reader.read_sexprs s));;

let asts = (string_to_asts 
"(define y '(#{a}=#t #{a}))");;


make_consts_tbl asts;;

  (* let generate consts fvars e = match e with
  | Const'(constant)
  | Var'(var)
  | Box'(var)
  | BoxGet'(var)
  | BoxSet'(var,expr)
  | If'(expr1,expr2,expr3)
  | Seq'(expr_list)
  | Set'(expr1,expr2)
  | Def'(expr1,expr2)
  | Or'(expr_list)
  | LambdaSimple'(string_list,expr)
  | LambdaOpt'(string_list,string,expr)
  | Applic'(expr,expr_list)
  | ApplicTP'(expr,expr_list);; *)
  
  
  
  
  ;;
  (* match e with
  | Const'(c) -> "mov rax, AddressInConstTable(c)" (string_of_int (fst (List.assoc c consts))) *)


(* 
				((itIs? 'const pe) (let ((label (get_label (cadr pe) const_table )))
										(string-append "\tmov rax , [" label "]\n")))
				((itIs? 'seq pe) (code_gen_seq (cadr pe) env))
				((itIs? 'if3 pe) (code_gen_if (cdr pe) (get_label_counter 'count_label_if) env))
				((itIs? 'or pe) (code_gen_or (cadr pe) (get_label_counter 'count_label_or) env))
				((itIs? 'pvar pe) (let ((minor (number->string (caddr pe))))
										(string-append "\tmov rax , qword [rbp + (4 + " minor ") * 8 ]\n")))
				((itIs? 'bvar pe) (code_gen_bvar (cdr pe) env))
				((itIs? 'fvar pe) (code_gen_fvar (cdr pe) env))
				((itIs? 'define pe) (code_gen_def (cdr pe) env))
				((itIs? 'set pe) (code_gen_set (cdr pe) env))
				((itIs? 'lambda-simple pe) (code_gen_lambda_simple (cdr pe) (get_label_counter 'count_label_lambda) (+ env 1)) )
				((itIs? 'lambda-opt pe) (code_gen_lambda_opt (cdr pe) (get_label_counter 'counter_label_lambda_opt) (+ env 1)))
				((itIs? 'applic pe) (code_gen_applic (cdr pe) (get_label_counter 'counter_label_applic) env))
				((itIs? 'tc-applic pe) (code_gen_tc_applic (cdr pe) (get_label_counter 'counter_label_Tc-applic) env))
				((itIs? 'box pe) (code_gen_box (cdr pe) env))
				((itIs? 'box-set pe) (code_gen_box_set (cdr pe) env))
				((itIs? 'box-get pe) (code_gen_box_get (cdr pe) env)) *)