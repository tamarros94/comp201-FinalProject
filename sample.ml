#use "code-gen.ml";;

open Code_Gen;;
let string_to_asts s = List.map Semantics.run_semantics
                         (Tag_Parser.tag_parse_expressions
                            (Reader.read_sexprs s));;

let asts = (string_to_asts 
"(or #f #t) (or #f #t)");;

  let consts_tbl = Code_Gen.make_consts_tbl asts ;;
  let fvars_tbl = Code_Gen.make_fvars_tbl asts ;;
  let generate = Code_Gen.generate consts_tbl fvars_tbl ;;
String.concat "\n\n"
                        (List.map
                           (fun ast -> (generate ast) ^ "\n\tcall write_sob_if_not_void")
                           asts) ;;

  
  
  
  
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