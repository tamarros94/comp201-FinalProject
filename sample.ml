#use "code-gen.ml";;

open Code_Gen;;
let string_to_asts s = List.map Semantics.run_semantics
                         (Tag_Parser.tag_parse_expressions
                            (Reader.read_sexprs s));;


(*                           
make_consts_tbl (string_to_asts 
"(define x #{a}='(#{a}))");; *)
let asts = (string_to_asts 
"(define x #{a}='(#{a}))");;

(* let sexpr_set = (convert_sexpr_list_to_set 
                        (expand_sexpr_list 
                            (convert_sexpr_list_to_set 
                                (exprs_to_sexpr_list asts))));; *)

(* collect_tag_defs [] sexpr_set   ;;                                *)

                                make_consts_tbl asts;;
