#use "code-gen.ml";;
#use "compiler.ml";;

open Code_Gen;;
make_consts_tbl (string_to_asts "(list \"ab\" '(1 2) 'c 'ab)");;
