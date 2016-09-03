:cd /path/to/current/dir

:load CodeGenerator

parse p_exp "" "(mul (sub 5 (add 1 2)) 4)"
main_gen "10"
main_gen "11"