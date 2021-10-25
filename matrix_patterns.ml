#use "list_tools.ml" ;;

(* mat_cross builds the pattern for a given n c1 and c2. *)
let mat_cross n c1 c2 =
  if n <= 1 || n mod 2 = 0 then invalid_arg "mat_cross: n should be odd and >1"
  else (
    let rec build_list x = function
        i when i = n -> []
      | i -> if (i = n / 2 || x = n / 2) then c1::build_list x (i + 1) 
             else c2::build_list x (i + 1)
    in let rec build_board = function
           0 -> []
         | i -> (build_list (n - i) 0)::build_board (i - 1)
       in build_board n    
  )
;;

(* mat_cross 3 '*' '-';; *)
(* print_board (mat_cross 5 '*' '-') ;; *)

(* mat_cross_diag builds the pattern for a given n c1 and c2 *)
let mat_cross_diag n c1 c2 =
  if n <= 1 then invalid_arg "mat_cross_diag: n must be >1"
  else (
    let rec build_list x = function
        i when i = n -> []
      | i -> if (i = x || i + x = n - 1) then c1::build_list x (i + 1)
             else c2::build_list x (i + 1)
    in let rec build_board = function
           0 -> []
         | i -> (build_list (n - i) 0)::build_board (i - 1)
       in build_board n
  )
;;

(* print_board (mat_cross_diag 5 '*' '-') ;;
print_board (mat_cross_diag 6 '*' '-') ;; *)

(* mat_square builds the pattern for a given n c1 and c2 *)
let mat_square n c1 c2 =
  if n <= 1 then invalid_arg "mat_square: n must be >1"
  else (
    let rec build_list x = function
        i when i = n -> []
      | i -> if (i + x) mod 2 = 0 then c1::build_list x (i + 1)
             else c2::build_list x (i + 1)
    in let rec build_board = function
           0 -> []
         | i -> (build_list (n - i) 0)::build_board (i - 1)
       in build_board n
  )
;;

(* print_board (mat_square 5 '*' '.') ;;
print_board (mat_square 6 '*' '.') ;; *)

(* mat_pattern_batch takes a board and a list and returns the given board with its elements replace at the coords given by the list *)
let mat_pattern_batch coor mat =
  let rec replace_v i = function
      (_,[]) -> []
    | ([], b) -> b
    | ((x, y, v)::l, e::b) -> if i = x then (put_list v y e)::(replace_v (i + 1) (l, b))
                              else e::(replace_v (i + 1) ((x, y, v)::l, b))
  in replace_v 0 (coor, mat)
;;

(* print_board (mat_pattern_batch [(1, 2, 'a'); (4, 1, 'b')] (init_board (5, 5) '*')) ;; *)
