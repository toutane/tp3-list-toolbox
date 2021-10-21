(* length function takes a list in parameter and returns its length. *)
let rec length = function
    [] -> 0
  | _::l -> 1 + length l
;;

(* nth function takes a natural and a list in parameter and returns the ith element of the list. *)
let nth i list =
  if i < 0 then invalid_arg "nth: index must be a natural"
  else (
    let rec browse n = function
        [] -> failwith "nth: list is too short"
      | e::_ when n = i -> e
      | _::l -> browse (n + 1) l
    in browse 0 list
  )
;;

(* is_pos function checks if all the elements of a given list are positive or zero. *)
let rec is_pos = function
    [] -> true
  | e::_ when e < 0 -> false
  | _::l -> is_pos l
;;

(* get_max function returns the maximun element of a given list. *)
let rec get_max = function
    [] -> invalid_arg "get_max: empty list"
  | [e] -> e
  | e1::e2::l -> if e1 > e2 then get_max (e1::l)
                 else get_max (e2::l)
;;

(* init_list function builds a list of n elements x. *)
let init_list n x =
  if n < 0 then invalid_arg "init_list: n must be a natural"
  else (
    let rec build = function
        0 -> []
      | i -> x::build (i - 1)
    in build n
  )
;;

(* append function concatenes two given lists. *)
let rec append l1 l2 =
  match (l1, l2) with
    ([], l) | (l, []) -> l
    | (e::l, _) -> e::(append l l2)
;;

(* put_list function replace ith element of a given list by v. *)
let put_list x i list =
  if i < 0 then invalid_arg "put_list: i must be a natural"
  else (
    let rec browse n = function
        _::l when n = i -> x::l
      | [] -> failwith "put_list: list is too short"
      | e::l -> e::(browse (n + 1) l)
    in browse 0 list
  )
;;

(* init_board function builds a board of length l times c filled of element v. *)
let init_board (l, c) v =
  if l < 0 || c < 0 then invalid_arg "init_board: the length of the list or sublist must be a natural"
  else (
    let rec build_list = function
        0 -> []
      | i -> v::build_list (i - 1)
    in let rec build_board = function
           0 -> []
         | i -> (build_list c)::build_board (i - 1)
       in build_board l
  )
;;

(* is_board function checks if a given board is valid. *)
let is_board board =
  let rec len = function
      [] -> 0
    | _::l -> 1 + len l
  in let rec browse_board = function
         [] | [_] -> true
         | e1::e2::b when len e1 = len e2 -> browse_board (e2::b)
         | _ -> false
     in browse_board board
;;

(* print_board function prints a given board with a lines for each sublist.  *)
let print_board board =
  let rec print_e = function
      [] -> print_newline()
    | e::l -> print_char e ;
              print_e l
  in let rec print_l = function
         [] -> print_char ' ' ;
       | e::b -> print_e e ;
                 print_l b
     in print_l board
;;

(* print_board (init_board (5,6) '*') ;; *)

(* get_cell function returns the yth element of the xth list of a given board. *)
let get_cell (x, y) board =
  if x < 0 || y < 0 then invalid_arg "get_cell: the coords of the element must be naturals"
  else (
    let rec get_element n = function
        [] -> failwith "get_cell: the list is too short"
      | e::_ when n = y -> e
      | _::l -> get_element (n + 1) l
    in let rec get_list n = function
           [] -> failwith "get_cell: the board is too short"
         | e::_ when n = x -> get_element 0 e
         | _::l -> get_list (n + 1) l
       in get_list 0 board
  )
;;
                   
(* get_cell (0, 0) [[1; 2]; [3; 4; 5]] ;; *)

(* put_cell function replace the value of an element in a board, it takes tree parameters *)
let put_cell v (x, y) board =
  if x < 0 || y < 0 then invalid_arg "put_cell: the coords of the element must be naturals"
  else (
    let rec replace n = function
        [] -> failwith "put_cell: the list is too short"
      | _::l when n = y -> v::l
      | e::l -> e::replace (n + 1) l
    in let rec find_list n = function
           [] -> failwith "put_cell: the board is too short"
         | e::l when n = x -> (replace 0 e)::l
         | e::l -> e::(find_list (n + 1) l)
       in find_list 0 board
  )
;;

(* put_cell 1 (0, 0) (init_board (5, 3) 0) ;; *)
