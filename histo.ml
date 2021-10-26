(* Charles-Antoine Leger - E2 - TP3 *)

(* Load functions in "list_tools.ml" *)
#use "list_tools.ml"

(* add_occ function takes two paramters: i and hist and returns hist increased by 1 for the ith element *)
let add_occ i hist =
  if i < 0 then invalid_arg "add_occ: i should be >= 0"
  else (
    let rec increase n = function
        [] -> invalid_arg "add_occ: hist is too short"
      | e::l when n = i -> (e + 1)::l
      | e::l -> e::(increase (n + 1) l)
    in increase 0 hist
  )
;;

(* add_occ 2 [0; 0; 2; 5; 0; 1; 1; 0] ;; *)

(* get_hist build the histogram of a given list *)
let get_hist list =
  if list = [] then invalid_arg "get_hist: the given list is empty"
  else (
    let rec fill_hist prev_hist = function
        [] -> prev_hist
      | e::l -> if e < 0 then failwith "I cannot sort that list"
                else fill_hist (add_occ e prev_hist) l
    in fill_hist (init_list ((get_max list) + 1) 0) list
  )
;;

(* get_hist  [0; 1; 2; 2; 0; 3] ;; *)

(* get_sorted function builds a sorted list from a given histogram. *)
let get_sorted hist =
  if hist = [] then invalid_arg "get_sorted: the given histogram is empty"
  else (
    let rec build_list n = function
        [] -> []
      | e::h -> match e with
                  0 -> build_list (n + 1) h
                | 1 -> n::(build_list (n + 1) h)
                | _ -> n::(build_list n ((e - 1)::h))
    in build_list 0 hist
  )
;;

(* get_sorted (get_hist  [1; 5; 0; 9; 4; 4; 3]) ;; *)

(* hist_sort functions sorts a given list thanks to the sort by histogram *)
let hist_sort list =
  if list = [] then []
  else (
    get_sorted (get_hist list)
  )
;;

(* hist_sort [12; 150; 66; 0; 12; 88; 5; 12; 555; 5; 150] ;; *)
