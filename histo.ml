#use "list_tools.ml"

(* add_occ function takes two paramters: i and hist and returns hist increased by 1 for the ith element *)
let add_occ i hist =
  if i < 0 then invalid_arg "add_occ: i should be >= 0"
  else (
    let rec increase n = function
        [] -> invalid_arg "add_occ: hist is too short"
      | e::l when n = i -> (e + 1)::l
      | e::l -> e::(increase (n + 1) l)
    in increase i hist
  )
;;

(* add_occ 0 [0; 0; 2; 5; 0; 1; 1; 0] ;; *)

(* get_hist build the histogram of a given list *)
let get_hist list =
  let rec fill_hist = function
      [] -> 
