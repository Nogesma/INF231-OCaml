(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)


(* Q4: *)


type 'a multiElement = 'a * int;;
type 'a multiSet = 'a multiElement list;;

(* return a tuple (a,b) where a = numbers of differents elements/b = numbers of elements *)
let cardinal ms = 
  (
    List.length ms,
    List.fold_left (+) 0 (List.map (fun (_, a) -> a) ms)
  );;

(* Itterate the set and return the number assiocated to the element e or 0 if e is not in the set *)
let rec occurences e =
  function
  | (a, b)::lp ->
    if a = e
    then b
    else occurences e lp
  | [] -> 0
;;

(* boolegyan function wich return true if an element e is in a MultiElement
   from the set, otherwise it will return false *)
let rec isInMS e =
  function
  | (a, b)::lp -> a = e || isInMS e lp
  | [] -> false
;;

(* helper function *)
(* same as before but will return true only when the element is present at least b times *)
let rec isMEInMS (a, b) =
  function
  | (x, y)::lp -> (a = x && b <= y) || isMEInMS (a, b) lp
  | [] -> false
;;

(* Boolean function wich return true if every tuple (a,b) in a first set,
   the element a is present at least b times in the second set *)
let rec isIncludedIn ms =
  function
  | (x, y)::lp -> (isMEInMS (x, y) ms) && isIncludedIn ms lp
  | [] -> true
;;

let rec add e =
  function
  | (a, b)::lp ->
    if a = e
    then (a, (b + 1))::lp
    else (a, b)::(add e lp)
  | [] -> [(e, 1)]
;;


let rec del (x, n) =
  function
  | (a, b)::lp ->
    if a = x
    then
      if n = 0 || n >= b
      then lp
      else (a, (b - n))::lp
    else (a, b)::(del (x, n) lp)
  | [] -> []
;;

let equality ms1 ms2 = isIncludedIn ms1 ms2 && isIncludedIn ms2 ms1;;


(* Tests: *)

assert (cardinal [(1, 2); (2, 1); (3, 5)] = (3, 8));;
assert (cardinal [] = (0, 0));;

assert (occurences 3 [(1, 2); (2, 1); (3, 5)] = 5);;
assert (occurences 0 [(1, 2); (2, 1); (3, 5)] = 0);;

assert (isInMS 2 [(1, 2); (2, 1); (3, 5)] = true);;
assert (isInMS 0 [(1, 2); (2, 1); (3, 5)] = false);;


assert (isMEInMS (1, 1) [(1, 2); (2, 1); (3, 5)] = true);;
assert (isMEInMS (1, 5) [(1, 2); (2, 1); (3, 5)] = false);;

assert (isIncludedIn [(1, 2); (2, 1); (3, 5)] [(1, 1); (3, 5)] = true);;
assert (isIncludedIn [(1, 2); (2, 1); (3, 5)] [(1, 2); (2, 2)] = false);;

assert (add 5 [(1, 2); (2, 1); (3, 5)] = [(1, 2); (2, 1); (3, 5); (5, 1)]);;
assert (add 5 [(1, 2); (5, 1); (3, 5)] = [(1, 2); (5, 2); (3, 5)]);;

assert (del (5, 2) [(1, 2); (5, 4); (3, 5)] = [(1, 2); (5, 2); (3, 5)]);;
assert (del (5, 0) [(1, 2); (5, 8); (3, 5)] = [(1, 2); (3, 5)]);;

assert (equality [(1, 2); (5, 4); (3, 5)] [(5, 4); (1, 2); (3, 5)] = true);;
assert (equality [(1, 2); (5, 4); (3, 5)] [(2, 4); (1, 2); (3, 5)] = false);;
