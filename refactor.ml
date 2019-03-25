(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)

(* Q2: *)

(*
* For each function, we just addapt the code from sets.ml in order to use it   * for native lists
* all the function work the same way as the old ones.
*)

let rec cardinal =
  function
  | x::lp -> cardinal lp + 1
  | [] -> 0
;;

let rec isInList e =
  function
  | x::lp -> e = x || isInList e lp
  | [] -> false
;;

let rec isIncludedIn l =
  function
  | x::lp -> isInList x l && isIncludedIn l lp
  | [] -> true
;;

let addElementToList e l  =
  if isInList e l
  then l
  else e::l
;;

let rec supElementFromList e =
  function
  | x::lp ->
    if e = x
    then lp
    else x::(supElementFromList e lp)
  | [] -> []
;;

let listsAreEqual l1 l2 = isIncludedIn l1 l2 && isIncludedIn l2 l1;;

let rec intersection l =
  function
  | x::lp ->
    if isInList x l
    then x::(intersection l lp)
    else intersection l lp
  | [] -> []
;;

let rec union l =
  function
  | x::lp -> union (addElementToList x l) lp
  | [] -> l
;;

let rec difference l =
  function
  | x::lp -> difference (supElementFromList x l) lp
  | [] -> l
;;

let symetricalDifference l1 l2 = difference (union l1 l2) (intersection l1 l2);;

(* Tests: *)

assert (cardinal [1; 2] = 2);;
assert (cardinal [] = 0);;

assert (isInList 2 [1; 2] = true);;
assert (isInList 4 [1; 2] = false);;

assert (isIncludedIn [1; 2; 3] [2; 3] = true);;
assert (isIncludedIn [1; 2; 3] [4; 3] = false);;

assert (addElementToList 3 [1; 2] = [3; 1; 2]);;
assert (addElementToList 2 [1; 2] = [1; 2]);;

assert (supElementFromList 3 [1; 2] = [1; 2]);;
assert (supElementFromList 3 [1; 2; 3] = [1; 2]);;

assert (listsAreEqual [1; 2] [2; 1] = true);;
assert (listsAreEqual [1; 2] [2; 3] = false);;

assert (intersection [1; 2] [1; 3] = [1]);;
assert (intersection [1; 2] [0; 3] = []);;

assert (union [1; 2] [1; 3] = [3; 1; 2]);;
assert (union [1] [2; 3] = [3; 2; 1]);;

assert (difference [1; 2] [1; 3] = [2]);;
assert (difference [1; 2] [4; 3] = [1; 2]);;

assert (symetricalDifference [1; 2] [1; 3] = [3; 2]);;
assert (symetricalDifference [0; 2] [1; 3] = [3; 1; 0; 2]);;


(* Q3: *)

let cardinal l = List.length l;;

let isElementInList e l = List.exists (fun x -> x = e) l;;

let isListIncludedIn l1 l2 = List.for_all (fun x -> isElementInList x l1) l2;;

let supElementFromList e l = List.filter (fun x -> x <> e) l;;

let intersection l1 l2 = List.filter (fun x -> isElementInList x l1) l2;;

let rec difference l1 l2 = List.filter (fun x -> not (isElementInList x l2)) l1;;

let union l1 l2 = l1 @ difference l2 l1;;

(* Tests: *)

assert (cardinal [1; 2] = 2);;
assert (cardinal [] = 0);;

assert (isElementInList 2 [1; 2] = true);;
assert (isElementInList 4 [1; 2] = false);;

assert (isListIncludedIn [1; 2; 3] [2; 3] = true);;
assert (isListIncludedIn [1; 2; 3] [4; 3] = false);;

assert (supElementFromList 3 [1; 2] = [1; 2]);;
assert (supElementFromList 3 [1; 2; 3] = [1; 2]);;

assert (intersection [1; 2] [1; 3] = [1]);;
assert (intersection [1; 2] [0; 3] = []);;

assert (union [1; 2] [1; 3] = [1; 2; 3]);;
assert (union [1] [2; 3] = [1; 2; 3]);;

assert (difference [1; 2] [1; 3] = [2]);;
assert (difference [1; 2] [4; 3] = [1; 2]);;
