(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)

(* Q2: *)

(*
* For each function, we just addapt the code from sets.ml in order to use it for native lists
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

(* Tests: OK *)

assert (cardinal [1; 2] = 2);;
assert (cardinal [] = 0);;

assert (isInList 2 [1; 2] = true);;
assert (isInList 4 [1; 2] = false);;

assert (isIncludedIn [1; 2; 3] [2; 3] = true);;
assert (isIncludedIn [1; 2; 3] [4; 3] = false);;

assert (addElementToList 3 [1; 2] = [1; 2; 3]);;
assert (addElementToList 2 [1; 2] = [1; 2]);;

supElementFromList 3 [1; 2];; (* [1; 2] *)
supElementFromList 3 [1; 2; 3];; (* [1; 2] *)

listsAreEqual [1; 2] [2; 1];; (* true *)
listsAreEqual [1; 2] [2; 3];; (* false *)

intersection [1; 2] [1; 3];; (* [1] *)
intersection [1; 2] [0; 3];; (* [] *)

union [1; 2] [1; 3];; (* [1; 2; 3] *)
union [1] [2; 3];; (* [1; 2; 3] *)

difference [1; 2] [1; 3];; (* [2] *)
difference [1; 2] [4; 3];; (* [1; 2] *)

symetricalDifference [1; 2] [1; 3];; (*[2; 3] *)
symetricalDifference [0; 2] [1; 3];; (* [0; 2; 1; 3] *)
