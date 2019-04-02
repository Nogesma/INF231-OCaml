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
  | _::tl -> cardinal tl + 1
  | [] -> 0
;;

let rec isInList (e :'a) =
  function
  | hd::tl -> e = hd || isInList e tl
  | [] -> false
;;

let rec isIncludedIn (l :'a list) =
  function
  | hd::tl -> isInList hd l && isIncludedIn l tl
  | [] -> true
;;

let addElementToList (e :'a) (l :'a list) :'a list =
  if isInList e l
  then l
  else e::l
;;

let rec supElement (e :'a) =
  function
  | hd::tl ->
    if e = hd
    then tl
    else hd::(supElement e tl)
  | [] -> []
;;

let listsAreEqual (l1 :'a list) (l2 :'a list) :bool = isIncludedIn l1 l2 && isIncludedIn l2 l1;;

let rec intersection (l :'a list) =
  function
  | hd::tl ->
    if isInList hd l
    then hd::(intersection l tl)
    else intersection l tl
  | [] -> []
;;

let rec union (l :'a list) =
  function
  | hd::tl -> union (addElementToList hd l) tl
  | [] -> l
;;

let rec difference (l :'a list) =
  function
  | hd::tl -> difference (supElement hd l) tl
  | [] -> l
;;

let symetricalDifference (l1 :'a list) (l2 :'a list) :'a list = difference (union l1 l2) (intersection l1 l2);;

(* Tests: *)

(* cardinal: *)
assert (cardinal [1; 2] = 2);;
assert (cardinal [] = 0);;

(* isInList: *)
assert (isInList 2 [1; 2] = true);;
assert (isInList 4 [1; 2] = false);;

(* isIncludedIn: *)
assert (isIncludedIn [1; 2; 3] [2; 3] = true);;
assert (isIncludedIn [1; 2; 3] [1; 2; 3] = true);;
assert (isIncludedIn [1; 2; 3] [3; 2] = true);;
assert (isIncludedIn [1; 2; 3] [4; 3] = false);;

(* addElementToList: *)
assert (addElementToList 3 [1; 2] = [3; 1; 2]);;
assert (addElementToList 2 [1; 2] = [1; 2]);;

(* supElement: *)
assert (supElement 3 [1; 2] = [1; 2]);;
assert (supElement 2 [1; 2; 3] = [1; 3]);;
assert (supElement 3 [1; 2; 3] = [1; 2]);;

(* listsAreEqual: *)
assert (listsAreEqual [1; 2] [1; 2] = true);;
assert (listsAreEqual [1; 2] [2; 1] = true);;
assert (listsAreEqual [1; 2] [2; 3] = false);;

(* intersection: *)
assert (intersection [1; 2] [1; 3] = [1]);;
assert (intersection [1; 2] [1; 2] = [1; 2]);;
assert (intersection [1; 2] [0; 3] = []);;

(* union: *)
assert (union [1; 2] [3; 4] = [4; 3; 1; 2]);;
assert (union [1; 2] [1; 4] = [4; 1; 2]);;
assert (union [1; 2] [1; 2] = [1; 2]);;
assert (union [] [2; 3] = [3; 2]);;

(* difference: *)
assert (difference [1; 2] [1; 2] = []);;
assert (difference [1; 2] [1; 3] = [2]);;
assert (difference [1; 2] [4; 3] = [1; 2]);;

(* symetricalDifference: *)
assert (symetricalDifference [1; 2] [1; 2] = []);;
assert (symetricalDifference [1; 2] [1; 3] = [3; 2]);;
assert (symetricalDifference [0; 2] [1; 3] = [3; 1; 0; 2]);;


(* Q3: *)

let card (l :'a list) :int = List.length l;;

let isElementInList (e :'a) (l :'a list) :bool = List.exists (fun x -> x = e) l;;

let isListIncludedIn (l1 :'a list) (l2 :'a list) :bool = List.for_all (fun x -> isElementInList x l1) l2;;

let supElementFromList (e :'a) (l :'a list) :'a list = List.filter (fun x -> x <> e) l;;

let inter (l1 :'a list) (l2 :'a list) :'a list = List.filter (fun x -> isElementInList x l1) l2;;

let diff (l1 :'a list) (l2 :'a list) :'a list = List.filter (fun x -> not (isElementInList x l2)) l1;;

let unionOfLists (l1 :'a list) (l2 :'a list) :'a list = l1 @ diff l2 l1;;

(* Tests: *)

(* card: *)
assert (card [1; 2] = 2);;
assert (card [] = 0);;

(* isElementInList: *)
assert (isElementInList 2 [1; 2] = true);;
assert (isElementInList 4 [1; 2] = false);;

(* isListIncludedIn: *)
assert (isListIncludedIn [1; 2; 3] [2; 3] = true);;
assert (isListIncludedIn [1; 2; 3] [1; 2; 3] = true);;
assert (isListIncludedIn [1; 2; 3] [3; 2] = true);;
assert (isListIncludedIn [1; 2; 3] [4; 3] = false);;

(* supElementFromList: *)
assert (supElementFromList 3 [1; 2] = [1; 2]);;
assert (supElementFromList 2 [1; 2; 3] = [1; 3]);;
assert (supElementFromList 3 [1; 2; 3] = [1; 2]);;

(* inter: *)
assert (inter [1; 2] [1; 3] = [1]);;
assert (inter [1; 2] [1; 2] = [1; 2]);;
assert (inter [1; 2] [0; 3] = []);;

(* unionOfLists: *)
assert (unionOfLists [1; 2] [3; 4] = [1; 2; 3; 4]);;
assert (unionOfLists [1; 2] [1; 4] = [1; 2; 4]);;
assert (unionOfLists [1; 2] [1; 2] = [1; 2]);;
assert (unionOfLists [] [2; 3] = [2; 3]);;

(* diff: *)
assert (diff [1; 2] [1; 2] = []);;
assert (diff [1; 2] [1; 3] = [2]);;
assert (diff [1; 2] [4; 3] = [1; 2]);;
