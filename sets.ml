(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 * sets.ml
 *
 *)

(* Q1: *)


(* Definition of our set wich is recursive *)
type 'a set =
  | Es                  (* E: empty, s: set *)
  | Cs of 'a * 'a set   (* Cs: constructor *)
;;


(* Recursively call the function while adding 1 until Es is reached *)
let rec cardinal =
  function
  | Cs (_, tl) -> cardinal tl + 1
  | Es -> 0
;;


(* Check each element of the set to see if it matches with e *)
let rec isInSet (e :'a) =
  function
  | Cs (hd, tl) -> e = hd || isInSet e tl
  | Es -> false
;;


(* Check if each element of the second set is included in the first *)
let rec isIncludedIn (s :'a set)  =
  function
  | Cs (hd, tl) -> isInSet hd s && isIncludedIn s tl
  | Es -> true
;;


(* If element is already in set, does nothing, otherwise, add the element to the set *)
let addElementToSet (e :'a) (s :'a set) :'a set =
  if isInSet e s
  then s
  else Cs (e, s)
;;


(* Iterates over the set until it reaches the end whilst checking if the current element is equal to e,
 * if it is, call the function again without the current element
*)
let rec supElementFromSet (e :'a)  =
  function
  | Cs (hd, tl) ->
    if e = hd
    then tl
    else Cs(hd, supElementFromSet e tl)
  | Es -> Es
;;


(* test if each set is included in the other *)
let setsAreEqual (s1 :'a set) (s2 :'a set) :bool = isIncludedIn s1 s2 && isIncludedIn s2 s1;;


(* check if each element of the second set is in the first, if it is add it to the returned set *)
let rec intersection (s :'a set) =
  function
  | Cs (hd, tl) ->
    if isInSet hd s
    then Cs(hd, intersection s tl)
    else intersection s tl
  | Es -> Es
;;


(* add each element of the second set to the first *)
let rec union (s :'a set) =
  function
  | Cs (hd, tl) -> union (addElementToSet hd s) tl
  | Es -> s
;;


(* deletes every element of the second set that are in the first *)
let rec difference (s :'a set) =
  function
  | Cs (hd, tl) -> difference (supElementFromSet hd s) tl
  | Es -> s
;;


(* difference of the union and the intersection of two sets *)
let symetricalDifference (s1 :'a set) (s2 :'a set) :'a set = difference (union s1 s2) (intersection s1 s2);;


(* Tests: *)

(* Cardinal: *)
assert (cardinal (Cs(1, Cs(2, Es))) = 2);;
assert (cardinal (Es) = 0);;

(* isINSet: *)
assert (isInSet 2 (Cs(1, Cs(2, Es))) = true);;
assert (isInSet 4 (Cs(1, Cs(2, Es))) = false);;

(* isIncludedIn: *)
assert (isIncludedIn (Cs(1, Cs(2, Cs(3, Es)))) (Cs(2, Cs(3, Es))) = true);;
assert (isIncludedIn (Cs(1, Cs(2, Es))) (Cs(1, Cs(2, Es))) = true);;
assert (isIncludedIn (Cs(1, Cs(2, Cs(3, Es)))) (Cs(3, Cs(2, Es))) = true);;
assert (isIncludedIn (Cs(1, Cs(2, Cs(3, Es)))) (Cs(4, Cs(3, Es))) = false);;

(* addElementToSet: *)
assert (addElementToSet 3 (Cs(1, Cs(2, Es))) = Cs(3, Cs(1, Cs(2, Es))));;
assert (addElementToSet 2 (Cs(1, Cs(2, Es))) = Cs(1, Cs(2, Es)));;

(* supElementFromSet: *)
assert (supElementFromSet 3 (Cs(1, Cs(2, Es))) = Cs(1, Cs(2, Es)));;

assert (supElementFromSet 3 (Cs(1, Cs(2, Cs(3, Es)))) = Cs(1, Cs(2, Es)));;
assert (supElementFromSet 2 (Cs(1, Cs(2, Cs(3, Es)))) = Cs(1, Cs(3, Es)));;

(* setsAreEqual: *)
assert (setsAreEqual (Cs(1, Cs(2, Es))) (Cs(1, Cs(2, Es))) = true);;
assert (setsAreEqual (Cs(1, Cs(2, Es))) (Cs(2, Cs(1, Es))) = true);;
assert (setsAreEqual (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es))) = false);;

(* intersection: *)
assert (intersection (Cs(1, Cs(2, Es))) (Cs(1, Cs(2, Es))) = Cs(1, Cs(2, Es)));;
assert (intersection (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es))) = Cs(1, Es));;
assert (intersection (Cs(1, Cs(2, Es))) (Cs(0, Cs(3, Es))) = Es);;

(* union: *)
assert (union (Cs(1, Cs(2, Es))) (Cs(3, Cs(4, Es))) = Cs(4, Cs(3, Cs(1, Cs(2, Es)))));;
assert (union (Cs(1, Cs(2, Es))) (Cs(1, Cs(4, Es))) = Cs(4, Cs(1, Cs(2, Es))));;
assert (union (Cs(1, Cs(2, Es))) (Cs(1, Cs(2, Es))) = Cs(1, Cs(2, Es)));;
assert (union Es (Cs(2, Cs(3, Es))) = (Cs(3, Cs(2, Es))));;

(* difference: *)
assert (difference (Cs(1, Cs(2, Es))) (Cs(1, Cs(2, Es))) = Es);;
assert (difference (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es))) = Cs(2, Es));;
assert (difference (Cs(1, Cs(2, Es))) (Cs(4, Cs(3, Es))) = Cs(1, Cs(2, Es)));;

(* symetricalDifference: *)
assert (symetricalDifference (Cs(1, Cs(2, Es))) (Cs(1, Cs(2, Es))) = Es);;
assert (symetricalDifference (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es))) = Cs(3, Cs(2, Es)));;
assert (symetricalDifference (Cs(0, Cs(2, Es))) (Cs(1, Cs(3, Es))) = Cs(3, Cs(1, Cs(0, Cs(2, Es)))));;
