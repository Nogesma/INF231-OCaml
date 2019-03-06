(*
Ocaml Project - INF231 - 2019

Benjamin Phénix
Mano Ségransan

*)

(* Q1: *)

type 'a set =
  | Es                   (* E: empty, s: set *)
  | Cs of 'a * 'a set;;  (* C: constructor *)

(* Definition of the type of our list wich is a recusrive type *)


let rec cardinal = function Es -> 0 | Cs (_, l) -> cardinal l + 1;;
(* Recursive function wich add +1 while the first element is different from the Empty element (Es) *)
(* The empty Element is the last so that work *)
(* cardinal is a function wich returns a function that match Es or Cs *)

let rec isInSet e = function Es -> false | Cs (x, lp) -> e = x || isInSet e lp;;
(* Recursive function wich return a bool. True if one of the elements is equal to the argument given. *)
(* False if the function reach the Empty element (Es). *)

let rec isIncludedIn s = function Es -> true | Cs (x, lp) -> isInSet x s && isIncludedIn s lp;;
(* Recursive function wich will test every element of a set to see if it's in the second set *)

let addElementToSet e s  = if isInSet e s then s else Cs (e, s);;
(* Function wich verify if an element is already is a set and return the original set if true *)
(* Or it will return a new set where we've added the element to our set *)

let rec supElementFromSet e = function Es -> Es | Cs (x, lp) ->
  if e = x
    then lp
    else Cs(x, supElementFromSet e lp);;
(* Recusrive function wich will return the rest of our set if the element that we're testing is the same as our argument *)
(* or will continue testing by moving to the next element if it exist *)

let setsAreEqual s1 s2 = isIncludedIn s1 s2 && isIncludedIn s2 s1;;
(* Function that test if each of them is include in the other and return a bool *)

let rec intersection s = function Es -> Es | Cs (x, lp) ->
  if isInSet x s
    then Cs(x, intersection s lp)
    else intersection s lp;;
(* Recursive function wich wil test all elements from a first set and return the ones that are also in a second *)

let rec union s = function Es -> s | Cs (x, lp) -> union (addElementToSet x s) lp;;
(* function wich will add all the elements from a set in another one using our addElementToSet function *)
(* addElementToSet verify if the element is already in a set so there won't be any duplication *)

let rec difference s = function Es -> s | Cs (x, lp) -> difference (supElementFromSet x s) lp;;
(* Recursive function wich will delete every element from a set that are also in a second *)

let symetricalDifference s1 s2 = difference (union s1 s2) (intersection s1 s2);;
(* function wich will remove all the element that are in both sets from the union of thoses two sets *)


(* Tests: OK *)

cardinal (Cs(1, Cs(2, Es)));;  (* 2 *)
cardinal (Es);; (* 0 *)

isInSet 2 (Cs(1, Cs(2, Es)));; (* true *)
isInSet 4 (Cs(1, Cs(2, Es)));; (* false *)

isIncludedIn (Cs(1, Cs(2, Cs(3, Es)))) (Cs(2, Cs(3, Es)));; (* true *)
isIncludedIn (Cs(1, Cs(2, Cs(3, Es)))) (Cs(4, Cs(3, Es)));; (* false *)


addElementToSet 3 (Cs(1, Cs(2, Es)));; (* Cs(3, Cs(1, Cs(2, Es))) *)
addElementToSet 2 (Cs(1, Cs(2, Es)));; (* Cs(1, Cs(2, Es)) *)

supElementFromSet 3 (Cs(1, Cs(2, Es)));; (* Cs(1, Cs(2, Es)) *)
supElementFromSet 3 (Cs(1, Cs(2, Cs(3, Es))));; (* Cs(1, Cs(2, Es)) *)

setsAreEqual (Cs(1, Cs(2, Es))) (Cs(2, Cs(1, Es)));; (* true *)
setsAreEqual (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es)));; (* false *)

intersection (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es)));; (* Cs(1, Es) *)
intersection (Cs(1, Cs(2, Es))) (Cs(0, Cs(3, Es)));; (* Es *)

union (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es)));; (* Cs(3, Cs(1, Cs(2, Es))) *)
union (Cs(1, Es)) (Cs(2, Cs(3, Es)));; (* Cs(3, Cs(2, Cs(1, Es))) *)

difference (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es)));; (* Cs(2, Es) *)
difference (Cs(1, Cs(2, Es))) (Cs(4, Cs(3, Es)));; (* Cs(1, Cs(2, Es)) *)

symetricalDifference (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es)));; (* Cs(3, Cs(2, Es)) *)
symetricalDifference (Cs(0, Cs(2, Es))) (Cs(1, Cs(3, Es)));; (* Cs(3, Cs(1, Cs(0, Cs(2, Es)))) *)
