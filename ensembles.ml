(*
Ocaml Project - INF231 - 2019

Benjamin Phénix
Mano Ségransan

*)

(* Q1: *)

type 'a set =
  | Es                   (* E: empty, s: set *)
  | Cs of 'a * 'a set;;  (* C: constructor *)


let rec cardinal = function Es -> 0 | Cs (_, l) -> cardinal l + 1;;

let rec isInSet e = function Es -> false | Cs (x, lp) -> e = x || isInSet e lp;;

let rec isIncludedIn s = function Es -> true | Cs (x, lp) -> isInSet x s && isIncludedIn s lp;;

let addElementToSet e s  = if isInSet e s then s else Cs (e, s);;

let rec supElementFromSet e = function Es -> Es | Cs (x, lp) ->
  if e = x
    then lp
    else Cs(x, supElementFromSet e lp);;

let rec setsAreEqual s1 s2 = isIncludedIn s1 s2 && isIncludedIn s2 s1;;

let rec intersection s = function Es -> Es | Cs (x, lp) ->
  if isInSet x s
    then Cs(x, intersection s lp)
    else intersection s lp;;

let rec union s = function Es -> s | Cs (x, lp) -> union (addElementToSet x s) lp;;

let rec difference s = function Es -> s | Cs (x, lp) -> difference (supElementFromSet x s) lp;;

let symetricalDifference s1 s2 = difference (union s1 s2) (intersection s1 s2);;

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
