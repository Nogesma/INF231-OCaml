(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)

(* Q1: *)


(* Definition of our set wich is recursive *)
type 'a set =
    | Es                   (* E: empty, s: set *)
    | Cs of 'a * 'a set;;  (* C: constructor *)


(* Recursively call the function while adding 1 until Es is reached *)
let rec cardinal =
    function
        Es -> 0
        | Cs (_, l) -> cardinal l + 1;;


(* Check each element of the set to see if it matches with e *)
let rec isInSet e =
    function
        Es -> false
        | Cs (x, lp) -> e = x || isInSet e lp;;


(* Check if each element of the second set is included in the first *)
let rec isIncludedIn s =
    function
        Es -> true
        | Cs (x, lp) -> isInSet x s && isIncludedIn s lp;;


(* If element is already in set, does nothing, otherwise, add the element to the set *)
let addElementToSet e s  =
    if isInSet e s
        then s
        else Cs (e, s);;


(* Iterates over the set until it reaches the end whilst checking if the current element is equal to e,
 * if it is, call the function again without the current element
 *)
let rec supElementFromSet e =
    function
        Es -> Es
        | Cs (x, lp) ->
            if e = x
                then lp
                else Cs(x, supElementFromSet e lp);;


(* test if each set is included in the other *)
let setsAreEqual s1 s2 = isIncludedIn s1 s2 && isIncludedIn s2 s1;;


(* check if each element of the second set is in the first, if it is add it to the returned set *)
let rec intersection s =
    function
        Es -> Es
        | Cs (x, lp) ->
            if isInSet x s
                then Cs(x, intersection s lp)
                else intersection s lp;;


(* add each element of the second set to the first *)
let rec union s =
    function
        Es -> s
        | Cs (x, lp) -> union (addElementToSet x s) lp;;


(* deletes every element of the second set that are in the first *)
let rec difference s =
    function
        Es -> s
        | Cs (x, lp) -> difference (supElementFromSet x s) lp;;


(* difference of the union and the intersection of two sets *)
let symetricalDifference s1 s2 = difference (union s1 s2) (intersection s1 s2);;


(* Tests: *)

assert (cardinal (Cs(1, Cs(2, Es))) = 2);;
assert (cardinal (Es) = 0);;

assert (isInSet 2 (Cs(1, Cs(2, Es))) = true);;
assert (isInSet 4 (Cs(1, Cs(2, Es))) = false);;

assert (isIncludedIn (Cs(1, Cs(2, Cs(3, Es)))) (Cs(2, Cs(3, Es))) = true);;
assert (isIncludedIn (Cs(1, Cs(2, Cs(3, Es)))) (Cs(4, Cs(3, Es))) = false);;


assert (addElementToSet 3 (Cs(1, Cs(2, Es))) = Cs(3, Cs(1, Cs(2, Es))));;
assert (addElementToSet 2 (Cs(1, Cs(2, Es))) = Cs(1, Cs(2, Es)));;

assert (supElementFromSet 3 (Cs(1, Cs(2, Es))) = Cs(1, Cs(2, Es)));;
assert (supElementFromSet 3 (Cs(1, Cs(2, Cs(3, Es)))) = Cs(1, Cs(2, Es)));;

assert (setsAreEqual (Cs(1, Cs(2, Es))) (Cs(2, Cs(1, Es))) = true);;
assert (setsAreEqual (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es))) = false);;

assert (intersection (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es))) = Cs(1, Es));;
assert (intersection (Cs(1, Cs(2, Es))) (Cs(0, Cs(3, Es))) = Es);;

assert (union (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es))) = Cs(3, Cs(1, Cs(2, Es))));;
assert (union (Cs(1, Es)) (Cs(2, Cs(3, Es))) = Cs(3, Cs(2, Cs(1, Es))));;

assert (difference (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es))) = Cs(2, Es));;
assert (difference (Cs(1, Cs(2, Es))) (Cs(4, Cs(3, Es))) = Cs(1, Cs(2, Es)));;

assert (symetricalDifference (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es))) = Cs(3, Cs(2, Es)));;
assert (symetricalDifference (Cs(0, Cs(2, Es))) (Cs(1, Cs(3, Es))) = Cs(3, Cs(1, Cs(0, Cs(2, Es)))));;
