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

let rec isInList a = function Es -> false | Cs (x, lp) -> a = x || (a > x) && isInList a lp;;

let rec isIncludedIn s = function Es -> true | Cs (x, lp) -> isInList x s && isIncludedIn s lp;;

let addElementToSet e s  = if isInList e s then s else Cs (e, s);;

let supElementFromSet e = function Es -> Es | Cs (x, lp) -> lp;;
