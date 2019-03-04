(*
Ocaml Project - INF231 - 2019

Benjamin Phénix
Mano Ségransan

*)

(* Q1: *)

type 'a ensemble =                  (* ’a est le réservoir d’éléments *)
        | Es                        (* V mis pour «vide», e pour «ensemble» *)
        | Cs of 'a * 'a ensemble;;  (* C mis pour « constructeur » *)


let rec cardinal = function Es -> 0 | Cs (_, l) -> cardinal l + 1;;

let rec isInList e = function Es -> false | Cs (x, lp) -> e = x || (e > x) && isInList e lp;;

let rec isIncludedIn l = function Es -> true | Cs (x, lp) -> isInList x l && isIncludedIn l lp;;

let ajoute (a : _) (e : 'a ensemble) = if isInList a e then e else Cs(a,e);;

let rec supFromSet e = function Es -> Es | Cs (x, lp) -> lp ;;

