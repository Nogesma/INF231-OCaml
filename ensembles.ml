(*
Ocaml Project - INF231 - 2019

Benjamin Phénix
Mano Ségransan

*)

(* Q1: *)

type 'a ensemble =                  (* ’a est le réservoir d’éléments *)
        | Ve                        (* V mis pour «vide», e pour «ensemble» *)
        | Ce of 'a * 'a ensemble;;  (* C mis pour « constructeur » *)


let rec cardinal = function Ve -> 0 | Ce (_, l) -> cardinal l + 1;;

let rec isInList e = function Ve -> false | Ce (x, lp) -> e = x || (e > x) && isInList e lp;;

let rec isIncludedIn l = function Ve -> true | Ce (x, lp) -> isInList x l && isIncludedIn l lp;;
