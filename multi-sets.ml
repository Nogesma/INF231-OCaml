(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)


(* Q4: *)


type 'a multiElement = 'a * int;;
type 'a multiSet = 'a multiElement list;;

(* return a tuple (a,b) where a = numbers of differents elements/b = numbers of elements *)
let cardinal ms = (List.length ms, List.fold_left (+) 0 (List.map (fun (_, a) -> a) ms));;

(* Itterate the set and return the number assiocated to the element e or 0 if e is not in the set *)
let rec occurences e = function (a, b)::lp -> if a = e then b else occurences e lp | _ -> 0;;

(* boolegvan function wich return true if an element e is in a MultiElement from the set, otherwise it will return false *)
let rec isInMS e = function (a, b)::lp -> a = e || isInMS e lp | _ -> false;;

(* same as before but will return true only when the element is present at least b times *)
let rec isMEInMS (a, b) = function (x, y)::lp -> (a = x && b <= y) || isMEInMS (a, b) lp | _ -> false;;

(* Boolean function wich return true if every tuple (a,b) in a first set, the element a is present at least b times in the second set *)
let rec isIncludedIn ms = function (x, y)::lp -> (isMEInMS (x, y) ms) && isIncludedIn ms lp | _ -> true;;

