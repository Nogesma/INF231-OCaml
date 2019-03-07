(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)

type 'a multiElement = 'a * int;;
type 'a multiSet = 'a multiElement list;;


let cardinal ms = (List.length ms, List.fold_left (+) 0 (List.map (fun (_, a) -> a) ms));;

let rec occurences e = function (a, b)::lp -> if a = e then b else occurences e lp | _ -> 0;;

let rec isInMS e = function (a, b)::lp -> a = e || isInMS e lp | _ -> false;;

let rec isMEInMS (a, b) = function (x, y)::lp -> (a = x && b <= y) || isMEInMS (a, b) lp | _ -> false;;

let rec isIncludedIn ms = function (x, y)::lp -> (isMEInMS (x, y) ms) && isIncludedIn ms lp | _ -> true;;
