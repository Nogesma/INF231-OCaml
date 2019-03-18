(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)

(* Q3: *)

let cardinal l = List.length l;;

let isElementInList e l = List.exists (fun x -> x = e) l;;

let isListIncludedIn l1 l2 = List.for_all (fun x -> isElementInList x l1) l2;;

let supElementFromList e l = List.filter (fun x -> x <> e) l;;

let intersection l1 l2 = List.filter (fun x -> isElementInList x l1) l2;;

let rec difference l1 l2 = List.filter (fun x -> not (isElementInList x l2)) l1;;

let union l1 l2 = l1 @ difference l2 l1;;

(* Tests: OK *)

cardinal [1; 2];;  (* 2 *)
cardinal [];; (* 0 *)

isElementInList 2 [1; 2];; (* true *)
isElementInList 4 [1; 2];; (* false *)

isListIncludedIn [1; 2; 3] [2; 3];; (* true *)
isListIncludedIn [1; 2; 3] [4; 3];; (* false *)

supElementFromList 3 [1; 2];; (* [1; 2] *)
supElementFromList 3 [1; 2; 3];; (* [1; 2] *)

intersection [1; 2] [1; 3];; (* [1] *)
intersection [1; 2] [0; 3];; (* [] *)

union [1; 2] [1; 3];; (* [1; 2; 3] *)
union [1] [2; 3];; (* [1; 2; 3] *)

difference [1; 2] [1; 3];; (* [2] *)
difference [1; 2] [4; 3];; (* [1; 2] *)
