(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)


(* Q4: *)

(*
 * MS: MultiSet
 * ME: MultiElement
 *)

type 'a multiElement = 'a * int;;
type 'a multiSet = 'a multiElement list;;

(* return a tuple (a,b) where a = numbers of differents elements/b = numbers of elements *)
let cardinal (ms :'a multiSet) :int * int =
  (
    List.length ms,
    List.fold_left (+) 0 (List.map (fun (_, b) -> b) ms)
  );;

(* Itterate the set and return the number assiocated to the element e or 0 if e is not in the set *)
let rec occurences (e :'a) =
  function
  | (a, b)::tl ->
    if a = e
    then b
    else occurences e tl
  | [] -> 0
;;

(* boolegyan function wich return true if an element e is in a MultiElement
   from the set, otherwise it will return false *)
let rec isInMs (e :'a) =
  function
  | (a, b)::tl -> a = e || isInMs e tl
  | [] -> false
;;

(* helper function *)
(* same as before but will return true only when the element is present at least b times *)
let rec isMeInMs ((x, y) :'a multiElement) =
  function
  | (a, b)::tl -> (x = a && y <= b) || isMeInMs (x, y) tl
  | [] -> false
;;

(* Boolean function wich return true if every tuple (a,b) in a first set,
   the element a is present at least b times in the second set *)
let rec isIncludedIn (ms :'a multiSet) =
  function
  | (a, b)::tl -> (isMeInMs (a, b) ms) && isIncludedIn ms tl
  | [] -> true
;;

let rec addMeToMs ((x, y) :'a multiElement) =
  function
  | (a, b)::tl ->
    if a = x
    then (a, (b + y))::tl
    else (a, b)::(addMeToMs (x, y) tl)
  | [] -> [(x, y)]
;;

(* Function wich delet a MultiElement from a List if the element is in less quantity than the removing values,
   If not, it will return a new MultiElement with less Element and the rest of the list *)
let rec supMeFromMs ((x, y) :'a multiElement) =
  function
  | (a, b)::tl ->
    if a = x
    then
      if y = 0 || y >= b
      then tl
      else (a, (b - y))::tl
    else (a, b)::(supMeFromMs (x, y) tl)
  | [] -> []
;;

(* Function wich verify if each MultiSet is include in one another and return a boolean *)
let msAreEqual (ms1 :'a multiSet) (ms2 :'a multiSet) :bool = isIncludedIn ms1 ms2 && isIncludedIn ms2 ms1;;

(* Function wich return the difference between two MultiSets *)
let rec intersection (ms :'a multiSet) =
  function
  | (a, b)::tl ->
    if isInMs a ms
    then
      let o = occurences a ms in
        (a, if b > o then o else b)::intersection ms tl
    else intersection ms tl
  | [] -> []
;;

let rec union (ms :'a multiSet) =
  function
  | (a, b)::tl -> union (addMeToMs (a, b) ms) tl
  | [] -> ms
;;

let rec diff1 (ms :'a multiSet) =
  function
  | (a, b)::tl-> diff1 (supMeFromMs (a, 0) ms) tl
  | [] -> ms
;;


let rec diff2 (ms :'a multiSet) =
  function
  | (a, b)::tl -> diff2 (supMeFromMs (a, b) ms) tl
  | [] -> ms
;;

let diffSym (ms1 :'a multiSet) (ms2 :'a multiSet) :'a multiSet = diff1 (union ms1 ms2) (intersection ms1 ms2);;

(* Tests: *)

(* cardinal: *)
assert (cardinal [(1, 2); (2, 1); (3, 5)] = (3, 8));;
assert (cardinal [] = (0, 0));;

(* occurences: *)
assert (occurences 3 [(1, 2); (2, 1); (3, 5)] = 5);;
assert (occurences 0 [(1, 2); (2, 1); (3, 5)] = 0);;

(* isInMs: *)
assert (isInMs 2 [(1, 2); (2, 1); (3, 5)] = true);;
assert (isInMs 0 [(1, 2); (2, 1); (3, 5)] = false);;

(* isMeInMs *)
assert (isMeInMs (1, 1) [(1, 2); (2, 1); (3, 5)] = true);;
assert (isMeInMs (3, 5) [(1, 2); (2, 1); (3, 5)] = true);;
assert (isMeInMs (1, 5) [(1, 2); (2, 1); (3, 5)] = false);;
assert (isMeInMs (5, 1) [(1, 2); (2, 1); (3, 5)] = false);;

(* isIncludedIn: *)
assert (isIncludedIn [(1, 2); (2, 1); (3, 5)] [(1, 1); (3, 5)] = true);;
assert (isIncludedIn [(1, 2); (2, 1); (3, 5)] [(3, 5); (1, 1)] = true);;
assert (isIncludedIn [(1, 2); (2, 1); (3, 5)] [(1, 2); (2, 1); (3, 5)] = true);;
assert (isIncludedIn [(1, 2); (2, 1); (3, 5)] [(1, 2); (2, 2)] = false);;
assert (isIncludedIn [(1, 1); (2, 1); (3, 5)] [(1, 2)] = false);;

(* addMeToMs: *)
assert (addMeToMs (5, 1) [(1, 2); (2, 1); (3, 5)] = [(1, 2); (2, 1); (3, 5); (5, 1)]);;
assert (addMeToMs (5, 2) [(1, 2); (5, 1); (3, 5)] = [(1, 2); (5, 3); (3, 5)]);;

(* supMeFromMs: *)
assert (supMeFromMs (5, 2) [(1, 2); (5, 4); (3, 5)] = [(1, 2); (5, 2); (3, 5)]);;
assert (supMeFromMs (5, 0) [(1, 2); (5, 4); (3, 5)] = [(1, 2); (3, 5)]);;
assert (supMeFromMs (5, 0) [(1, 2); (5, 8); (3, 5)] = [(1, 2); (3, 5)]);;

(* msAreEqual: *)
assert (msAreEqual [(1, 2); (5, 4); (3, 5)] [(5, 4); (1, 2); (3, 5)] = true);;
assert (msAreEqual [(1, 2); (5, 4); (3, 5)] [(2, 4); (1, 2); (3, 5)] = false);;

(* intersection: *)
assert (intersection [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 2); (3, 5)] = [(1, 2); (3, 3)]);;
assert (intersection [(1, 2); (5, 4); (3, 3)] [(1, 2); (5, 4); (3, 3)] = [(1, 2); (5, 4); (3, 3)]);;
assert (intersection [(1, 2); (5, 4); (3, 3)] [(2, 4); (6, 2); (0, 5)] = []);;

(* union: *)
assert (union [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 2); (3, 5)] = [(1, 4); (5, 4); (3, 8); (2, 4)]);;
assert (union [(1, 2); (5, 4)] [(1, 2); (5, 4)] = [(1, 4); (5, 8)]);;
assert (union [] [(2, 4); (1, 2); (3, 5)] = [(2, 4); (1, 2); (3, 5)]);;

(* diff1 *)
assert (diff1 [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 2); (3, 5)] = [(5, 4)]);;
assert (diff1 [(1, 2); (5, 4); (3, 3)] [(2, 4); (6, 2); (0, 5)] = [(1, 2); (5, 4); (3, 3)]);;

(* diff2 *)
assert (diff2 [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 2); (3, 5)] = [(5, 4)]);;
assert (diff2 [(1, 2); (5, 4); (3, 3)] [(2, 4); (6, 2); (0, 5)] = [(1, 2); (5, 4); (3, 3)]);;

(* diffSym *)
assert (diffSym [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 2); (3, 5)] = [(5, 4); (2, 4)]);;
assert (diffSym [(1, 2); (5, 4); (3, 3)] [(2, 4); (6, 2); (0, 5)] = [(1, 2); (5, 4); (3, 3); (2, 4); (6, 2); (0, 5)]);;
