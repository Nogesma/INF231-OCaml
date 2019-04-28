(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)

open List;;
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
    length ms,
    fold_left (+) 0 (map (fun (_, b) -> b) ms)
  );;

(* Itterate the set and return the number assiocated to the element e or 0 if e is not in the set *)
let occurences (e :'a) (ms :'a multiSet) :int = fold_left (fun n (a, b) -> if a = e then b else n) 0 ms;;

(* boolean function wich return true if an element e is in a MultiElement
   from the set, otherwise it will return false *)
let isInMs (e :'a) (ms :'a multiSet) :bool = fold_left (fun z (a, _) -> a = e || z) false ms;;

(* helper function *)
(* same as before but will return true only when the element is present at least b times *)
let isMeInMs ((x, y) :'a multiElement) (ms :'a multiSet) :bool =
  fold_left (fun z (a, b) -> (a = x && y <= b) || z) false ms;;

(* Boolean function wich return true if every tuple (a,b) in a first set,
   the element a is present at least b times in the second set *)
let isIncludedIn (ms1 :'a multiSet) (ms2 :'a multiSet) :bool =
  fold_left (fun z ms -> isMeInMs ms ms1 && z) true ms2;;

let addMeToMs ((x, y) :'a multiElement) (ms :'a multiSet) :'a multiSet =
  if isInMs x ms
  then map (fun (a, b) -> if a = x then (a, (b + y)) else (a, b)) ms
  else (x, y)::ms
;;

(* Function wich delet a MultiElement from a List if the
 * element is in less quantity than the removing values,
 * if not, it will return a new MultiElement with less Element and the rest of the list
 *)

let supMeFromMs ((x, y) :'a multiElement) (ms1 :'a multiSet) :'a multiSet =
  fold_left (fun ms (a, b) ->
    if a = x
    then
      if y = 0 || y >= b
      then ms
      else (a, (b - y))::ms
    else (a, b)::ms
        ) [] ms1
;;

(* Function wich verify if each MultiSet is include in one another and return a boolean *)
let msAreEqual (ms1 :'a multiSet) (ms2 :'a multiSet) :bool = isIncludedIn ms1 ms2 && isIncludedIn ms2 ms1;;

(* Function wich return the difference between two MultiSets *)
let intersection (ms1 :'a multiSet) (ms2 :'a multiSet) :'a multiSet =
  fold_left (
    fun ms (a, b) ->
    let o = occurences a ms2 in
      if o <> 0
      then (a, if b > o then o else b)::ms
      else ms
    ) [] ms1
;;

let union (ms1 :'a multiSet) (ms2 :'a multiSet) :'a multiSet =
  fold_left (fun ms me -> addMeToMs me ms) ms1 ms2
;;

let diff1 (ms1 :'a multiSet) (ms2 :'a multiSet) :'a multiSet =
  fold_left (fun ms (a, b) -> supMeFromMs (a, 0) ms) ms1 ms2
;;

let diff2 (ms1 :'a multiSet) (ms2 :'a multiSet) :'a multiSet =
  fold_left (fun ms (a, b) -> supMeFromMs (a, b) ms) ms1 ms2
;;

let diffSym (ms1 :'a multiSet) (ms2 :'a multiSet) :'a multiSet =
  diff1 (union ms1 ms2) (intersection ms1 ms2)
;;

(* Tests: *)

(* cardinal: *)
assert (cardinal [(1, 2); (2, 1); (3, 5)] = (3, 8));;
assert (cardinal [] = (0, 0));;

(* occurences: *)
assert (occurences 2 [(1, 2); (2, 1); (3, 5)] = 1);;
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
assert (addMeToMs (5, 1) [(1, 2); (2, 1); (3, 5)] = [(5, 1); (1, 2); (2, 1); (3, 5)]);;
assert (addMeToMs (5, 2) [(1, 2); (5, 1); (3, 5)] = [(1, 2); (5, 3); (3, 5)]);;

(* supMeFromMs: *)
assert (supMeFromMs (5, 2) [(1, 2); (5, 4); (3, 5)] = [(3, 5); (5, 2); (1, 2)]);;
assert (supMeFromMs (5, 0) [(1, 2); (5, 4); (3, 5)] = [(3, 5); (1, 2)]);;
assert (supMeFromMs (5, 0) [(1, 2); (5, 8); (3, 5)] = [(3, 5); (1, 2)]);;

(* msAreEqual: *)
assert (msAreEqual [(1, 2); (5, 4); (3, 5)] [(5, 4); (1, 2); (3, 5)] = true);;
assert (msAreEqual [(1, 2); (5, 4); (3, 5)] [(2, 4); (1, 2); (3, 5)] = false);;

(* intersection: *)
assert (intersection [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 2); (3, 5)] = [(3, 3); (1, 2)]);;
assert (intersection [(1, 2); (5, 4); (3, 3)] [(1, 2); (5, 4); (3, 3)] = [(3, 3); (5, 4); (1, 2)]);;
assert (intersection [(1, 2); (5, 4); (3, 3)] [(2, 4); (6, 2); (0, 5)] = []);;

(* union: *)
assert (union [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 2); (3, 5)] = [(2, 4); (1, 4); (5, 4); (3, 8)]);;
assert (union [(1, 2); (5, 4)] [(1, 2); (5, 4)] = [(1, 4); (5, 8)]);;
assert (union [] [(2, 4); (1, 2); (3, 5)] = [(3, 5); (1, 2); (2, 4)]);;

(* diff1 *)
assert (diff1 [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 2); (3, 5)] = [(5, 4)]);;
assert (diff1 [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 1); (0, 5)] = [(3, 3); (5, 4)]);;

(* diff2 *)
assert (diff2 [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 2); (3, 5)] = [(5, 4)]);;
assert (diff2 [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 1); (0, 5)] = [(3, 3); (5, 4); (1, 1)]);;

(* diffSym *)
assert (diffSym [(1, 2); (5, 4); (3, 3)] [(2, 4); (1, 2); (3, 5)] = [(2, 4); (5, 4)]);;
assert (diffSym [(1, 2); (5, 4); (3, 3)] [(2, 4); (6, 2); (0, 5)] = [(0, 5); (6, 2); (2, 4); (1, 2); (5, 4); (3, 3)]);;
