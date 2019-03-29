(* Q17: *)

(* Sets *)

type 'a orderedSet =
  | Es                  (* E: empty, s: set *)
  | Cs of 'a * 'a orderedSet   (* Cs: constructor *)
;;

let rec isInSet e =
  function
  | Cs (x, lp) -> if e < x then false else e = x || isInSet e lp
  | Es -> false
;;

let rec addElementToSet e =
  function
  | Cs (x, lp) ->
    if e > x
      then Cs(x, addElementToSet e lp)
    else if e = x
      then Cs(x, lp)
      else Cs(e, Cs(x, lp))
  | Es -> Cs(e, Es)
;;

let rec supElementFromSet e =
  function
  | Cs (x, lp) ->
    if e > x
      then Cs(x, supElementFromSet e lp)
    else if e = x
      then lp
      else Cs(x, lp)
  | Es -> Es
;;


let setsAreEqual s1 s2 = s1 = s2;;

(* The intersection, union, difference and, symetricalDifference
 * functions don't need to change, as they are using the add, sup or, isInSet functions.
 *)


assert (isInSet 2 (Cs(1, Cs(2, Es))) = true);;
assert (isInSet 4 (Cs(1, Cs(2, Es))) = false);;

assert (addElementToSet 3 (Cs(1, Cs(2, Es))) = Cs(1, Cs(2, Cs(3, Es))));;
assert (addElementToSet 2 (Cs(1, Cs(2, Es))) = Cs(1, Cs(2, Es)));;

assert (supElementFromSet 3 (Cs(1, Cs(2, Es))) = Cs(1, Cs(2, Es)));;
assert (supElementFromSet 3 (Cs(1, Cs(2, Cs(3, Es)))) = Cs(1, Cs(2, Es)));;

assert (setsAreEqual (Cs(1, Cs(2, Es))) (Cs(1, Cs(2, Es))) = true);;
assert (setsAreEqual (Cs(1, Cs(2, Es))) (Cs(1, Cs(3, Es))) = false);;

(* Multi-sets *)

type 'a multiElement = 'a * int;;
type 'a orderedMultiSet = 'a multiElement list;;

let rec occurences e =
  function
  | (a, b)::lp ->
    if e > a
      then occurences e lp
    else if e = a 
      then b
      else 0
  | [] -> 0
;;

let rec isInMS e =
  function
  | (a, b)::lp -> if e < a then false else a = e || isInMS e lp
  | [] -> false
;;

let rec isMEInMS (a, b) =
  function
  | (x, y)::lp -> if x < a then false else (a = x && b <= y) || isMEInMS (a, b) lp
  | [] -> false
;;

let rec add (x, n) =
  function
  | (a, b)::lp ->
    if x > a
      then (a, b)::(add (x, n) lp)
    else if x = a
      then (a, (b + n))::lp
      else (x, n)::(a, b)::lp
  | [] -> [(x, n)]
;;

let rec del (x, n) =
  function
  | (a, b)::lp ->
  if x > a
    then (a, b)::(del (x, n) lp)
  else if x = a
    then 
      if n = 0 || n >= b
        then lp
        else (a, (b - n))::lp

    else (a, b)::lp
  | [] -> []
;;

let equality ms1 ms2 =  ms1 = ms2;;

assert (occurences 3 [(1, 2); (2, 1); (3, 5)] = 5);;
assert (occurences 0 [(1, 2); (2, 1); (3, 5)] = 0);;

assert (isInMS 2 [(1, 2); (2, 1); (3, 5)] = true);;
assert (isInMS 0 [(1, 2); (2, 1); (3, 5)] = false);;

assert (isMEInMS (1, 1) [(1, 2); (2, 1); (3, 5)] = true);;
assert (isMEInMS (1, 5) [(1, 2); (2, 1); (3, 5)] = false);;

assert (add (3, 1) [(1, 2); (2, 1); (3, 5)] = [(1, 2); (2, 1); (3, 6)]);;
assert (add (2, 2) [(1, 2); (3, 5)] = [(1, 2); (2, 2); (3, 5)]);;

assert (del (5, 2) [(1, 2); (3, 5); (5, 4)] = [(1, 2); (3, 5); (5, 2)]);;
assert (del (5, 0) [(1, 2); (3, 5); (5, 8);] = [(1, 2); (3, 5)]);;

assert (equality [(1, 2); (3, 5); (5, 4)] [(1, 2); (3, 5); (5, 4)] = true);;
assert (equality [(1, 2); (3, 5); (5, 4)] [(1, 2); (2, 4); (3, 5)] = false);;
