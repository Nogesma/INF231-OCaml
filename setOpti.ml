(* Q17: *)

(* Sets *)

type 'a orderedSet =
  | Es                  (* E: empty, s: set *)
  | Cs of 'a * 'a orderedSet   (* Cs: constructor *)
;;

let rec isInSet (e :'a) =
  function
  | Cs (hd, tl) ->
    if e < hd
    then false
    else e = hd || isInSet e tl
  | Es -> false
;;

let rec addElementToSet (e :'a) =
  function
  | Cs (hd, tl) ->
    if e > hd
    then Cs(hd, addElementToSet e tl)
    else
      if e = hd
      then Cs(hd, tl)
      else Cs(e, Cs(hd, tl))
  | Es -> Cs(e, Es)
;;

let rec supElementFromSet (e :'a) =
  function
  | Cs (hd, tl) ->
    if e > hd
    then Cs(hd, supElementFromSet e tl)
    else
      if e = hd
      then tl
      else Cs(hd, tl)
  | Es -> Es
;;


let setsAreEqual (s1 :'a orderedSet) (s2 :'a orderedSet) :bool = s1 = s2;;

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

let rec occurences (e :'a) =
  function
  | (a, b)::tl ->
    if e > a
    then occurences e tl
    else
      if e = a
      then b
      else 0
  | [] -> 0
;;

let rec isInMS (e : 'a) =
  function
  | (a, b)::tl ->
    if e < a
    then false
    else a = e || isInMS e tl
  | [] -> false
;;

let rec isMEInMS ((x, y) :'a multiElement) =
  function
  | (a, b)::tl ->
    if a < x
    then false
    else (x = a && y <= b) || isMEInMS (x, y) tl
  | [] -> false
;;

let rec add ((x, y) :'a multiElement) =
  function
  | (a, b)::tl ->
    if x > a
    then (a, b)::(add (x, y) tl)
    else
      if x = a
      then (a, (b + y))::tl
      else (x, y)::(a, b)::tl
  | [] -> [(x, y)]
;;

let rec del ((x, y) :'a multiElement) =
  function
  | (a, b)::tl ->
  if x > a
  then (a, b)::(del (x, y) tl)
  else
    if x = a
    then
      if y = 0 || y >= b
      then tl
      else (a, (b - y))::tl
    else (a, b)::tl
  | [] -> []
;;

let equality (ms1 :'a orderedMultiSet) (ms2 :'a orderedMultiSet) :bool =  ms1 = ms2;;

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
