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

assert (isInSet 2 (Cs(1, Cs(2, Es))) = true);;
assert (isInSet 4 (Cs(1, Cs(2, Es))) = false);;

assert (addElementToSet 3 (Cs(1, Cs(2, Es))) = Cs(1, Cs(2, Cs(3, Es))));;
assert (addElementToSet 2 (Cs(1, Cs(2, Es))) = Cs(1, Cs(2, Es)));;
