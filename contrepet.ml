(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)

open List;;
(* Q6: *)

type letter = char;;
type word = letter list;;

(* Q7: *)

type 'a set =
  | Es                  (* E: empty, s: set *)
  | Cs of 'a * 'a set   (* C: constructor *)
;;

(* Q7: *)

type dictionnary = word set;;

(* Q8: *)

let wordToList (w :string) :char list = (init (String.length w) (String.get w));;
let addWordToDic (dic :dictionnary) (word :string) :dictionnary =  Cs((wordToList word), dic);;

(* Q9: *)

let cst_DICO = Es;;
let cst_DICO = addWordToDic cst_DICO "quelle";;
let cst_DICO = addWordToDic cst_DICO "ministre";;
let cst_DICO = addWordToDic cst_DICO "seche";;
let cst_DICO = addWordToDic cst_DICO "sinistre";;
let cst_DICO = addWordToDic cst_DICO "meche";;
let cst_DICO = addWordToDic cst_DICO "il";;
let cst_DICO = addWordToDic cst_DICO "fait";;
let cst_DICO = addWordToDic cst_DICO "chaud";;
let cst_DICO = addWordToDic cst_DICO "et";;
let cst_DICO = addWordToDic cst_DICO "beau";;
let cst_DICO = addWordToDic cst_DICO "rame";;
let cst_DICO = addWordToDic cst_DICO "bare";;
let cst_DICO = addWordToDic cst_DICO "tare";;

(* Q10: *)

type phrase = word list;;

(* Q11: *)

let deletePrefix (w1 :word) (w2 :word) :word * word =
  if (hd w1) = (hd w2)
  then ((tl w1),(tl w2))
  else (w1, w2)
;;

let sameSuffix (w1 :word) (w2 :word) :bool =
  w1 <> w2 && (tl w1) = (tl w2)
;;


let samePrefix (w1 :word) (w2 :word) :bool =
  w1 <> w2 && (hd w1) = (hd w2)
;;

(* Q12: *)

let wordsAreSpoonerisms ((m1, m1') :word * word) ((m2, m2') :word * word) :bool =
  sameSuffix m1 m2
  && sameSuffix m1' m2'
  && deletePrefix m1 m2' = deletePrefix m2 m1'
;;

(* Q13: *)

let phraseAreSpoonerisms (p1 :phrase) (p2 :phrase) :bool =
  let x = (filter (fun e -> for_all (fun x -> x <> e) p2) p1) in
  let y = (filter (fun e -> for_all (fun x -> x <> e) p1) p2) in
  length x = 2
  && length y = 2
  && wordsAreSpoonerisms (nth x 0, nth x 1) (nth y 0, nth y 1)
 ;;

(* Q14: *)

(* range is a subfunction used by sublist, which creates an array of numbers ranging from i to j *)
let rec range ?i:(i=0) (j :int) :int list =
  if i >= j
  then []
  else i :: (range ~i:(i + 1) j)
;;

(* sublist is a subfunction used by splitWord,
 * which return the part of the given list within the given indexes
 *)
let sublist (l :word) (s: int) (e :int) :word =
  map (fun (_, x) -> x)
    (filter (fun (i, _) -> ((i >= s)&&(i < e)))
      (combine (range (length l)) l))
;;

(* The recursive function wich deals with the word and separation and put the sublist in another list *)
let rec splitWord ?i:(i=0) (w :word) :'a list =
  if i = (length w)
  then []
  else (sublist w 0 i, (nth w i), sublist w (i + 1) (length w))::(splitWord ~i:(i + 1) w)
;;

(* Q15: *)
let swap
  ((p1, l1, s1) :word * letter * word)
  ((p2, l2, s2) :word * char * word)
  :word * word
  = p1@l2::s1, p2@l1::s2
;;

(* Q16: *)

let rec isInDic (e :'a) =
  function
  | Cs (hd, tl) -> e = hd || isInDic e tl
  | Es -> false
;;

let rec inDic (dic:dictionnary) =
  function
  | hd::tl ->
    if isInDic hd dic
    then hd::(inDic dic tl)
    else (inDic dic tl)
  | [] -> []
;;

let rec listSwap w =
  function
  | hd::tl -> let (w1, w2) = swap w hd in w1::listSwap w tl
  | [] -> []
;;

let spoonerism dic phr =
  map hd (map splitWord phr)
  (* map listSwap (map firstElement (map splitWord phr)) *)
;;



(* let fonctionTropCoool a b = map (fun y -> map (fun x -> x * y) a) b;; *)

(*
# let fonctionTropCoool a b = map (fun y -> map (fun x -> x * y) a) b;;
val fonctionTropCoool : int list -> int list -> int list list = <fun>
# fonctionTropCoool [1; 2; 3] [4; 5; 6];;
- : int list list = [[4; 8; 12]; [5; 10; 15]; [6; 12; 18]]
*)



(*
# fonctionTropCoool [1; 2; 3] [2; 9; 1];;
- : bool list list =
[[false; true; false]; [false; false; false]; [true; false; false]]
*)



(* TESTS *)
assert (wordToList "test" = ['t'; 'e'; 's'; 't']);;
assert (wordToList "" = []);;

assert (addWordToDic Es "test" = Cs(['t'; 'e'; 's'; 't'], Es));;
assert (addWordToDic (Cs(['t'; 'e'; 's'; 't'], Es)) "mot" = Cs(['m'; 'o'; 't'], Cs(['t'; 'e'; 's'; 't'], Es)));;

assert (deletePrefix (wordToList "test") (wordToList "ton") = (['e'; 's'; 't'], ['o'; 'n']));;
assert (deletePrefix (wordToList "test") (wordToList "on") = (['t'; 'e'; 's'; 't'], ['o'; 'n']));;

assert (sameSuffix (wordToList "test") (wordToList "on") = false);;
assert (sameSuffix (wordToList "test") (wordToList "cest") = true);;

assert (wordsAreSpoonerisms ((wordToList "ministre"),(wordToList "seche")) ((wordToList "sinistre"),(wordToList "meche")) = true);;
assert (wordsAreSpoonerisms ((wordToList "sinistre"),(wordToList "seche")) ((wordToList "sinistre"),(wordToList "meche")) = false);;
assert (wordsAreSpoonerisms ((wordToList "linistre"),(wordToList "seche")) ((wordToList "sinistre"),(wordToList "meche")) = false);;

assert (phraseAreSpoonerisms [(wordToList "quelle"); (wordToList "min"); (wordToList "seche")] [(wordToList "quelle"); (wordToList "sin"); (wordToList "meche")] = true);;
assert (phraseAreSpoonerisms [(wordToList "quelle"); (wordToList "sin"); (wordToList "seche")] [(wordToList "quelle"); (wordToList "min"); (wordToList "meche")] = false);;

assert (range 5 = [0; 1; 2; 3; 4]);;
assert (range ~i:3 5 = [3; 4]);;
assert (range ~i:6 5 = []);;

assert (sublist (wordToList "test") 0 0 = []);;
assert (sublist (wordToList "test") 0 3 = ['t'; 'e'; 's']);;
assert (sublist (wordToList "test") 2 3 = ['s']);;

assert (splitWord (wordToList "test") = [([], 't', ['e'; 's'; 't']); (['t'], 'e', ['s'; 't']); (['t'; 'e'], 's', ['t']); (['t'; 'e'; 's'], 't', [])]);;
assert (splitWord (wordToList "sin") = [([], 's', ['i'; 'n']); (['s'], 'i', ['n']); (['s'; 'i'], 'n', [])]);;

assert (swap (nth (splitWord (wordToList "test")) 1) (nth (splitWord (wordToList "sint")) 1) = (['t'; 'i'; 's'; 't'], ['s'; 'e'; 'n'; 't']));;
assert (swap (nth (splitWord (wordToList "sin")) 0) (nth (splitWord (wordToList "min")) 0) = (['m'; 'i'; 'n'], ['s'; 'i'; 'n']));;




