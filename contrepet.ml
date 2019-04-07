(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)

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

let wordToList (w :string) :char list = (List.init (String.length w) (String.get w));;
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

(* Q10: *)

type phrase = word list;;

(* Q11: *)

let deletePrefix (w1 :word) (w2 :word) :word * word =
  if (List.hd w1) = (List.hd w2)
  then ((List.tl w1),(List.tl w2))
  else (w1, w2)
;;

let sameSuffix (w1 :word) (w2 :word) :bool =
  w1 <> w2 && (List.tl w1) = (List.tl w2)
;;

(* Q12: *)

let wordsAreSpoonerisms ((m1, m1') :word * word) ((m2, m2') :word * word) :bool =
  sameSuffix m1 m2
  && sameSuffix m1' m2'
  && deletePrefix m1 m2' = deletePrefix m2 m1'
;;

(* Q13: *)

let phraseAreSpoonerisms (p1 :phrase) (p2 :phrase) :bool =
  let x = (List.filter (fun e -> List.for_all (fun x -> x <> e) p2) p1) in
  let y = (List.filter (fun e -> List.for_all (fun x -> x <> e) p1) p2) in
  List.length x = 2
  && List.length y = 2
  && wordsAreSpoonerisms (List.nth x 0, List.nth x 1) (List.nth y 0, List.nth y 1)
 ;;

(* Q14: *)

(* range is a subfunction used by sublist, which creates an array of numbers ranging from i to j *)
let rec range ?i:(i=0) (j :int) :int list =
  if i >= j
  then []
  else i :: (range ~i:(i + 1) j)
;;

(* sublist is a subfunction used by splitWord, which return the part of the given list within the given indexes *)
let sublist (l :word) (s: int) (e :int) :word =
  List.map (fun (_, x) -> x)
    (List.filter (fun (i, _) -> i >= s && i < e)
      (List.combine (range (List.length l)) l))
;;

(* The recursive function wich deals with the word and separation and put the sublist in another list *)
let rec splitWord ?i:(i=0) (w :word) :'a list =
  if i = (List.length w)
  then []
  else (sublist w 0 i, (List.nth w i), sublist w (i + 1) (List.length w))::(splitWord ~i:(i + 1) w)
;;

(* Q15: *)
let swap
  ((p1, l1, s1) :word * letter * word)
  ((p2, l2, s2) :word * char * word)
  :(word * char * word) * (word * letter * word)
  = ((p1, l2, s1), (p2, l1, s2))
;;

(* Q16: *)
(* let contrepet dic phr ?start(start=[])=
  (*
   * insert a function wich:
   * Define phr1 as a contrepet if not possible then phr1 = phr
   * Test for a certain word if by decomposing the rest of phr if there is a word in dic and if so do the back check
   * Define next_word as the next word to check
   * Careful about the presence of same contrepet in the final list (need to addapt isInList from "refracor.ml")
   *)
  if (phraseContrepet phr phr1)&& then phr1::(contrepet dic phr next_word) else (contrepet dic phr next_word) *)



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

assert (swap (List.nth (splitWord (wordToList "test")) 1) (List.nth (splitWord (wordToList "sint")) 1) = ((['t'], 'i', ['s'; 't']), (['s'], 'e', ['n'; 't'])));;
assert (swap (List.nth (splitWord (wordToList "sin")) 0) (List.nth (splitWord (wordToList "min")) 0) = (([], 'm', ['i'; 'n']), ([], 's', ['i'; 'n'])));;




