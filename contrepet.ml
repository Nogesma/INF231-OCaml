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
let add (dic :dictionnary) (word :string) :dictionnary =  Cs((wordToList word), dic);;

(* Q9: *)

let cst_DICO = Es;;
let cst_DICO = add cst_DICO "quelle";;
let cst_DICO = add cst_DICO "ministre";;
let cst_DICO = add cst_DICO "seche";;
let cst_DICO = add cst_DICO "sinistre";;
let cst_DICO = add cst_DICO "meche";;
let cst_DICO = add cst_DICO "il";;
let cst_DICO = add cst_DICO "fait";;
let cst_DICO = add cst_DICO "chaud";;
let cst_DICO = add cst_DICO "et";;
let cst_DICO = add cst_DICO "beau";;

(* Q10: *)

type phrase = word list;;

(* Q11: *)

let supprimePrefixeCommun (w1 :word) (w2 :word) :word * word =
  if (List.hd w1) = (List.hd w2)
  then ((List.tl w1),(List.tl w2))
  else (w1, w2)
;;

let suffixeEgaux (w1 :word) (w2 :word) :bool =
  w1 <> w2 && (List.tl w1) = (List.tl w2)
;;

(* Q12: *)

let motsSontContrepets ((m1, m1') :word * word) ((m2, m2') :word * word) :bool =
  suffixeEgaux m1 m2
  && suffixeEgaux m1' m2'
  && supprimePrefixeCommun m1 m2' = supprimePrefixeCommun m2 m1'
;;

(* Q13: *)

let phraseContrepet (p1 :phrase) (p2 :phrase) :bool =
  let x = (List.filter (fun e -> List.for_all (fun x -> x <> e) p2) p1) in
  let y = (List.filter (fun e -> List.for_all (fun x -> x <> e) p1) p2) in
  List.length x = 2
  && List.length y = 2
  && motsSontContrepets (List.nth x 0, List.nth x 1) (List.nth y 0, List.nth y 1)
 ;;

(* Q14: *)

(* range is a subfunction wich is responsible for decomposing a certain part of a word *)
let rec range ?i:(i=0) (j :int) :int list =
  if i >= j
  then []
  else i :: (range ~i:(i + 1) j)
;;

let sublist (l :word) (s: int) (e :int) :word =
  List.map (fun (_, x) -> x)
    (List.filter (fun (i, _) -> i >= s && i < e)
      (List.combine (range (List.length l)) l))
;;

(* The recursive function wich deals with the word and separation and put the sublist in another list *)
let rec decompose ?i:(i=0) (w :word) :'a list =
  if i = (List.length w)
  then []
  else (sublist w 0 i, (List.nth w i), sublist w (i + 1) (List.length w))::(decompose ~i:(i + 1) w)
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

assert (add Es "test" = Cs(['t'; 'e'; 's'; 't'], Es));;
assert (add (Cs(['t'; 'e'; 's'; 't'], Es)) "mot" = Cs(['m'; 'o'; 't'], Cs(['t'; 'e'; 's'; 't'], Es)));;

assert (supprimePrefixeCommun (wordToList "test") (wordToList "ton") = (['e'; 's'; 't'], ['o'; 'n']));;
assert (supprimePrefixeCommun (wordToList "test") (wordToList "on") = (['t'; 'e'; 's'; 't'], ['o'; 'n']));;

assert (suffixeEgaux (wordToList "test") (wordToList "on") = false);;
assert (suffixeEgaux (wordToList "test") (wordToList "cest") = true);;

assert (motsSontContrepets ((wordToList "ministre"),(wordToList "seche")) ((wordToList "sinistre"),(wordToList "meche")) = true);;
assert (motsSontContrepets ((wordToList "sinistre"),(wordToList "seche")) ((wordToList "sinistre"),(wordToList "meche")) = false);;
assert (motsSontContrepets ((wordToList "linistre"),(wordToList "seche")) ((wordToList "sinistre"),(wordToList "meche")) = false);;

assert (phraseContrepet [(wordToList "quelle"); (wordToList "min"); (wordToList "seche")] [(wordToList "quelle"); (wordToList "sin"); (wordToList "meche")] = true);;
assert (phraseContrepet [(wordToList "quelle"); (wordToList "sin"); (wordToList "seche")] [(wordToList "quelle"); (wordToList "min"); (wordToList "meche")] = false);;

assert (range 5 = [0; 1; 2; 3; 4]);;
assert (range ~i:3 5 = [3; 4]);;
assert (range ~i:6 5 = []);;

assert (sublist (wordToList "test") 0 0 = []);;
assert (sublist (wordToList "test") 0 3 = ['t'; 'e'; 's']);;
assert (sublist (wordToList "test") 2 3 = ['s']);;

assert (decompose (wordToList "test") = [([], 't', ['e'; 's'; 't']); (['t'], 'e', ['s'; 't']); (['t'; 'e'], 's', ['t']); (['t'; 'e'; 's'], 't', [])]);;
assert (decompose (wordToList "sin") = [([], 's', ['i'; 'n']); (['s'], 'i', ['n']); (['s'; 'i'], 'n', [])]);;

assert (swap (List.nth (decompose (wordToList "test")) 1) (List.nth (decompose (wordToList "sint")) 1) = ((['t'], 'i', ['s'; 't']), (['s'], 'e', ['n'; 't'])));;
assert (swap (List.nth (decompose (wordToList "sin")) 0) (List.nth (decompose (wordToList "min")) 0) = (([], 'm', ['i'; 'n']), ([], 's', ['i'; 'n'])));;




