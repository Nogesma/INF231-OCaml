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

let rec decompose ?i:(i=0) (w :word) :'a list =
  if i = (List.length w)
  then []
  else (sublist w 0 i, (List.nth w i), sublist w (i + 1) (List.length w))::(decompose ~i:(i + 1) w)
;;

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
