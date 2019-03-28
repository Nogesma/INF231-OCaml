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
let wordToList w = (List.init (String.length w) (String.get w));;
let add dic word =  Cs((wordToList word), dic);;

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
let supprimePrefixeCommun w1 w2 =
  if (List.hd w1) = (List.hd w2) 
  then ((List.tl w1),(List.tl w2))
  else (w1,w2)
;;

let suffixeEgaux w1 w2 =
  w1 <> w2 && (List.tl w1) = (List.tl w2)
;;

(* Q12: *)
let motsSontContrepets (m1,m1_bis) (m2,m2_bis) = 
  suffixeEgaux m1 m2 
  && suffixeEgaux m1_bis m2_bis 
  && supprimePrefixeCommun m1 m2_bis = supprimePrefixeCommun m2 m1_bis
;;

(* Q13: *)
let phraseContrepet p1 p2 = 
  let x = (List.filter (fun e -> List.for_all (fun x -> x <> e) p2) p1) in
  let y = (List.filter (fun e -> List.for_all (fun x -> x <> e) p1) p2) in
  List.length x = 2 
  && List.length y = 2 
  && motsSontContrepets (List.nth x 0, List.nth x 1) (List.nth y 0, List.nth y 1)
 ;;

(* Q14: *)
let rec partieMot w i n = 
  if i <> n then
    (List.nth w i)::partieMot w (i+1) n
  else
    []
;;

(* non recursif *)
let decompose w =
  let temp = [] in
  while List.length temp < List.length w do
    temp = (partieMot w 0 (List.length temp), partieMot w ((List.length temp)-1) (List.length temp), partieMot w (List.length temp) (List.length w))::temp
  done
;;

(* recursif *)
let rec decompose w i =
  if i < List.length w then
    (partieMot w 0 (List.length temp), partieMot w ((List.length temp)-1) (List.length temp), partieMot w (List.length temp) (List.length w))::decompose w i+1
  else []
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

