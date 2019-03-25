(*
 * Ocaml Project - INF231 - 2019
 *
 * Benjamin Phénix
 * Mano Ségransan
 *
 *)

(* Q6: *)

type letter = char;;
type word = char list;;

(* Q7: *)

type 'a set =
  | Es                  (* E: empty, s: set *)
  | Cs of 'a * 'a set   (* C: constructor *)
;;

type dictionnary = word set;;

let wordToList w = (List.init (String.length w) (String.get w));;
let add dic word =  Cs((wordToList word), dic);;

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

type phrase = word list;;

let supprimePrefixeCommun w1 w2 =
  if (List.hd w1) = (List.hd w2) 
  then ((List.tl w1), (List.tl w2)) 
  else (w1,w2)
;;

let suffixeEgaux w1 w2 =
  (List.tl w1) = (List.tl w2)
;;

let rec contrepet ph =
  function
  | w::ls -> if w = (List.hd ph) then contrepet (List.tl ph) ls else  (w, (List.hd ph))::(contrepet (List.tl ph) ls)
  | [] -> []
;;

assert (wordToList "test" = ['t'; 'e'; 's'; 't']);;
assert (wordToList "" = []);;

assert (add Es "test" = Cs(['t'; 'e'; 's'; 't'], Es));;
assert (add (Cs(['t'; 'e'; 's'; 't'], Es)) "mot" = Cs(['m'; 'o'; 't'], Cs(['t'; 'e'; 's'; 't'], Es)));;

assert (supprimePrefixeCommun (wordToList "test") (wordToList "ton") = (['e'; 's'; 't'], ['o'; 'n']));;
assert (supprimePrefixeCommun (wordToList "test") (wordToList "on") = (['t'; 'e'; 's'; 't'], ['o'; 'n']));;

assert (suffixeEgaux (wordToList "test") (wordToList "on") = false);;
assert (suffixeEgaux (wordToList "test") (wordToList "cest") = true);;