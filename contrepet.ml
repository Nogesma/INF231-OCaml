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

let add dic word =  Cs((List.init (String.length word) (String.get word)), dic);;

let cst_DICO = Es;;

let cst_DICO = add cst_DICO "quelle";;
let cst_DICO = add cst_DICO "ministre";;
let cst_DICO = add cst_DICO "seche";;
let cst_DICO = add cst_DICO "sinistre";;
let cst_DICO = add cst_DICO "meche";;