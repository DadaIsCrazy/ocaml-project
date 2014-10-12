(* Un objet musical est :
   - Une note avec une hauteur [0, 127], un volume [0, 127] et une durée (ms).
   - Un silence (ms).
   - Une sequence (une liste d'objet musicaux).
   - Un parallele (une liste d'objet musicaux). *)
type objet_musical =
  Note of (int * int * int)
| Silence of int
| Sequence of objet_musical list
| Parallel of objet_musical list

let exemple = 
  Parallel 
    [Sequence 
	[Note (60, 64, 1000); 
	 Note (64, 64, 500); 
	 Note (62, 64, 500); 
	 Silence 1000; 
	 Note(67, 64, 1000)];  
     Sequence 
       [Note (52, 64, 2000) ; 
	Note (55, 64, 1000) ;
	Note (55, 64, 1000) ]];;

let rec print_tab x =
  match x with
    | 0 -> ()
    | n -> (print_string "\t";
	    print_tab (x - 1));;

(* Affiche un objet musical. *)
let affichage objet =
  let rec loop obj tab =
    match obj with
      | Note (hauteur, volume, duree) -> (print_tab tab;
					  print_string "Note (" ;
					  print_int hauteur;
					  print_string ", ";
					  print_int volume;
					  print_string ", ";
					  print_int duree;
					  print_string ")")
      | Silence duree -> (print_tab tab;
			  print_string "Silence ";
			  print_int duree)
      | Sequence seq -> (print_tab tab;
			 print_string "Sequence";
			 print_string "[\n";
			 (List.map (fun x -> (loop x (tab + 1);
					      print_string ";\n")) seq);
			 print_tab (1 + tab);
			 print_string "]" )
      | Parallel seq -> (print_tab tab;
			 print_string "Parallel";
			 print_string "[\n";
			 (List.map (fun x -> (loop x (tab + 1);
					      print_string ";\n")) seq);
			 print_tab (tab + 1);
			 print_string "]" ) 
  in loop objet 0;
  print_string "\n";;

(* Renvoie la durée totale de obj. *)
let rec duration obj =
  match obj with 
  | Note (_, _, duree) -> duree
  | Silence duree -> duree
  | Sequence seq -> List.fold_left (+) 0 (List.map duration seq)
  | Parallel seq -> List.fold_left max 0 (List.map duration seq);;

(* Fais une copie 1:1 de obj. *)
let rec copy obj =
  match obj with
  | Note x -> Note x
  | Silence duree -> Silence duree
  | Sequence seq -> Sequence (List.map copy seq)
  | Parallel seq -> Parallel (List.map copy seq);;

(* Compte le nombre de notes contenues dans obj. *)
let rec note_count obj =
  match obj with
  | Note x -> 1
  | Silence duree -> 0
  | Sequence seq -> List.fold_left (+) 0 (List.map note_count seq)
  | Parallel seq -> List.fold_left (+) 0 (List.map note_count seq);;

(* Augmente ou diminue la durée de toutes les notes de obj. *)
let rec stretch obj facteur =
  match obj with
  | Note (hauteur, volume, duree) -> Note (hauteur, volume, int_of_float ((float_of_int duree) *. facteur))
  | Silence duree -> Silence (int_of_float ((float_of_int duree) *. facteur))
  | Sequence seq -> Sequence (List.map (fun x -> stretch x facteur) seq)
  | Parallel seq -> Parallel (List.map (fun x -> stretch x facteur) seq);;

(* Renvoie le nombre de temps de obj, selon le tempo. *)
let beats obj tempo =
  (duration obj) / 1000 * (tempo / 60);;

(* Renvoies les notes de obj situées tous les n temps. *)
let beatsList obj n tempo =
  let tmp = ref 0 in
  let rec loop obj n tempo =
    match obj with
    | Note (hauteur, volume, duree) -> (tmp := !tmp + duree;
      (if ((!tmp - duree) / 1000 * (tempo / 60)) mod n = 0 then Note (hauteur, volume, duree) else Silence 0))
    | Silence duree -> (tmp := !tmp + duree;
      (if ((!tmp - duree) / 1000 * (tempo / 60)) mod n = 0 then Silence duree else Silence 0))   
    | Sequence seq -> (let auw = !tmp and return = Sequence (List.map (fun x -> loop x n tempo) seq) in
		      tmp := auw;
		      return)
    | Parallel seq -> Parallel (List.map (fun x -> loop x n tempo) seq) in
  loop obj n tempo;;
      
(* Augmente ou diminue la hauteur de chaque note de obj. *)
let rec transpose obj intervalle = 
  match obj with
  | Note (hauteur, volume, duree) -> Note (hauteur + intervalle, volume, duree)
  | Silence duree -> Silence duree
  | Sequence seq -> Sequence (List.map (fun x -> transpose x intervalle) seq)
  | Parallel seq -> Parallel (List.map (fun x -> transpose x intervalle) seq);;

(* Renverse les notes du morceau obj. *)
let rec retrograde obj =
  match obj with
  | Note x -> Note x
  | Silence duree -> Silence duree
  | Sequence seq -> Sequence (List.map retrograde (List.rev seq))
  | Parallel seq -> Parallel (List.map retrograde (List.rev seq));;

(* Pratique un miroir des notes de obj par rapport à une note de référence. *)
let rec miroir obj note =
  match obj with
  | Note (hauteur, volume, duree) -> Note (note + (note - hauteur), volume, duree)
  | Silence duree -> Silence duree
  | Sequence seq -> Sequence (List.map (fun x -> miroir x note) seq)
  | Parallel seq -> Parallel (List.map (fun x -> miroir x note) seq) ;;

(* Reverse un morceau obj. *)
let rec reverse obj =
  match obj with 
    | Note x -> Note x
    | Silence x -> Silence x
    | Sequence x -> Sequence (List.rev (List.map reverse x))
    | Parallel x -> Parallel (List.rev (List.map reverse x))

(* Concatène un morceau obj à son rétrograde. *)
let rec palindrome (obj: objet_musical) =
  match obj with
    | Note x -> Sequence [(Note x); (Note x)]
    | Silence x -> Sequence [(Silence x); (Silence x)]
    | Sequence x -> Sequence [(Sequence x); reverse (Sequence x)]
    | Parallel x -> Sequence [ Parallel x; reverse (Parallel x)];;

(* Transforme une note en accord selon une liste de transposition liste_obj. *)
let chordify note liste_obj =
  match note with
  | Note (hauteur, volume, duree) -> Parallel (List.map (fun x -> Note (hauteur + x, volume, duree)) liste_obj);;

(* Applique chordify à l'ensemble des notes d'un morceau obj. *)
let rec chordifyList obj transfo =
  match obj with
    | Note x -> chordify (Note x) transfo
    | Silence x -> Silence x
    | Parallel seq -> Parallel (List.map (fun x -> chordifyList x transfo) seq)
    | Sequence seq -> Sequence (List.map (fun x -> chordifyList x transfo) seq);;

(* Prends un accord et le transforme en séquence suivant un entier de décalage decal. *)
let sequencify accord decal =
  match accord with 
    | Parallel seq -> Sequence (List.map (fun x -> match x with Note (h, v, d) -> Note (h, v, decal)) seq) ;;

(* Supprime un élément d'une liste. *)
let suppr_elem ls n =
  let rec loop l nb res =
    match nb with
      | 0 -> (List.append (List.rev res) (List.tl l))
      | x -> loop (List.tl l) (x - 1) ((List.hd l)::res)
  in loop ls n [] ;;

(* Tire un élément d'une liste au hasard. *)
let alea_liste ls =
  Random.self_init;
  let rec loop l res =
    match l with
      | [] -> res
      | ll -> let alea = Random.int (List.length ll) in
	      let elem = List.nth ll alea in
	      loop (suppr_elem l alea) (elem::res)
  in loop ls [] ;;

(* Change aléatoirement l'ordre des éléments d'un morceau obj. *)
let rec scrambleOnset obj =
  match obj with 
    | Note x -> Note x
    | Silence x -> Silence x
    | Parallel seq -> Parallel (List.map scrambleOnset (alea_liste seq))
    | Sequence seq -> Sequence (List.map scrambleOnset (alea_liste seq));;

(* Bla ? *)
let alea_atom x duree_max =
  Random.self_init;
  let new_duree = Random.int duree_max in
  match x with
    | Note (hauteur, volume, duree) -> Note (hauteur, volume, new_duree)
    | Silence duree -> Silence new_duree

(* Change aléatoirement l'ordre et la durée des éléments d'un morceau obj. *)
let rec scrambleAll obj duree_max=
  match obj with 
    | Note x -> alea_atom (Note x) duree_max
    | Silence x -> alea_atom (Silence x) duree_max
    | Parallel seq -> Parallel (List.map (fun x -> (scrambleAll x duree_max)) (alea_liste seq))
    | Sequence seq -> Sequence (List.map (fun x -> (scrambleAll x duree_max)) (alea_liste seq));;
