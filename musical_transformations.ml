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


let rec duration obj =
  match obj with 
  | Note (_, _, duree) -> duree
  | Silence duree -> duree
  | Sequence seq -> List.fold_left (+) 0 (List.map duration seq)
  | Parallel seq -> List.fold_left max 0 (List.map duration seq);;

let rec copy obj =
  match obj with
  | Note x -> Note x
  | Silence duree -> Silence duree
  | Sequence seq -> Sequence (List.map copy seq)
  | Parallel seq -> Parallel (List.map copy seq);;

let rec note_count obj =
  match obj with
  | Note x -> 1
  | Silence duree -> 0
  | Sequence seq -> List.fold_left (+) 0 (List.map note_count seq)
  | Parallel seq -> List.fold_left (+) 0 (List.map note_count seq);;

let rec stretch obj facteur =
  match obj with
  | Note (hauteur, volume, duree) -> Note (hauteur, volume, int_of_float ((float_of_int duree) *. facteur))
  | Silence duree -> Silence (int_of_float ((float_of_int duree) *. facteur))
  | Sequence seq -> Sequence (List.map (fun x -> stretch x facteur) seq)
  | Parallel seq -> Parallel (List.map (fun x -> stretch x facteur) seq);;

let beats obj tempo =
  (duration obj) / 1000 * (tempo / 60);;

let beatsList obj n tempo=
  let tmp = ref 0 in
  let rec loop obj n tempo=
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
      

let rec transpose obj intervalle = 
  match obj with
  | Note (hauteur, volume, duree) -> Note (hauteur + intervalle, volume, duree)
  | Silence duree -> Silence duree
  | Sequence seq -> Sequence (List.map (fun x -> transpose x intervalle) seq)
  | Parallel seq -> Parallel (List.map (fun x -> transpose x intervalle) seq);;


let rec retrograde obj =
  match obj with
  | Note x -> Note x
  | Silence duree -> Silence duree
  | Sequence seq -> Sequence (List.map retrograde (List.rev seq))
  | Parallel seq -> Parallel (List.map retrograde (List.rev seq));;


let rec miroir obj note =
  match obj with
  | Note (hauteur, volume, duree) -> Note (note + (note - hauteur), volume, duree)
  | Silence duree -> Silence duree
  | Sequence seq -> Sequence (List.map (fun x -> miroir x note) seq)
  | Parallel seq -> Parallel (List.map (fun x -> miroir x note) seq) ;;


let palindrome ( obj: objet_musical ) =
  match obj with
    | Note x -> Sequence [(Note x); (Note x)]
    | Silence x -> Sequence [(Silence x); (Silence x)]
    | Sequence x -> Sequence [(Sequence x); (Sequence (List.rev  x))]
    | Parallel x -> Sequence [(Parallel x); (Parallel x)];;


let chordify note liste_obj =
  match note with
  | Note (hauteur, volume, duree) -> Parallel (List.map (fun x -> Note (hauteur + x, volume, duree)) liste_obj);;


let rec chordifyList obj transfo =
  match obj with
    | Note x -> chordify (Note x) transfo
    | Silence x -> Silence x
    | Parallel seq -> Parallel (List.map (fun x -> chordifyList x transfo) seq)
    | Sequence seq -> Sequence (List.map (fun x -> chordifyList x transfo) seq);;


let sequencify accord decal =
  match accord with 
    | Parallel seq -> Sequence (List.map (fun x -> match x with Note (h, v, d) -> Note (h, v, decal)) seq) ;;

let suppr_elem ls n =
  let rec loop l nb res =
    match nb with
      | 0 -> (List.append (List.rev res) (List.tl l))
      | x -> loop (List.tl l) (x - 1) ((List.hd l)::res)
  in loop ls n [] ;;

let alea_liste ls =
  Random.self_init;
  let rec loop l res =
    match l with
      | [] -> res
      | ll -> let alea = Random.int (List.length ll) in
	      let elem = List.nth ll alea in
	      loop (suppr_elem l alea) (elem::res)
  in loop ls [] ;;

let rec scrambleOnset obj =
  match obj with 
    | Note x -> Note x
    | Silence x -> Silence x
    | Parallel seq -> Parallel (List.map scrambleOnset (alea_liste seq))
    | Sequence seq -> Sequence (List.map scrambleOnset (alea_liste seq));;

let alea_atom x duree_max =
  Random.self_init;
  let new_duree = Random.int duree_max in
  match x with
    | Note (hauteur, volume, duree) -> Note (hauteur, volume, new_duree)
    | Silence duree -> Silence new_duree


let rec scrambleAll obj duree_max=
  match obj with 
    | Note x -> alea_atom (Note x) duree_max
    | Silence x -> alea_atom (Silence x) duree_max
    | Parallel seq -> Parallel (List.map (fun x -> (scrambleAll x duree_max)) (alea_liste seq))
    | Sequence seq -> Sequence (List.map (fun x -> (scrambleAll x duree_max)) (alea_liste seq));;
