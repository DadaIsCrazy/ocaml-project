open Musical_transformations;;

let harmonizeOnTemps obj n tempo apply_fun =
  let tmp = ref 0 in
  let rec loop obj n tempo apply_fun =
    match obj with
    | Note (hauteur, volume, duree) -> (tmp := !tmp + duree;
		 (if ((!tmp - duree) / 1000 * (tempo / 60)) mod n = 0
		  then (apply_fun (Note (hauteur, volume, duree)))
		  else Note (hauteur, volume, duree)))
    | Silence duree -> (tmp := !tmp + duree;
			Silence duree)   
    | Sequence seq -> (let auw = !tmp and return = Sequence (List.map (fun x -> loop x n tempo apply_fun) seq) in
		       tmp := auw;
		       return)
    | Parallel seq -> Parallel (List.map (fun x -> loop x n tempo apply_fun) seq) in
  loop obj n tempo apply_fun;;
  
(* Prends un morceau obj et transforme la première note de chaque n temps
 * en accord, en choisissant aléatoirement dans liste_accord. *)
let harmonizeChords obj n tempo liste_accord =
  harmonizeOnTemps obj n tempo (fun note -> chordify note (alea_liste liste_accord));;
  
(* Effectue une harmonisation sur le morceau obj en transformant la
 * première note de chaque n temps en une mélodie avec un décalage
 * decal donné en paramètre. *)
let harmonizeSequence obj n tempo decal liste_accord =
  harmonizeOnTemps obj n tempo (fun note -> sequencify (chordify note (alea_liste liste_accord)) decal);;

(* Effectue une harmonisation sur le morceau obj en transformant la
 * première note de chaque n temps en une mélodie avec un décalage
 * decal donné en paramètre, ou en un accord, en choisissant
 * aléatoirement dans liste_accord, suivant le facteur choisi.
 * 0.0 = Harmonisation par accords seuls.
 * 0.5 = Harmonisation par accords ou par décalage.
 * 1.0 = Harmonisation par décalage seuls. *)
let harmonizeRandom obj n tempo decal liste_accord facteur =
  harmonizeOnTemps obj n tempo (fun note -> Random.self_init;
					    let rand = Random.float 1.0 in
					    if rand < facteur
					    then sequencify (chordify note (alea_liste liste_accord)) decal
					    else chordify note (alea_liste liste_accord));;
