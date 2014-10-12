open Midi;;
open Musical_transformations;;
  
let rec calcul_duree obj_midi =
  match obj_midi with 
  | [] -> 0
  | (temps, _, _)::suite -> temps + (calcul_duree suite) ;;
  
(* Fonction principale, qui convertit un objet musical en objet MIDI
 * puis l'écrit dans le fichier file au format MIDI. *)
let main_fun obj fichier =
  let rec creer_midi obj duree_prev =
    let duree_prec = ref duree_prev in
    match obj with
      | Note (hauteur, volume, duree) -> (0, 0, NoteON (hauteur, volume))::(duree, 0, NoteOFF (hauteur, volume))::[]
      | Silence duree -> (0, 0, NoteON (1, 1))::(duree, 0, NoteOFF (0, 0))::[]
      | Parallel seq -> (MIDIStart)::(List.fold_left (@) (List.map (fun x -> (duree_prev, 0, creer_midi x !duree_prec)) seq) [])@[(MIDIStop)]
      | Sequence seq -> (duree_prev, 0, List.map (fun x -> let res = creer_midi x !duree_prec in
                               duree_prec := !duree_prec + (calcul_duree duree_prev);
                               res) seq)
  in
  write (0, (MIDIStart)::(creer_midi obj 0)@[MIDIStop]) fichier;;
