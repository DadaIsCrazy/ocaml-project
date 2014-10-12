open Midi;;
open Musical_transformations;;
  

let rec concat_l l =
  match l with
    | [] -> []
    | hd::tl -> hd@(concat_l tl) ;;

let main_fun obj fichier =
  let rec creer_midi obj =
    match obj with
      | Note (hauteur, volume, duree) -> (0, 0, NoteON (hauteur, volume))::(duree, 0, NoteOFF (hauteur, volume))::[]
      | Silence duree -> (0, 0, NoteON (1, 1))::(duree, 0, NoteOFF (0, 0))::[]
      | Parallel seq -> (List.map (fun x -> (0, 0, MIDIStart)::(creer_midi x)@[(0, 0, MIDIStop)]) seq)
      | Sequence seq -> (concat_l (List.map (creer_midi) seq))
  in

  write (0, [(0, 0, MIDIStart)::(creer_midi obj)@[(0, 0, MIDIStop)]]) fichier;;
