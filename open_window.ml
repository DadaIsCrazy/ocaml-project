open Musical_transformations;;
open Harmonisation;;

type basic_color =
  | Black | White | Red | Green | Blue | Yellow | Cyan | Magenta;;

(* Permet de changer la couleur d'affichage. *)
let actual_color = ref Black;;
let new_color () =
  match !actual_color with
  | Black   -> actual_color := Red;     Graphics.black
  | White   -> actual_color := Red;     Graphics.white
  | Red     -> actual_color := Green;   Graphics.red
  | Green   -> actual_color := Blue;    Graphics.green
  | Blue    -> actual_color := Cyan;    Graphics.blue
  | Yellow  -> actual_color := Cyan;    Graphics.yellow
  | Cyan    -> actual_color := Magenta; Graphics.cyan
  | Magenta -> actual_color := Black;   Graphics.magenta;;

(* Ouvre une fenêtre et la met à une taille correcte. *)
let init_window title =
  Graphics.open_graph " 1000x256";
  Graphics.set_window_title title;
  Graphics.set_color Graphics.black;
  (* let rec loop x =
    if x < 400 then
       (Graphics.moveto x 100;
       Graphics.draw_string (string_of_int (x * 10));
       loop (x + 100)) in
  loop 100 *);;

(* Affiche une chanson MIDI dans le formalisme Piano Roll, dans la fenêtre créée. *)
let draw_music music =
  Graphics.clear_graph ();
  let start = ref 0 in
  let rec loop music =
    match music with
    | Note (hauteur, _, duree) -> Graphics.set_color (new_color ());
				  Graphics.draw_segments [| !start / 10, hauteur * 2, (!start + duree) / 10, hauteur * 2 |];
				  start := !start + duree
    | Silence duree -> start := !start + duree
    | Sequence seq -> let tmp = !start in
		      List.map loop seq;
		      start := tmp
    | Parallel seq -> let tmp = !start in
		      List.map loop seq;
		      start := tmp in
  loop music;;

(* Attends la pression de la touche q pour quitter. *)
let continue () =
  let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
  ev.Graphics.key <> 'q';;

(* Lance l'ouverture de la fenêtre et écrit le morceau en Piano Roll. *)
let start_and_stop morceau =
  init_window "Piano Roll";
  draw_music morceau;
  while continue () do
    ()
  done; 
  Graphics.close_graph ();;
    
  start_and_stop exemple;;
    start_and_stop (harmonizeChords exemple 4 60 [24 ; 56 ; -23 ]);;
          start_and_stop (harmonizeSequence exemple 4 60 5 [24 ; 56 ; -23 ]);;
