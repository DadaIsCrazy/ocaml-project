open Musical_transformations;;

type basic_color =
  | Black | White | Red | Green | Blue | Yellow | Cyan | Magenta;;
     
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
  
let init_window title =
  Graphics.open_graph " 1000x256";
  Graphics.set_window_title title;
  Graphics.set_color Graphics.black;
  let rec loop x =
    if x < 400 then
       (Graphics.moveto x 100;
       Graphics.draw_string (string_of_int (x * 10));
       loop (x + 100)) in
  loop 100;;

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
     
let continue () =
  let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
  ev.Graphics.key <> 'q';;

let start_and_stop () =
  init_window "Piano Roll";
  draw_music exemple;
  while continue () do
    ()
  done; 
  Graphics.close_graph ();;
    
(*  start_and_stop (); *)
