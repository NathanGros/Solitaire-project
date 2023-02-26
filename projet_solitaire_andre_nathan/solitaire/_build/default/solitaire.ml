open Graphics;;

(*global variables*)
(*default background : bd; default foreground : fd; highlighted background : bh; highlighted foreground : fh; selected background : bs; selected foreground : fs*)
let bd = ref 'b' ;;
let fd = ref 'w' ;;
let bh = ref 'w' ;;
let fh = ref 'r' ;;
let points = ref [] ;;
let temp_points = ref [] ;;
let diff_int = ref 0 ;;

let customize_window () =
  open_graph " 800x800+600-150";
  clear_graph ();
  set_window_title "Peg Solitaire"
;;

let color_of_char c =
  match c with
  | 'b' -> black
  | 'w' -> white
  | 'r' -> red
  | _ -> white
;;

let matrix_fill_rectangle x1 y1 x2 y2 m c points_list =
  for i = x1 to x2 do
    for j = y1 to y2 do
      m.(i).(j).(0) <- c;
      points_list := (i,j)::!points_list
    done;
  done
;;

let rec included l c =
  match l with
  | [] -> false
  | t::q ->
    if t = c then
      true
    else
      included q c
;;

let make_3x3 m x y n =
  if ((x<n-1) && (y<n-1) && (x>0) && (y>0)) then
    matrix_fill_rectangle (x-1) (y-1) (x+1) (y+1) m 'O' temp_points
;;

let board_make diff =
  diff_int := int_of_char(!diff) - 48;
  let board_size = !diff_int + 7 in
  let board = Array.init board_size (fun _ -> Array.init board_size (fun _ -> [|' ';!fd;!bd|])) in
  (board, board_size)
;;

let prepare_board m n =
  Random.self_init ();
  match n with
  | 7 -> matrix_fill_rectangle 0 2 6 4 m 'O' points;
    matrix_fill_rectangle 2 0 4 6 m 'O' points;
    m.(3).(3).(0) <- '.'
  | _ -> matrix_fill_rectangle (n/2-3) (n/2-1) (n/2+3) (n/2+1) m 'O' points;
    matrix_fill_rectangle (n/2-1) (n/2-3) (n/2+1) (n/2+3) m 'O' points;
    for k = 0 to !diff_int do
      for i = 0 to n-1 do
        for j = 0 to n-1 do
          let rand = Random.int 10-k in
          if ((m.(i).(j).(0) = 'O') && (rand = 0)) && included !points (i, j) then make_3x3 m i j n
        done;
      done;
      points := !points @ !temp_points;
      temp_points := [];
    done;    
    m.(n/2).(n/2).(0) <- '.'
;;

let print_matrix m score n startx starty =
  let side = 40 in
  set_color black;
  fill_rect 0 0 5000 5000;
  set_color white;
  fill_rect 0 0 70 20;
  moveto 5 5;
  set_color red;
  draw_string ("Score : ");
  draw_string (string_of_int (!score));
  for i = 0 to (n-1) do
    for j = 0 to (n-1) do
      set_color (color_of_char(m.(i).(j).(2)));
      fill_rect (startx+side*i) (starty+side*j) side side;
      set_color (color_of_char(m.(i).(j).(1)));
      moveto (startx+side*i) (starty+side*j);
      match m.(i).(j).(0) with
      | 'O' -> fill_circle (startx+side*i+side/2) (starty+side*j+side/2) 10
      | '.' -> fill_circle (startx+side*i+side/2) (starty+side*j+side/2) 3
      | _ -> ()
    done
  done
;;

let tilecolor m x y b f =
  m.(!x).(!y).(1) <- !f;
  m.(!x).(!y).(2) <- !b
;;

let move_cursor m n x y c =
  tilecolor m x y bd fd;
  match c with
  |'z' -> if !y<(n-1) then y := !y+1
  |'s' -> if !y>0 then y := !y-1
  |'q' -> if !x>0 then x := !x-1
  |'d' -> if !x<(n-1) then x := !x+1
  |_ -> ()
;;

let possible_move m n x y c =
  match c with
  |'z' -> (!y<(n-2) && (m.(!x).(!y).(0) = 'O') && (m.(!x).(!y+1).(0) = 'O') && (m.(!x).(!y+2).(0) = '.'))
  |'s' -> (!y>1 && (m.(!x).(!y).(0) = 'O') && (m.(!x).(!y-1).(0) = 'O') && (m.(!x).(!y-2).(0) = '.'))
  |'q' -> (!x>1 && (m.(!x).(!y).(0) = 'O') && (m.(!x-1).(!y).(0) = 'O') && (m.(!x-2).(!y).(0) = '.'))
  |'d' -> (!x<(n-2) && (m.(!x).(!y).(0) = 'O') && (m.(!x+1).(!y).(0) = 'O') && (m.(!x+2).(!y).(0) = '.'))
  |_ -> false
;;

let move_ball m x y c cursor_mode score=
  cursor_mode := true;
  bh := 'w';
  fh := 'r';
  tilecolor m x y bh fh;
  score := !score+1;
  match c with
  | 'z' -> m.(!x).(!y).(0) <- '.';
    m.(!x).(!y+1).(0) <- '.';
    m.(!x).(!y+2).(0) <- 'O'
  | 's' -> m.(!x).(!y).(0) <- '.';
    m.(!x).(!y-1).(0) <- '.';
    m.(!x).(!y-2).(0) <- 'O'
  | 'q' -> m.(!x).(!y).(0) <- '.';
    m.(!x-1).(!y).(0) <- '.';
    m.(!x-2).(!y).(0) <- 'O'
  | 'd' -> m.(!x).(!y).(0) <- '.';
    m.(!x+1).(!y).(0) <- '.';
    m.(!x+2).(!y).(0) <- 'O'
  | _ -> ()
;;

let process_key running cursor_mode score c m n x y =
  match c with
  | '\027' -> running := false
  | '\032' -> cursor_mode := (not !cursor_mode);
    if !cursor_mode then (
      bh := 'w';
      fh := 'r';
    )
    else (
      bh := 'r';
      fh := 'w';
    );
    tilecolor m x y bh fh
  | _ -> if !cursor_mode then begin move_cursor m n x y c; tilecolor m x y bh fh; end
                              else if possible_move m n x y c then move_ball m x y c cursor_mode score
;;

let print_rules () =
  let lines = [|"Welcome to Peg Solitaire !";
  "The goal of this game is to remove";
  "as many balls as you can.";
  "You can choose a ball, jump over an other";
  "one, and remove the ball you jumped.";
  "";
  "To select a ball, move with z, q, s, and d";
  "and press the spacebar.";
  "(you can unselect by";
  "pressing the spacebar again)";
  "Then choose the direction you want to jump";
  "with z, q, s, or d.";
  "Press Escape to leave the game.";
  "";
  "Press any key to continue"|]
  in        
  set_color black;
  fill_rect 0 0 5000 5000;
  set_color white;
  for i = 0 to 14 do
    moveto 290 (520-15*i);
    draw_string lines.(i);
  done
;;

let print_difficulty () =
  let lines = [|"Choose a difficulty by typing";
    "a number between 0 and 9.";
    "0 is the classic game, difficulty";
    "increases with numbers"|] in
  set_color black;
  fill_rect 0 0 5000 5000;
  set_color white;
  for i = 0 to 3 do
    moveto 290 (450-15*i);
    draw_string lines.(i);
  done
;;

let _ =
  customize_window ();
  let running = ref true in
  let x = ref 3 in
  let y = ref 3 in
  let cursor_mode = ref true in
  let score = ref 0 in
  print_rules ();
  (*difficulty selection*)
  let wait_key_pressed = wait_next_event [Key_pressed] in
  print_difficulty ();
  let difficulty_detect = wait_next_event [Key_pressed] in
  let diff = ref difficulty_detect.key in
  while ((!diff <> '0') &&
    (!diff <> '1') &&
    (!diff <> '2') &&
    (!diff <> '3') &&
    (!diff <> '4') &&
    (!diff <> '5') &&
    (!diff <> '6') &&
    (!diff <> '7') &&
    (!diff <> '8') &&
    (!diff <> '9'))
    do
    let difficulty_detect = wait_next_event [Key_pressed] in
    diff := difficulty_detect.key;
  done;

  (*making board*)
  let board, board_size = board_make diff in
  prepare_board board board_size;
  tilecolor board x y bh fh;
  while !running do
    print_matrix board score board_size ((800-board_size*40)/2) ((800-board_size*40)/2);
    let keydetect = wait_next_event [Key_pressed] in
    let c = keydetect.key in
    process_key running cursor_mode score c board board_size x y
  done
;;
