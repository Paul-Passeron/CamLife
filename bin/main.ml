open Raylib
let width = 800
let height = 450
let title = "Conway's Game of Life"

let emod a b = ((a mod b + b) mod b)

type state = 
  | Alive
  | Dead

let draw_grid (t: state array array) (w: int) (h: int): unit =
  let stepX = w / (Array.length t)
  and stepY = h / (Array.length t.(0)) in
  for x = 0 to (Array.length t -1) do
    for y = 0 to (Array.length t.(0) -1) do
      let col = match t.(x).(y) with
        | Alive -> Color.black
        | _ -> Color.white in
      draw_rectangle (x*stepX) (y*stepY) stepX stepY col;
    done;
  done


let create_random_world (width: int) (height: int): state array array =
  let t = Array.make_matrix width height Dead in
  for x = 0 to width -1 do
    for y = 0 to height -1 do
      t.(x).(y) <- match Random.bool () with
        | true -> Alive
        | _ -> Dead;
    done;
  done;
  t

let number_of_neightbours t x y =
  let w = Array.length t
  and h = Array.length t.(0) in
  let cpt = ref 0 in
  for dx = -1 to 1 do
    for dy = -1 to 1 do
      let i = emod (x+dx) w
      and j = emod (y+dy) h in
      match t.(i).(j) with
        | Alive when dx <> 0 || dy <> 0 -> incr cpt
        | _ -> ();
    done;
  done;
  !cpt

let next_state t x y =
  let n = number_of_neightbours t x y in
  let alive = match t.(x).(y) with
    | Alive -> true
    | _ -> false in
  let cond = ((n=3)||(alive && n=2))in
  match cond with
    | true -> Alive
    | _ -> Dead

let update_grid (t: state array array): state array array =
  let w = Array.length t
  and h = Array.length t.(0) in
  let res = Array.make_matrix w h Dead in
  for x = 0 to w-1 do
    for y = 0 to h-1 do
      res.(x).(y) <- next_state t x y;
    done;
  done;
  res

let t = ref (create_random_world 80 45)

let setup () =
  init_window width height title(*;
  set_target_fps 60*)

let rec loop () =
  if window_should_close () then close_window ()
  else
    begin_drawing ();
    clear_background Color.raywhite;
    draw_grid !t width height;
    t := update_grid !t;    
    end_drawing ();
    loop ()

let () = setup () |> loop