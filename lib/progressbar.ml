open Coloring

let cached_width = ref None

let get_terminal_width () =
  match !cached_width with
  | Some w -> w
  | None ->
    let w =
      try
        let ic = Unix.open_process_in "tput cols" in
        let cols = input_line ic in
        ignore (Unix.close_process_in ic);
        int_of_string cols
      with
      | _ -> 80
    in
    cached_width := Some w;
    w
;;

let print_progress_bar current maximum pair ?(symbol = "\u{2588}") () =
  let width = get_terminal_width () in
  let bar_width = width - 10 in
  let progress = float_of_int current /. float_of_int maximum in
  let filled_width = int_of_float (progress *. float_of_int bar_width) in
  let s_r, s_g, s_b = Coloring.hex_to_rgb pair.start_colour in
  let e_r, e_g, e_b = Coloring.hex_to_rgb pair.stop_colour in
  let r = Coloring.colour_linear_interp s_r e_r progress in
  let g = Coloring.colour_linear_interp s_g e_g progress in
  let b = Coloring.colour_linear_interp s_b e_b progress in
  Printf.printf "\r\x1b[38;2;%d;%d;%dm" r g b;
  for i = 1 to bar_width do
    if i <= filled_width then print_string symbol else print_string "-"
  done;
  Printf.printf "\x1b[0m %3d%% %!" (int_of_float (progress *. 100.0))
;;
