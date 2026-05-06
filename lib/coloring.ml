type colour_pair =
  { start_colour : string
  ; stop_colour : string
  }

type theme =
  | FOCUS
  | RELAX
  | CREATIVE

let get_theme = function
  | FOCUS -> { start_colour = "#06447eef"; stop_colour = "#e1f90c" }
  | RELAX -> { start_colour = "#3f0d58"; stop_colour = "#3f0d58" }
  | CREATIVE -> { start_colour = "#edc93a"; stop_colour = "#467777" }
;;

let hex_to_rgb hex =
  let scan s = Scanf.sscanf s "#%2x%2x%2x" (fun r g b -> r, g, b) in
  try scan hex with
  | _ -> 255, 255, 255
;;

let colour_linear_interp start_value end_value factor =
  int_of_float
    ((float_of_int start_value *. (1.0 -. factor)) +. (float_of_int end_value *. factor))
;;
