open Rtimer.Pomodoro
open Rtimer.Utils
open Rtimer.Arguments
open Rtimer.Coloring
open Rtimer.Progressbar

let rec primary_loop (s : Rtimer.Pomodoro.state) =
  match s.is_active with
  | false ->
    let final_theme =
      match s.current_mode with
      | WORK -> get_theme FOCUS
      | IDLE -> get_theme RELAX
      | HOBBY -> get_theme CREATIVE
    in
    print_progress_bar s.maximum_seconds s.maximum_seconds final_theme ();
    print_newline ();
    print_in_situ "Timer is Finished!\n"
  | true ->
    let theme_pair =
      match s.current_mode with
      | WORK -> get_theme FOCUS
      | IDLE -> get_theme RELAX
      | HOBBY -> get_theme CREATIVE
    in
    let elapsed = s.maximum_seconds - s.remaining_seconds in
    print_progress_bar elapsed s.maximum_seconds theme_pair ();
    Unix.sleep 1;
    primary_loop (step s)
;;

let () =
  try
    let given_state = parse_arguments () in
    print_time_banner "Start";
    primary_loop given_state;
    print_time_banner "End"
  with
  | Failure msg ->
    print_endline ("Error: " ^ msg);
    exit 1
  | _ ->
    print_endline "An unexpected error occurred.";
    exit 1
;;
