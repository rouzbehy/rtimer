open Rtimer.Pomodoro
open Rtimer.Utils
open Rtimer.Arguments
(*
   let state = {remaining_seconds=5; is_active=false; current_mode=WORK}
let () = print_endline (print_state state);
*)

let rec primary_loop (s : Rtimer.Pomodoro.state) =
  match s.is_active with
  | false -> print_in_situ "Timer is finished!"
  | true ->
    print_in_situ (state_to_string s);
    Unix.sleep 1;
    primary_loop (step s)
;;

let () =
  let state = parse_arguments () in
  state |> state_to_string |> print_in_situ
;;
(* let mode = int_to_mode (get_mode_as_int ()) in
  let initial_state = { remaining_seconds = 5; current_mode = mode; is_active = true } in
  print_time_banner "start";
  primary_loop initial_state;
  print_endline "";
  print_time_banner "stop" *)
