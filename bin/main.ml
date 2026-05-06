open Rtimer.Pomodoro
open Rtimer.Utils
open Rtimer.Arguments
open Rtimer.Coloring
open Rtimer.Progressbar
open Rtimer.Dbinterface

let rec primary_loop (s : Rtimer.Pomodoro.session) =
  let bar_theme =
    match s.current_mode with
    | WORK -> get_theme FOCUS
    | IDLE -> get_theme RELAX
    | HOBBY -> get_theme CREATIVE
  in
  match s.is_active with
  | false ->
    print_progress_bar s.maximum_seconds s.maximum_seconds bar_theme ();
    print_newline ();
    let message =
      Printf.sprintf "\tTimer for %s Finished!\n" (string_of_mode s.current_mode)
    in
    alert message;
    print_in_situ message
  | true ->
    let elapsed = s.maximum_seconds - s.remaining_seconds in
    print_progress_bar elapsed s.maximum_seconds bar_theme ();
    Unix.sleep 1;
    primary_loop (take_a_step s)
;;

let () =
  try
    let db = get_database () in
    create_table db ();
    let given_session = parse_arguments () in
    print_time_banner "Start";
    primary_loop given_session;
    record_session db given_session;
    close_database db ();
    print_time_banner "End"
  with
  | Failure msg ->
    print_endline ("Error: " ^ msg);
    exit 1
  | exn ->
    print_endline ("Unexpected error: " ^ Printexc.to_string exn);
    exit 1
;;
