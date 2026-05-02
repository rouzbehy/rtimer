open Pomodoro

let print_help () =
  print_endline "Usage: [time value, number] [time unit: m | s] [mode: work | idle]"
;;

type timeunit =
  | MINUTE
  | SECOND

let string_to_timeunit (v : string) : timeunit =
  match String.lowercase_ascii v with
  | "minute" | "minutes" | "m" -> MINUTE
  | "second" | "seconds" | "s" -> SECOND
  | _ -> failwith (Printf.sprintf "Invalid time unit: %s. Must be 'minute' or 'second'" v)
;;

let get_time_in_seconds (value : int) (unit : timeunit) =
  match unit with
  | MINUTE -> value * 60
  | SECOND -> value
;;

let parse_arguments () =
  let args = Sys.argv in
  match Array.to_list args with
  | [ _prog_name; "help" ] ->
    print_help ();
    exit 0
  | [ _prog_name; _time_value; _time_unit; _mode ] ->
    let initial_mode = string_to_mode _mode in
    let tvalue = int_of_string _time_value in
    let time_unit = string_to_timeunit _time_unit in
    let initial_seconds = get_time_in_seconds tvalue time_unit in
    { maximum_seconds = initial_seconds
    ; remaining_seconds = initial_seconds
    ; current_mode = initial_mode
    ; is_active = true
    }
  | _ -> failwith "Invalid argument structure. Use 'help' for more information."
;;
