type mode =
  | WORK
  | IDLE
  | HOBBY

type session =
  { maximum_seconds : int
  ; remaining_seconds : int
  ; current_mode : mode
  ; is_active : bool
  }

(* function that ticks the time, takes a session, returns a session*)
let tick (s : session) : session =
  match s.is_active, s.remaining_seconds with
  | false, 0 -> s
  | false, _ -> s
  | true, 0 -> { s with is_active = false }
  | true, _ -> { s with remaining_seconds = s.remaining_seconds - 1 }
;;

(* transition the session *)
let transition (s : session) : session =
  match s.current_mode, s.remaining_seconds, s.is_active with
  | WORK, 0, true -> { s with is_active = false; current_mode = IDLE }
  | WORK, 0, false -> { s with current_mode = IDLE }
  | IDLE, 0, true -> { s with is_active = false }
  | IDLE, 0, false -> s
  | _, n, true when n > 0 -> s
  | _, n, false when n > 0 -> { s with is_active = true }
  | _ -> s
;;

let string_of_mode = function
  | WORK -> "Work"
  | IDLE -> "Break"
  | HOBBY -> "Hobby"
;;

let session_to_string (s : session) : string =
  let minutes = s.remaining_seconds / 60 in
  let seconds = s.remaining_seconds mod 60 in
  Printf.sprintf
    "[%s] %02d:%02d (Active: %b)"
    (string_of_mode s.current_mode)
    minutes
    seconds
    s.is_active
;;

let take_a_step (s : session) : session = s |> tick |> transition

let int_to_mode (v : int) =
  match v with
  | 1 -> WORK
  | 2 -> HOBBY
  | 3 -> IDLE
  | _ ->
    failwith
      (Printf.sprintf "Invalid mode: %d. Choose from [1] WORK, [2] HOBBY or [3] IDLE." v)
;;

let string_to_mode (v : string) =
  match String.lowercase_ascii v with
  | "work" -> WORK
  | "hobby" | "fun" -> HOBBY
  | "idle" -> IDLE
  | "rest" -> IDLE
  | _ -> failwith (Printf.sprintf "Value %s did not match either WORK or IDLE modes." v)
;;
