let print_time_banner (banner : string) =
  let current_time = Unix.time () |> Unix.localtime in
  let formatted_time =
    Printf.sprintf
      "%s timestamp: %04d-%02d-%02d %02d:%02d:%02d\n"
      banner
      (current_time.Unix.tm_year + 1900)
      (current_time.Unix.tm_mon + 1)
      current_time.Unix.tm_mday
      current_time.Unix.tm_hour
      current_time.Unix.tm_min
      current_time.Unix.tm_sec
  in
  print_endline formatted_time
;;

let get_mode_as_int () =
  print_endline "Enter the target mode: \n 1. Work 2. Idle \n";
  read_int ()
;;

let print_in_situ (message : string) = Printf.printf "\r\027[K%s%!" message
