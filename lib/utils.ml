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

let prompt_for_integer () =
  print_endline "Enter the target mode: \n 1. Work 2. Idle \n";
  read_int ()
;;

let print_in_situ (message : string) = Printf.printf "\r\027[K%s%!" message

type operating_system =
  | MacOS
  | Linux
  | FreeBSD
  | OpenBSD
  | NetBSD
  | DragonFly
  | Cygwin
  | Win32
  | Unix
  | Other of string

let get_os_name () =
  match Sys.os_type with
  | "Unix" ->
    let ic = Unix.open_process_in "uname" in
    let name = input_line ic in
    ignore (Unix.close_process_in ic);
    (match String.trim name with
     | "Darwin" -> MacOS
     | "Linux" -> Linux
     | "FreeBSD" -> FreeBSD
     | "OpenBSD" -> OpenBSD
     | "NetBSD" -> NetBSD
     | "DragonFly" -> DragonFly
     | _ -> Unix)
  | "Win32" -> Win32
  | "Cygwin" -> Cygwin
  | s -> Other s
;;

(*reads a pre-saved JavaScript file and replaces the placeholder message
  with the provided message.*)
let js_template =
  {|
var app = Application('System Events');
app.includeStandardAdditions = true;
app.displayDialog("PROMPT", {withTitle:"RTimer", buttons:["OK"], defaultButton:"OK"});
|}
;;

let alert message =
  let name = get_os_name () in
  match name with
  | MacOS ->
    let payload =
      Str.global_replace (Str.regexp_string "PROMPT") message js_template
      |> Str.global_replace (Str.regexp "\n") " "
    in
    let command =
      Printf.sprintf "osascript -l JavaScript -e %s" (Filename.quote payload)
    in
    ignore (Sys.command command)
  | _ -> ()
;;
