(* To import Sqlite3, first install sqlite3 using
   opam install sqlite3
   then build so that the IDE can have access to it.
*)
open Sqlite3
open Fpath (* for type-safe path manipulation*)
open Bos
open Pomodoro
(* for OS operations: creating files, folders etc*)

(*prepare the location to save the database. After some digging,
  it seems like it is the preferred location for documents that need to
  persist between restarts of applications. These are not files used system-wide,
  only for important, portable files. They are not necessary to the
  machine's performance, and are only important to the user and their experience
  with various applications that they use.
*)

let get_database () =
  let home =
    match OS.Env.var "HOME" with
    | Some h -> v h
    | None -> v "."
  in
  let dir_path = home / ".local" / "share" / "rtimer" in
  let db_path = dir_path / "rtimer.db" in
  let _ = OS.Dir.create dir_path |> Rresult.R.get_ok in
  db_open (to_string db_path)
;;

let create_table db () =
  let sql =
    "CREATE TABLE IF NOT EXISTS timed_sessions (\n\
    \  id INTEGER PRIMARY KEY ,\n\
    \  date_of_session date NOT NULL,\n\
    \  mode TEXT,\n\
    \  duration_seconds INT \n\
    \  );"
  in
  match exec db sql with
  | Rc.OK -> ()
  | _ -> failwith "Could not initialize the database"
;;

(*exec: a fire-and-forget function for use on string commands
  prepare & bind & step : prevent SQL injections,
*)
let record_session (database : db) (s : session) =
  let sql_command =
    "INSERT INTO timed_sessions (date_of_session, mode, duration_seconds) VALUES \
     (date('now'), ?, ?);"
  in
  let statement = prepare database sql_command in
  ignore (bind statement 1 (Data.TEXT (string_of_mode s.current_mode)));
  ignore (bind statement 2 (Data.INT (Int64.of_int s.maximum_seconds)));
  match step statement with
  | Rc.DONE -> ignore (finalize statement)
  | _ -> failwith "Failed to record the session."
;;

let close_database (database : db) () = ignore (db_close database)
let delete_session () = ()
