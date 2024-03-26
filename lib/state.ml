open Lwt.Syntax
open Yabai

let () = print_endline "Hello, World!"

type info = { space : string; id : string }
type state = (App.t * info) list

let state = State.create ()

let f v = v |> Yojson.Safe.from_string |> Query.t_of_yojson

let refresh () =
  let* v =
    Lwt_process.pread ("yabai", [| "yabai"; "-m"; "query"; "--windows" |])
  in
  print_string v;
  let q = v |> Yojson.Safe.from_string |> Query.t_of_yojson in
  State.update q state;
  Lwt.return_unit

let focus { space; id } =
  let* _ =
    Lwt_process.exec ("yabai", [| "yabai"; "-m"; "space"; "--focus"; space |])
  in
  let* _ =
    Lwt_process.exec ("yabai", [| "yabai"; "-m"; "window"; "--focus"; id |])
  in
  Lwt.return_unit

let fix () =
  print_endline "fix"; Lwt.return_unit

let cycle () =
  print_endline "cycle"; Lwt.return_unit

let handler addr (ic, oc) =
  let* s = Lwt_io.read_line ic in
  match s with
  | "r" -> refresh ()
  | "c" -> cycle ()
  | "x" -> fix ()
  | _ -> Lwt.return_unit

let start_server () =
  Lwt_main.run
    (let* s =
       Lwt_io.establish_server_with_client_address
         (ADDR_UNIX "/Users/ml/chubby") handler
     in
     fst (Lwt.wait ()))
