open Lwt.Syntax
open Yabai

let () = print_endline "Hello, World!"

type info = { space : string; id : string }
type state = (App.t * info) list

let state = ref State.AppMap.empty

let refresh () =
  let* v =
    Lwt_process.pread ("yabai", [| "yabai"; "-m"; "query"; "--windows" |])
  in
  state := v |> Yojson.Safe.from_string |> Query.t_of_yojson |> State.t_of_query;
  Lwt.return_unit

let focus { space; id } =
  let* _ =
    Lwt_process.exec ("yabai", [| "yabai"; "-m"; "space"; "--focus"; space |])
  in
  let* _ =
    Lwt_process.exec ("yabai", [| "yabai"; "-m"; "window"; "--focus"; id |])
  in
  Lwt.return_unit

let handler addr (ic, oc) =
  let* s = Lwt_io.read_line ic in
  match s with
  | "r" -> refresh ()
  | "c" -> cycle ()
  | "x" -> fix ()
  | _ -> Lwt.return_unit

let () =
  Lwt_main.run
    (let* s =
       Lwt_io.establish_server_with_client_address
         (ADDR_UNIX "/Users/ml/chubby") handler
     in
     fst (Lwt.wait ()))
