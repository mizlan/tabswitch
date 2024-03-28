open Containers
open Lwt.Syntax
open Yabai

let state = State.create ()

let refresh () =
  let* v =
    Lwt_process.pread ("yabai", [| "yabai"; "-m"; "query"; "--windows" |])
  in
  let q = v |> Yojson.Safe.from_string |> Query.t_of_yojson in
  State.update q state;
  Format.printf "%a@." State.pp state;
  Lwt.return_unit

let focus Locinfo.{ id; space } =
  let* _ =
    Lwt_process.exec ("yabai", [| "yabai"; "-m"; "space"; "--focus"; space |])
  in
  let* _ =
    Lwt_process.exec ("yabai", [| "yabai"; "-m"; "window"; "--focus"; id |])
  in
  Lwt.return_unit

let fix () =
  print_endline "fix";
  Lwt.return_unit

let cycle () =
  print_endline "cycle";
  Lwt.return_unit

let set_active (id : string) =
  fst state := Some id;
  Lwt.return_unit

let all_but_first s = String.sub s 1 (String.length s - 1)

let handler _ (ic, _oc) =
  let* s = Lwt_io.read_line ic in
  match s with
  | s when String.starts_with ~prefix:"a" s -> set_active (all_but_first s)
  | "r" -> refresh ()
  | "c" -> cycle ()
  | "x" -> fix ()
  | s when String.starts_with ~prefix:"g" s ->
      Option.get_or ~default:Lwt.return_unit
        (let open Option.Infix in
         let* app = App.of_string (all_but_first s) in
         let+ info = State.first_app_window app state in
         focus info)
  | _ -> Lwt.return_unit

let start_server () =
  Lwt_main.run
    (let* _s =
       Lwt_io.establish_server_with_client_address
         (ADDR_UNIX "/Users/ml/chubby") handler
     in
     let* () = refresh () in
     fst (Lwt.wait ()))
