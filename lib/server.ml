open Containers
open Lwt.Syntax

let state = State.create ()

let refresh () =
  let* v =
    Lwt_process.pread ("yabai", [| "yabai"; "-m"; "query"; "--windows" |])
  in
  let q = v |> Yojson.Safe.from_string |> Yabai.Query.t_of_yojson in
  State.update q state;
  Format.printf "%a@." State.pp state;
  Lwt.return_unit

let focus Appinfo.{ id; space } =
  let* _ =
    Lwt_process.exec ("yabai", [| "yabai"; "-m"; "space"; "--focus"; space |])
  in
  let* _ =
    Lwt_process.exec ("yabai", [| "yabai"; "-m"; "window"; "--focus"; id |])
  in
  Lwt.return_unit

let prioritize () =
  print_endline "prioritize";
  Lwt.return_unit

let set_active (id : string) =
  let a, _, _ = state in
  Format.printf "active: %s@." id;
  a := Some id;
  Lwt.return_unit

let cycle () =
  Option.get_or ~default:Lwt.return_unit
    (let a_opt, i, _ = state in
     let open Option.Infix in
     let* a = !a_opt in
     let* a = Hashtbl.find_opt !i a in
     let+ next = State.get_next a state in
     let open Lwt.Syntax in
     (* optimistically update in the case of fast cycling *)
     let* () = set_active next.id in
     focus next)

let all_but_first s = String.sub s 1 (String.length s - 1)

(* [id] -> promotion *)
(* [id] -> app -> cycle *)

let handler _ (ic, _oc) =
  let* s = Lwt_io.read_line ic in
  match s with
  | s when String.starts_with ~prefix:"a" s -> set_active (all_but_first s)
  | "r" -> refresh ()
  | "c" -> cycle ()
  | "x" -> prioritize ()
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
