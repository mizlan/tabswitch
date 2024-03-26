open Containers
open Yabai

type t = (App.t, Locinfo.t list) Hashtbl.t

let create () : t = Hashtbl.create 16

let update (q : Query.t) (s : t) =
  let keep =
    Appinfo.(
      function
      | { app = "kitty"; id; space } ->
          Some (App.Kitty, string_of_int id, string_of_int space)
      | { app = "Skim"; id; space } ->
          Some (App.Skim, string_of_int id, string_of_int space)
      | { app = "Firefox"; id; space } ->
          Some (App.Firefox, string_of_int id, string_of_int space)
      | _ -> None)
  in
  let id_to_info = Hashtbl.create 8 in
  List.iter
    (fun a ->
      match keep a with
      | Some (app, id, space) -> Hashtbl.replace id_to_info id (space, app)
      | None -> ())
    q;

  (* [tbl] now contains the set of windows that now exist *)
  (* [id_to_info] now contains a mapping from now-existing window IDs to spaces (and app name) *)

  (* keep all windows that continue to exist, processing *)
  (* them first to keep their order. their space may have changed *)
  Hashtbl.filter_map_inplace
    (fun _ infos ->
      Some
        (List.filter_map
           (fun (Locinfo.{ id; _ } as loc) ->
             try
               let sp, _ = Hashtbl.find id_to_info id in
               Hashtbl.remove id_to_info id;
               Some { loc with space = sp }
             with Not_found -> None)
           infos))
    s;

  (* now [id_to_info] only contains extra windows which did not previously exist *)
  Hashtbl.iter
    (fun id (space, app) ->
      let l = try Hashtbl.find s app with Not_found -> [] in
      Hashtbl.replace s app ({ id; space } :: l))
    id_to_info

let pp f s =
  let open CCFormat in
  fprintf f "{@[";
  fprintf f "%a"
    (Hashtbl.pp App.pp
       (CCList.pp ~pp_start:(return "[") ~pp_stop:(return "]") Locinfo.pp))
    s;
  fprintf f "@]}"

let f v = v |> Yojson.Safe.from_string |> Query.t_of_yojson

(* below should be server.ml *)
