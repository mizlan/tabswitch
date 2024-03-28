open Containers
open Yabai
module Locinfo_Priority_list = Priority_list.Make (Locinfo)

type t = string option ref * (App.t, Locinfo_Priority_list.t) Hashtbl.t

let create () : t = (ref None, Hashtbl.create 16)

let update (q : Query.t) ((a, s) : t) =
  let keep =
    Appinfo.(
      function
      | { app = "kitty"; id; space } ->
          Some (App.Kitty, string_of_int id, string_of_int space)
      | { app = "Skim"; id; space } ->
          Some (App.Skim, string_of_int id, string_of_int space)
      | { app = "Firefox"; id; space } ->
          Some (App.Firefox, string_of_int id, string_of_int space)
      | { app = "Neovide"; id; space } ->
          Some (App.Neovide, string_of_int id, string_of_int space)
      | _ -> None)
  in
  let id_to_info = Hashtbl.create 8 in
  List.iter
    (fun a ->
      match keep a with
      | Some (app, id, space) -> Hashtbl.replace id_to_info id (space, app)
      | None -> ())
    q;

  (match !a with
  | Some id -> if not (Hashtbl.mem id_to_info id) then a := None
  | _ -> ());

  (* [tbl] now contains the set of windows that now exist *)
  (* [id_to_info] now contains a mapping from now-existing window IDs to spaces (and app name) *)

  (* keep all windows that continue to exist, processing *)
  (* them first to keep their order. their space may have changed *)
  let clean infos =
    List.filter_map
      (fun (Locinfo.{ id; _ } as loc) ->
        try
          let sp, _ = Hashtbl.find id_to_info id in
          Hashtbl.remove id_to_info id;
          Some { loc with space = sp }
        with Not_found -> None)
      infos
  in
  Hashtbl.filter_map_inplace (fun _ (p, u) -> Some (clean p, clean u)) s;

  (* now [id_to_info] only contains extra windows which did not previously exist *)
  Hashtbl.iter
    (fun id (space, app) ->
      let p, u = try Hashtbl.find s app with Not_found -> ([], []) in
      Hashtbl.replace s app (p, { id; space } :: u))
    id_to_info

let first_app_window (app : App.t) ((_, s) : t) =
  let open Option.Infix in
  Hashtbl.find_opt s app >>= Locinfo_Priority_list.head_opt

(* let get_next_by_id id ((_, s) : t) = *)
(*   let locs = Hashtbl.to_seq_values s |> List.of_seq |> List.flatten in *)
(*   List.find_opt (fun { Locinfo.id = id'; _ } -> String.(id = id')) locs *)

let pp f ((a, s) : t) =
  let open CCFormat in
  pp_set_margin f 100;
  fprintf f "@[<v2>(%a) %a" (Option.pp String.pp) !a
    (Hashtbl.pp ~pp_start:(return "{@ ") ~pp_stop:(return "@]@ }") ~pp_sep:cut
       ~pp_arrow:(return ":@ ") App.pp
       (CCPair.pp ~pp_start:(return "(@[") ~pp_stop:(return ")@]")
          (CCList.pp ~pp_start:(return "P[") ~pp_sep:space ~pp_stop:(return "]")
             Locinfo.pp)
          (CCList.pp ~pp_start:(return "U[") ~pp_sep:space ~pp_stop:(return "]")
             Locinfo.pp)))
    s
