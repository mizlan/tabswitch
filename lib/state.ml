open Containers
module Appinfo_Priority_list = Priority_list.Make (Appinfo)

type t =
  string option ref
  * (string, Appinfo.t) Hashtbl.t ref
  * (App.t, Appinfo_Priority_list.t) Hashtbl.t

let create () : t = (ref None, ref (Hashtbl.create 16), Hashtbl.create 16)

let update (q : Yabai.Query.t) ((a, i, s) : t) =
  let keep =
    Yabai.Appinfo.(
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
      | Some (app, id, space) ->
          Hashtbl.replace id_to_info id Appinfo.{ app; id; space }
      | None -> ())
    q;

  (match !a with
  | Some id -> if not (Hashtbl.mem id_to_info id) then a := None
  | _ -> ());

  (* [tbl] now contains the set of windows that now exist *)
  (* [id_to_info] now contains a mapping from now-existing window IDs to spaces (and app name) *)

  (* keep all windows that continue to exist, processing *)
  (* them first to keep their order. their space may have changed *)
  let temp_id_to_info = Hashtbl.copy id_to_info in
  let clean infos =
    List.filter_map
      (fun (Appinfo.{ id; _ } as loc) ->
        try
          let Appinfo.{ space; _ } = Hashtbl.find temp_id_to_info id in
          Hashtbl.remove temp_id_to_info id;
          Some { loc with space }
        with Not_found -> None)
      infos
  in
  Hashtbl.filter_map_inplace (fun _ (p, u) -> Some (clean p, clean u)) s;

  (* now [id_to_info] only contains extra windows which did not previously exist *)
  Hashtbl.iter
    (fun id Appinfo.{ app; id; space } ->
      let p, u = try Hashtbl.find s app with Not_found -> ([], []) in
      Hashtbl.replace s app (p, { app; id; space } :: u))
    temp_id_to_info;

  i := id_to_info

let first_app_window (app : App.t) ((_, _, s) : t) =
  let open Option.Infix in
  Hashtbl.find_opt s app >>= Appinfo_Priority_list.head_opt

let get_by_id id ((_, i, _) : t) = Hashtbl.find_opt !i id

let get_next app ((_, _, s) : t) =
  Appinfo_Priority_list.get_next app (Hashtbl.find s app.app)

let prioritize app ((_, _, s) : t) =
  Appinfo_Priority_list.prioritize_back app (Hashtbl.find s app.app)

let pp f ((a, _, s) : t) =
  let open CCFormat in
  pp_set_margin f 100;
  fprintf f "@[<v2>(%a) %a" (Option.pp String.pp) !a
    (Hashtbl.pp ~pp_start:(return "{@ ") ~pp_stop:(return "@]@ }") ~pp_sep:cut
       ~pp_arrow:(return ":@ ") App.pp
       (CCPair.pp ~pp_start:(return "(@[") ~pp_stop:(return ")@]")
          (CCList.pp ~pp_start:(return "P[") ~pp_sep:space ~pp_stop:(return "]")
             Appinfo.pp)
          (CCList.pp ~pp_start:(return "U[") ~pp_sep:space ~pp_stop:(return "]")
             Appinfo.pp)))
    s
