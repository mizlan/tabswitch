open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Containers

module Appinfo = struct
  type t = { app : App.t; id : string; space : string }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Query = struct
  type t = Appinfo.t list [@@deriving yojson]
end

module State = struct
  module Locinfo = struct
    type t = { id : string; space : string }
  end

  type t = (App.t, Locinfo.t list) Hashtbl.t

  let create () = Hashtbl.create 16

  let update (s : t) (q : Query.t) =
    let tbl = Hashtbl.create 8 in
    let id_to_space = Hashtbl.create 8 in
    List.iter
      (fun Appinfo.{ app; id; space } ->
        let l = try Hashtbl.find tbl app with Not_found -> [] in
        Hashtbl.replace tbl app (Locinfo.{ id; space } :: l);
        Hashtbl.replace id_to_space id space)
      q;

    (* [tbl] now contains the set of windows that now exist *)
    (* [id_to_space] now contains a mapping from window ID to space *)

    (* keep all windows that continue to exist, processing *)
    (* them first to keep their order. their space may have changed *)
    Hashtbl.filter_map_inplace
      (fun app infos ->
        try
          let now = Hashtbl.find tbl app in
          Some (List.inter ~eq:Stdlib.( = ) infos now)
        with Not_found -> None)
      s
end
