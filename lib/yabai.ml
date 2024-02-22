open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Appinfo = struct
  type t = { app : App.t; id : string; space : string }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Query = struct
  type t = Appinfo.t list [@@deriving yojson]
end

module State = struct
  module AppMap = Map.Make (App)

  type r = { id : string; space : string }
  type t = r list AppMap.t

  let empty : t = AppMap.empty

  let t_of_query : Query.t -> t =
    List.fold_left
      (fun acc (x : Appinfo.t) ->
        AppMap.add_to_list x.app { id = x.id; space = x.space } acc)
      empty
end
