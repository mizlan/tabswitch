open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Appinfo = struct
  type t = { app : string; id : int; space : int }
  [@@deriving yojson] [@@yojson.allow_extra_fields]
end

module Query = struct
  type t = Appinfo.t list [@@deriving yojson]
end
