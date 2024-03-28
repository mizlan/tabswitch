open Containers

type t = { app : App.t; id : string; space : string }

let equal a b = String.(a.id = b.id)

let pp f { app; id; space } =
  Format.fprintf f "(%a,@ %s,@ %s)" App.pp app id space
