open Containers

type t = { id : string; space : string }

let equal a b = String.(a.id = b.id)
let pp f { id; space } = Format.fprintf f "(%s,@ %s)" id space
