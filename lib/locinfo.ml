type t = { id : string; space : string }

let pp f { id; space } = Format.fprintf f "(%s,@ %s)" id space
