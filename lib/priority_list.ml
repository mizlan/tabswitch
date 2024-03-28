module Make (E : sig
  type t

  val equal : t -> t -> bool
end) =
struct
  open Containers

  type elem = E.t
  type t = elem list * elem list

  let prioritize e (p, u) =
    if not (List.exists (E.equal e) p) then
      let m, n = List.partition (E.equal e) u in
      (p @ m, n)
    else (p, u)

  let deprioritize_all (p, u) = ([], p @ u)
  let cons x (l, r) = (x :: l, r)
  let snoc x (l, r) = (l, x :: r)
  let head_opt = function [], x -> List.head_opt x | h :: _, _ -> Some h

  let rotate = function
    | [], r -> ( match List.rev r with [] -> ([], []) | h :: t -> (t, [ h ]))
    | h :: t, r -> (t, h :: r)

  let find x (p, u) = List.find (E.equal x) (p @ u)

  let get_next x (p, u) =
    let rec go l = function
      | [ h ] when E.equal x h -> List.head_opt l
      | h :: (h' :: _ as t) -> if E.equal x h then Some h' else go l t
      | _ -> None
    in
    Option.or_ (go p p) ~else_:(go u u)
end
