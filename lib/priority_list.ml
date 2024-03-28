module Make (E : sig
  type t

  val equal : t -> t -> bool
end) =
struct
  open Containers

  type elem = E.t
  type t = elem list * elem list

  (* TODO add prioritize_front *)
  let prioritize_back e (p, u) =
    if not (List.exists (E.equal e) p) then
      let m, n = List.partition (E.equal e) u in
      (p @ m, n)
    else (p, u)

  let deprioritize e (p, u) =
    if not (List.exists (E.equal e) u) then
      let m, n = List.partition (E.equal e) p in
      (n, m @ u)
    else (p, u)

  let deprioritize_all (p, u) = ([], p @ u)
  let cons x (l, r) = (x :: l, r)
  let snoc x (l, r) = (l, x :: r)
  let head_opt = function [], x -> List.head_opt x | h :: _, _ -> Some h

  (* let rotate = function *)
  (*   | [], r -> ( match List.rev r with [] -> ([], []) | h :: t -> (t, [ h ])) *)
  (*   | h :: t, r -> (t, h :: r) *)

  let find_opt x (p, u) = List.find_opt (E.equal x) (p @ u)
  let mem x pu = find_opt x pu |> Option.is_some

  let get_next x (p, u) =
    let rec go = function
      | [ h ] when E.equal x h -> head_opt (p, u)
      | h :: (h' :: _ as t) -> if E.equal x h then Some h' else go t
      | _ -> None
    in
    Option.or_ (go p) ~else_:(go u)

  let to_list (p, u) = p @ u
end
