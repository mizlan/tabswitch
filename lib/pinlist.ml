(* open Containers *)
(**)
(* type 'a t = 'a list * 'a list *)
(**)
(* let cons x (l, r) = (x :: l, r) *)
(* let snoc x (l, r) = (l, x :: r) *)
(**)
(* let rotate = function *)
(*   | [], r -> ( match List.rev r with [] -> ([], []) | h :: t -> (t, [ h ])) *)
(*   | h :: t, r -> (t, h :: r) *)
(**)
(* let pin *)
