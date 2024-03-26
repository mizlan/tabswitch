type t = Kitty [@name "kitty"] | Firefox | Skim [@@deriving compare]

let launch = function
  | Kitty -> Lwt_process.exec ("kitty", [| "kitty"; "-1" |])
  | Firefox -> Lwt_process.exec ("open", [| "open"; "-a"; "Firefox" |])
  | Skim -> Lwt_process.exec ("open", [| "open"; "-a"; "Skim" |])
