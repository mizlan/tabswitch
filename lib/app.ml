type t = Kitty | Firefox | Skim | Neovide [@@deriving compare]

let pp f = function
  | Kitty -> Format.fprintf f "kitty"
  | Firefox -> Format.fprintf f "Firefox"
  | Skim -> Format.fprintf f "Skim"
  | Neovide -> Format.fprintf f "Neovide"

let of_string = function
  | "kitty" -> Some Kitty
  | "Firefox" -> Some Firefox
  | "Skim" -> Some Skim
  | "Neovide" -> Some Neovide
  | _ -> None

let launch = function
  | Kitty -> Lwt_process.exec ("kitty", [| "kitty"; "-1" |])
  | Firefox -> Lwt_process.exec ("open", [| "open"; "-a"; "Firefox" |])
  | Skim -> Lwt_process.exec ("open", [| "open"; "-a"; "Skim" |])
  | Neovide ->
      Lwt_process.exec
        ("neovide", [| "/Applications/Neovide.app/Contents/MacOS/neovide" |])
