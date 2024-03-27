type t = Kitty | Firefox | Skim [@@deriving compare]

let pp f = function
  | Kitty -> Format.fprintf f "kitty"
  | Firefox -> Format.fprintf f "Firefox"
  | Skim -> Format.fprintf f "Skim"

let launch = function
  | Kitty -> Lwt_process.exec ("kitty", [| "kitty"; "-1" |])
  | Firefox -> Lwt_process.exec ("open", [| "open"; "-a"; "Firefox" |])
  | Skim -> Lwt_process.exec ("open", [| "open"; "-a"; "Skim" |])
  | Neovide ->
      Lwt_process.exec
        ("neovide", [| "/Applications/Neovide.app/Contents/MacOS/neovide" |])
