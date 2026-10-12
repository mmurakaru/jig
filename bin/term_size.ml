(* Real terminal geometry, re-queried on every repaint so a resize cannot
   corrupt the frame. ioctl first; environment as the piped/odd-tty
   fallback. Floors keep the box layout sane on tiny terminals. *)

external ioctl_columns : unit -> int = "jig_terminal_columns"
external ioctl_rows : unit -> int = "jig_terminal_rows"

let from_env name fallback =
  match Sys.getenv_opt name with
  | Some value -> ( try int_of_string value with _ -> fallback)
  | None -> fallback

let columns () =
  let measured = ioctl_columns () in
  max 40 (if measured > 0 then measured else from_env "COLUMNS" 120)

let rows () =
  let measured = ioctl_rows () in
  max 10 (if measured > 0 then measured else from_env "LINES" 40)
