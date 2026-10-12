(* Sanitize untrusted step output for fixed-width terminal layout. The
   contract: the display width of a sanitized string equals its scalar
   count, so callers can pad and truncate by counting scalars. ANSI escape
   sequences and control characters are stripped, tabs expand to 8-column
   stops, and characters a terminal may render two columns wide (CJK,
   emoji) become '?' - full logs on disk keep the original bytes. *)

let tab_stop = 8

(* Ranges terminals commonly render double-width. Deliberately coarse:
   a false positive costs one '?' in a transient tail view. *)
let is_wide code =
  (code >= 0x1100 && code <= 0x115F)
  || (code >= 0x2E80 && code <= 0xA4CF)
  || (code >= 0xAC00 && code <= 0xD7A3)
  || (code >= 0xF900 && code <= 0xFAFF)
  || (code >= 0xFE30 && code <= 0xFE4F)
  || (code >= 0xFF00 && code <= 0xFF60)
  || (code >= 0xFFE0 && code <= 0xFFE6)
  || (code >= 0x1F000 && code <= 0x1FAFF)
  || (code >= 0x20000 && code <= 0x3FFFD)

(* Skip one escape sequence starting at [start] (which holds ESC); returns
   the index just past it. CSI runs to its final byte, OSC to BEL or ST. *)
let skip_escape text start =
  let length = String.length text in
  if start + 1 >= length then length
  else
    match text.[start + 1] with
    | '[' ->
        let position = ref (start + 2) in
        while
          !position < length
          && not (text.[!position] >= '\x40' && text.[!position] <= '\x7e')
        do
          incr position
        done;
        min length (!position + 1)
    | ']' ->
        let position = ref (start + 2) in
        let finished = ref false in
        while not !finished && !position < length do
          if text.[!position] = '\x07' then (
            incr position;
            finished := true)
          else if
            text.[!position] = '\x1b'
            && !position + 1 < length
            && text.[!position + 1] = '\\'
          then (
            position := !position + 2;
            finished := true)
          else incr position
        done;
        !position
    | _ -> start + 2

let line text =
  let buffer = Buffer.create (String.length text) in
  let column = ref 0 in
  let emit_char c =
    Buffer.add_char buffer c;
    incr column
  in
  let emit_utf8 u =
    Buffer.add_utf_8_uchar buffer u;
    incr column
  in
  let position = ref 0 in
  let length = String.length text in
  while !position < length do
    let byte = text.[!position] in
    if byte = '\x1b' then position := skip_escape text !position
    else if byte = '\t' then (
      let target = ((!column / tab_stop) + 1) * tab_stop in
      while !column < target do
        emit_char ' '
      done;
      incr position)
    else if byte < ' ' || byte = '\x7f' then incr position
    else if byte < '\x80' then (
      emit_char byte;
      incr position)
    else
      let decode = String.get_utf_8_uchar text !position in
      position := !position + Uchar.utf_decode_length decode;
      if not (Uchar.utf_decode_is_valid decode) then emit_char '?'
      else
        let u = Uchar.utf_decode_uchar decode in
        if is_wide (Uchar.to_int u) then emit_char '?' else emit_utf8 u
  done;
  Buffer.contents buffer

(* Scalar count of a sanitized string - by the contract above, its display
   width. Safe on any valid UTF-8 (each glyph counts as one). *)
let width text =
  let count = ref 0 in
  let position = ref 0 in
  let length = String.length text in
  while !position < length do
    let decode = String.get_utf_8_uchar text !position in
    position := !position + Uchar.utf_decode_length decode;
    incr count
  done;
  !count

(* First [limit] scalars of a sanitized string. *)
let truncate limit text =
  if width text <= limit then text
  else
    let position = ref 0 in
    let taken = ref 0 in
    let length = String.length text in
    while !taken < limit && !position < length do
      let decode = String.get_utf_8_uchar text !position in
      position := !position + Uchar.utf_decode_length decode;
      incr taken
    done;
    String.sub text 0 !position
