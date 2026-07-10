open Result_syntax

(* Loads a forEach items file: one (column, value) binding list per item,
   column order preserved. The first column is the item's key - its stable
   identity in position lines, status output, and step provenance - so it
   must be non-empty and unique across items.

   TSV is kept dumb on purpose: a header row, fields split on tab, one
   trailing '\r' trimmed per line. No quoting, no escaping - a value cannot
   contain a tab or a newline. Any quoting dialect imports ambiguity that
   paths and identifiers never need. *)

let split_on_tabs line = String.split_on_char '\t' line

let trim_carriage_return line =
  let length = String.length line in
  if length > 0 && line.[length - 1] = '\r' then String.sub line 0 (length - 1)
  else line

let check_keys ~path items =
  let* () =
    match
      List.find_opt
        (fun bindings ->
          match bindings with [] -> true | (_, key) :: _ -> key = "")
        items
    with
    | Some _ -> Error (Printf.sprintf "items: %s has an empty item key" path)
    | None -> Ok ()
  in
  let keys =
    List.filter_map
      (fun bindings ->
        match bindings with [] -> None | (_, key) :: _ -> Some key)
      items
  in
  match
    List.find_opt
      (fun key -> List.length (List.filter (fun k -> k = key) keys) > 1)
      keys
  with
  | Some key ->
      Error (Printf.sprintf "items: %s has duplicate item key %S" path key)
  | None -> Ok ()

let load_tsv ~path content =
  let lines = String.split_on_char '\n' content in
  let lines =
    match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
  in
  let lines = List.map trim_carriage_return lines in
  match lines with
  | [] -> Error (Printf.sprintf "items: %s contains no items" path)
  | header_line :: rows ->
      let header = split_on_tabs header_line in
      let* () =
        match
          List.find_opt
            (fun column ->
              List.length (List.filter (fun c -> c = column) header) > 1)
            header
        with
        | Some column ->
            Error
              (Printf.sprintf "items: %s has duplicate column %S" path column)
        | None -> Ok ()
      in
      let width = List.length header in
      let* items =
        List.fold_right
          (fun (line_number, row) accumulator ->
            let* items = accumulator in
            if row = "" then
              Error
                (Printf.sprintf "items: %s line %d is empty" path line_number)
            else
              let fields = split_on_tabs row in
              if List.length fields <> width then
                Error
                  (Printf.sprintf
                     "items: %s line %d has %d fields but the header has %d"
                     path line_number (List.length fields) width)
              else Ok (List.combine header fields :: items))
          (List.mapi (fun index row -> (index + 2, row)) rows)
          (Ok [])
      in
      if items = [] then
        Error (Printf.sprintf "items: %s contains no items" path)
      else
        let* () = check_keys ~path items in
        Ok items

let load_json ~path content =
  let* json =
    match Yojson.Safe.from_string content with
    | json -> Ok json
    | exception Yojson.Json_error message ->
        Error (Printf.sprintf "items: %s is not valid json: %s" path message)
  in
  let* items =
    match json with
    | `List entries ->
        List.fold_right
          (fun entry accumulator ->
            let* items = accumulator in
            let index = List.length items in
            match entry with
            | `Assoc pairs ->
                let* bindings =
                  List.fold_right
                    (fun (key, value) bindings ->
                      let* bindings = bindings in
                      match value with
                      | `String text -> Ok ((key, text) :: bindings)
                      | _ ->
                          Error
                            (Printf.sprintf
                               "items: %s item %d key %S must be a string"
                               path index key))
                    pairs (Ok [])
                in
                Ok (bindings :: items)
            | _ ->
                Error
                  (Printf.sprintf "items: %s must be a json array of objects"
                     path))
          entries (Ok [])
    | _ ->
        Error (Printf.sprintf "items: %s must be a json array of objects" path)
  in
  if items = [] then Error (Printf.sprintf "items: %s contains no items" path)
  else
    (* Every object must share the shape of the first: same first (key)
       column, and no item may miss a column another one binds - ragged
       objects make {{ var.column }} a partial function. *)
    let* () =
      match items with
      | first :: rest ->
          let columns = List.map fst first in
          List.fold_left
            (fun accumulator bindings ->
              let* () = accumulator in
              if List.map fst bindings <> columns then
                Error
                  (Printf.sprintf
                     "items: %s items must all have the same keys in the \
                      same order"
                     path)
              else Ok ())
            (Ok ()) rest
      | [] -> Ok ()
    in
    let* () = check_keys ~path items in
    Ok items

let load ~path =
  let* content =
    Result.map_error (fun message -> "items: " ^ message) (File.read path)
  in
  if Filename.check_suffix path ".tsv" then load_tsv ~path content
  else if Filename.check_suffix path ".json" then load_json ~path content
  else
    Error
      (Printf.sprintf "items: %s must be a .tsv or .json file" path)

(* The stable key of one item: its first column's value. *)
let key bindings = match bindings with [] -> "" | (_, value) :: _ -> value

(* Every referenced column must exist in every item - checked at validate
   time and again when a forEach entry starts. *)
let check_columns ~path ~required items =
  List.fold_left
    (fun accumulator (index, bindings) ->
      let* () = accumulator in
      match
        List.find_opt
          (fun column -> not (List.mem_assoc column bindings))
          required
      with
      | Some column ->
          Error
            (Printf.sprintf "items: %s item %d has no column %S" path index
               column)
      | None -> Ok ())
    (Ok ())
    (List.mapi (fun index bindings -> (index, bindings)) items)

(* The columns one item binds. *)
let columns bindings = List.map fst bindings
