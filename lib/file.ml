let read path =
  try
    if Sys.file_exists path && not (Sys.is_directory path) then
      Ok (In_channel.with_open_text path In_channel.input_all)
    else Error (Printf.sprintf "%s not found" path)
  with Sys_error message -> Error message
