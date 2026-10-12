(* Messages between the driving jig process and a forked forEach item
   worker: marshalled OCaml values over a pipe. Both ends are the same
   binary, so Marshal's framing (it records each value's size) is the whole
   protocol. Raw reads, not an in_channel, so select(2) on the descriptor
   never misses a message already pulled into a channel buffer. *)

let rec write_all descriptor bytes offset =
  if offset < Bytes.length bytes then
    let written =
      Unix.write descriptor bytes offset (Bytes.length bytes - offset)
    in
    write_all descriptor bytes (offset + written)

(* Blocks until the whole message is in the pipe. *)
let send_worker_message descriptor value =
  write_all descriptor (Marshal.to_bytes value []) 0

type reader = { descriptor : Unix.file_descr; pending : Buffer.t }

let create_reader descriptor = { descriptor; pending = Buffer.create 4096 }

let chunk_size = 65536

(* Every complete message in the pending bytes, oldest first. *)
let take_complete_messages reader =
  let rec take messages =
    let available = Buffer.length reader.pending in
    if available < Marshal.header_size then List.rev messages
    else
      let bytes = Buffer.to_bytes reader.pending in
      let size = Marshal.total_size bytes 0 in
      if available < size then List.rev messages
      else
        let message = Marshal.from_bytes bytes 0 in
        Buffer.clear reader.pending;
        Buffer.add_subbytes reader.pending bytes size (available - size);
        take (message :: messages)
  in
  take []

(* One read of what the pipe holds now: the messages it completed, and
   whether the worker closed its end. The caller fixes the message type. *)
let receive_worker_messages reader =
  let chunk = Bytes.create chunk_size in
  match Unix.read reader.descriptor chunk 0 chunk_size with
  | 0 -> (take_complete_messages reader, true)
  | count ->
      Buffer.add_subbytes reader.pending chunk 0 count;
      (take_complete_messages reader, false)
  | exception Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN), _, _) -> ([], false)
