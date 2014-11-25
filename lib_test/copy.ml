open Core.Std
open Async.Std

let io_buffer_size = 4096

let main fn =
  Reader.open_file fn >>= fun r ->
  let buf = Bigstring.create io_buffer_size in
  let d = Scid.Nb.decoder @@ `Manual buf in
  let rec read_forever acc =
    match Scid.Nb.decode d with
    | `R r -> read_forever @@ r::acc
    | `End -> return acc
    | `Error `Header_invalid bs ->
      Printf.eprintf "Invalid header %S. Aborting.\n"
        (Bigstring.to_string bs);
      return []
    | `Error `Bytes_unparsed bs ->
      Printf.eprintf "Error while parsing: %d bytes not parsed.\n"
        (Bigstring.length bs);
      return acc
    | `Await (pos, len) ->
      Reader.read_bigsubstring r @@ Bigsubstring.create buf ~pos ~len >>= function
      | `Eof -> return acc
      | `Ok len -> Scid.Nb.Manual.src d buf len; read_forever acc
  in
  read_forever [] >>= fun recs ->
  Printf.printf "Read %d records.\n%!" (List.length recs);
  Shutdown.exit 0

let () =
  don't_wait_for @@ main Sys.argv.(1);
  never_returns @@ Scheduler.go ()
