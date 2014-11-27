open Core.Std
open Async.Std
open Scid

let io_buffer_size = 4096

let main fn =
  Reader.open_file fn >>= fun r ->
  let buf = Bytes.create io_buffer_size in
  let d = D.make @@ `Manual in
  let rec read_forever acc =
    match D.decode d with
    | `Yield r -> read_forever @@ r::acc
    | `End -> return acc
    | `Error `Header_invalid -> Printf.eprintf "Invalid header. Aborting.\n";
      return []
    | `Error `Bytes_unparsed bs ->
      Printf.eprintf "Error while parsing: %S not parsed.\n" bs;
      return acc
    | `Await ->
      Reader.read r buf >>= function
      | `Eof -> return acc
      | `Ok len -> D.Manual.src d buf 0 io_buffer_size; read_forever acc
  in
  read_forever [] >>= fun recs ->
  Printf.printf "Read %d records.\n%!" (List.length recs);
  Shutdown.exit 0

let () =
  don't_wait_for @@ main Sys.argv.(1);
  never_returns @@ Scheduler.go ()
