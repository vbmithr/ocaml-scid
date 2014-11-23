open Core.Std
open Async.Std

let io_buffer_size = 4096

let main fn =
  Reader.open_file fn >>= fun r ->
  let buf = Bigstring.create io_buffer_size in
  let d = Scid.Nb.decoder @@ `Manual in
  let rec read_forever acc =
    match Scid.Nb.decode d with
    | `R r -> read_forever @@ r::acc
    | `End -> return acc
    | `Error `Invalid_header bs ->
      Printf.eprintf "Invalid header. aborting.\n";
      return []
    | `Error `Bytes_unparsed bs ->
      Printf.eprintf "Error while parsing: %d bytes not parsed.\n"
        (Bigstring.length bs);
      return acc
    | `Await ->
      Reader.read_bigsubstring r @@ Bigsubstring.create buf >>= function
      | `Eof ->
        return acc
      | `Ok len ->
        Scid.Nb.Manual.src d buf 0 len;
        read_forever acc
  in
  read_forever [] >>= fun recs ->
  Printf.printf "Read %d records.\n%!" (List.length recs);
  Shutdown.exit 0

let () =
  don't_wait_for @@ main Sys.argv.(1);
  never_returns @@ Scheduler.go ()

  (* let oc = open_out_bin Sys.argv.(2) in *)
  (* let e = B.encoder @@ `Channel oc in *)
  (* List.iter (fun r -> B.encode e @@ `R r) recs; *)
  (* B.encode e `End *)
