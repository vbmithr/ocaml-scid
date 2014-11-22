open Core.Std

let io_buffer_size = 4096

let () =
  let ic = open_in_bin Sys.argv.(1) in
  let buf = Bigstring.create io_buffer_size in
  let d = Scid.Nb.decoder `Manual in
  let rec read_forever acc =
    match Scid.Nb.decode d with
    | `R r -> read_forever @@ r::acc
    | `End -> assert false
    | `Error `Invalid_header bs ->
      Printf.eprintf "Invalid header. aborting.\n"; exit 1
    | `Error `Bytes_unparsed bs ->
      Printf.eprintf "Error while parsing: %d bytes not parsed.\n"
        (Bigstring.length bs); exit 1
    | `Await ->
      let rec refill () =
        try Bigstring.input ic buf
        with
        | Bigstring.IOError (0, End_of_file) ->
          Printf.eprintf "Decoded %d records so far\n%!" (List.length acc);
          Unix.sleep 2; refill ()
        | Bigstring.IOError (rc, End_of_file) -> rc
      in
      let rc = refill () in
      Scid.Nb.Manual.src d buf 0 rc;
      read_forever acc
  in never_returns @@ read_forever []
