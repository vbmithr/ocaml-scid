open Core.Std

let io_buffer_size = 4096

let () =
  let fd = UnixLabels.(openfile Sys.argv.(1) ~mode:[O_RDONLY] ~perm:0o644) in
  let buf = Bigstring.create io_buffer_size in
  let d = Scid.Nb.decoder `Manual in
  let rec decode acc =
    match Scid.Nb.decode d with
    | `R r -> decode @@ r::acc
    | `End -> assert false
    | `Error `Invalid_header bs ->
      Printf.eprintf "Invalid header. aborting.\n"; exit 1
    | `Error `Bytes_unparsed bs ->
      Printf.eprintf "Error while parsing: %d bytes not parsed.\n"
        (Bigstring.length bs); exit 1
    | `Await ->
      let rc =
        try Bigstring.read fd buf
        with Bigstring.IOError (rc, End_of_file) -> rc
      in if rc > 0 then
        begin
          Scid.Nb.Manual.src d buf 0 rc;
          decode acc
        end
      else acc
  in
  let recs = decode [] in
  Printf.eprintf "Decoded %d records so far.\n%!" (List.length recs)
