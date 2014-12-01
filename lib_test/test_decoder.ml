open Scid

let io_buffer_size = 4096

let r_of_ba ba pos =
  let open Bigarray.Array1 in
  if pos < 0 || pos > dim ba then invalid_arg "bounds"
  else
    let b = Bytes.create R.size in
    for i = 0 to R.size - 1 do Bytes.set b i @@ get ba (pos + i) done;
    R.read b 0

let () =
  let ic = open_in_bin Sys.argv.(1) in
  let iclen = in_channel_length ic in
  let fd = Unix.(openfile Sys.argv.(1) [O_RDONLY] 0o644) in
  let mapped_file = Bigarray.(Array1.map_file fd Char C_layout false iclen) in
  Printf.eprintf "Mapped %s of length %d.\n" Sys.argv.(1) iclen;
  let buf = Bytes.create io_buffer_size in
  let d = D.make `Manual in
  let rec decode nb_decoded = match D.decode d with
  | `R r ->
    let r' = r_of_ba mapped_file (H.size + nb_decoded * R.size) in
    if (R.compare r r' = 0) then decode (succ nb_decoded)
    else begin
      Printf.eprintf "Record %d incorrectly decoded (at offset %d)\n"
        nb_decoded (H.size + nb_decoded * R.size);
      Printf.eprintf "Decoded %S\nWas %S\n"
        (let b = Bytes.create R.size in R.write r b 0; b)
        (let b = Bytes.create R.size in R.write r' b 0; b);
      exit 1 end
  | `End -> assert false
  | `Error `Header_invalid s -> Printf.eprintf "Invalid header %S. aborting.\n" s; exit 1
  | `Error `Eof s -> Printf.eprintf "Premature EOF: %S not parsed.\n" s; exit 1
  | `Await ->
    let rc = input ic buf 0 io_buffer_size in
    if rc > 0 then (D.Manual.refill_bytes d buf 0 rc; decode nb_decoded)
    else nb_decoded in
  let nb_decoded = decode 0 in
  Printf.eprintf "Decoded %d records.\n%!" nb_decoded
