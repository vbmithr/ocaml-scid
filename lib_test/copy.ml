open Core.Std

let () =
  let ic = open_in_bin Sys.argv.(1) in
  let d = Scid.Nb.decoder @@ `Channel ic in
  let rec read_forever acc =
    match Scid.Nb.decode d with
    | `R r -> read_forever @@ r::acc
    | `End -> acc
    | `Error `Invalid_header bs ->
      Cstruct.(of_bigarray bs |> hexdump);
      Printf.eprintf "Invalid header. aborting.\n"; exit 1
    | `Error `Bytes_unparsed bs ->
      Cstruct.(of_bigarray bs |> hexdump);
      Printf.eprintf "Error while parsing: %d bytes not parsed.\n"
        (Bigstring.length bs);
      acc
    | _ -> assert false
  in
  let recs = read_forever [] in
  Printf.printf "Read %d records.\n%!" (List.length recs);

  (* let oc = open_out_bin Sys.argv.(2) in *)
  (* let e = B.encoder @@ `Channel oc in *)
  (* List.iter (fun r -> B.encode e @@ `R r) recs; *)
  (* B.encode e `End *)
