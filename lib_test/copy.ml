open Scid

let () =
  let ic = open_in_bin Sys.argv.(1) in
  let d = Nb.decoder @@ `Channel ic in
  let rec read_forever acc =
    match Nb.decode d with
    | `R r -> read_forever @@ r::acc
    | `End -> acc
    | `Error `Invalid_header bs ->
      Cstruct.(of_bigarray valid_header |> hexdump);
      Cstruct.(of_bigarray bs |> hexdump);
      Printf.eprintf "Invalid header. aborting.\n"; exit 1
    | `Error `Bytes_unparsed nb ->
      Printf.eprintf "Error while parsing: %d bytes not parsed.\n" nb;
      acc
    | _ -> assert false
  in
  let recs = read_forever [] in
  Printf.printf "Read %d records.\n%!" (List.length recs);

  (* let oc = open_out_bin Sys.argv.(2) in *)
  (* let e = B.encoder @@ `Channel oc in *)
  (* List.iter (fun r -> B.encode e @@ `R r) recs; *)
  (* B.encode e `End *)
