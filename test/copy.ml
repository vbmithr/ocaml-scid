open Scid

let () =
  let ic = open_in Sys.argv.(1) in
  let oc = open_out Sys.argv.(2) in
  let recs = input_sc_records ic in
  Printf.printf "Copied %d records.\n%!" (List.length recs);
  output_sc_records oc recs
