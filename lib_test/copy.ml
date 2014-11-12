open Scid

let () =
  let ic = open_in_bin Sys.argv.(1) in
  let oc = open_out_bin Sys.argv.(2) in
  let recs = IDR.of_channel ic in
  Printf.printf "Copied %d records.\n%!" (List.length recs);
  IDR.to_channel oc recs
