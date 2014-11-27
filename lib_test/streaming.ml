open Core.Std
open Scid

let io_buffer_size = 4096

let () =
  let ic = open_in_bin Sys.argv.(1) in
  let buf = Bytes.create io_buffer_size in
  let d = D.make `Manual in
  let rec decode acc =
    match D.decode d with
    | `Yield r -> decode @@ r::acc
    | `End -> assert false
    | `Error `Header_invalid -> Printf.eprintf "Invalid header. aborting.\n"; exit 1
    | `Error `Bytes_unparsed _ -> Printf.eprintf "Error while parsing.\n"; exit 1
    | `Await ->
      let rc = input ic buf 0 io_buffer_size
      in if rc > 0 then
        begin
          D.Manual.src d buf 0 rc;
          decode acc
        end
      else acc
  in
  let recs = decode [] in
  Printf.eprintf "Decoded %d records so far.\n%!" (List.length recs)
