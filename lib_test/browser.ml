open Scid

let io_buffer_size = 4096

let () =
  let ic = open_in_bin Sys.argv.(1) in
  let d = D.make @@ Channel ic in
  let rec print () =
    match D.decode d with
    | R r -> Format.printf "%a@." R.pp r; print ()
    | End -> ()
    | Error Header_invalid s -> Printf.eprintf "Invalid header %S. aborting.\n" s; exit 1
    | Error Eof s -> Printf.eprintf "Premature EOF: %S not parsed.\n" s; exit 1
    | Await -> assert false
  in print ()
