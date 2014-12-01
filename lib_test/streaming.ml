open Core.Std
open Scid

let io_buffer_size = 4096

let () =
  let ic = open_in_bin Sys.argv.(1) in
  let buf = Bytes.create io_buffer_size in
  let buf_e = Bytes.create io_buffer_size in
  let d = D.make `Manual in
  let e = E.make `Manual in
  E.Manual.add_bytes e buf_e 0 io_buffer_size;
  let rec transcode nb_transcoded =
    match D.decode d with
    | `R r -> transcode @@ begin
        E.encode e @@ `R r |> function
        | `Ok -> succ nb_transcoded
        | `Partial ->
          output stdout buf_e 0 (io_buffer_size - E.Manual.rem e);
          E.Manual.add_bytes e buf_e 0 io_buffer_size;
          while E.encode e `Await <> `Ok do () done;
          succ nb_transcoded
      end
    | `End -> assert false
    | `Error `Header_invalid s -> Printf.eprintf "Invalid header %S. aborting.\n" s; exit 1
    | `Error `Eof s -> Printf.eprintf "Premature EOF: %S not parsed.\n" s; exit 1
    | `Await ->
      let rc = input ic buf 0 io_buffer_size in
      if rc > 0 then (D.Manual.refill_bytes d buf 0 rc; transcode nb_transcoded)
      else (output stdout buf_e 0 (io_buffer_size - E.Manual.rem e); nb_transcoded) in
  let nb_transcoded = transcode 0 in
  Printf.eprintf "Decoded %d records.\n%!" nb_transcoded
