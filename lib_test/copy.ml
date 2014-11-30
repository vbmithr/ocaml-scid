open Core.Std
open Async.Std
open Scid

let io_buffer_size = 4096

let main fn =
  Reader.open_file fn >>= fun r ->
  let buf = Bytes.create io_buffer_size in
  let buf_e = Bytes.create io_buffer_size in
  let d = D.make `Manual in
  let e = E.make `Manual in
  E.Manual.add_bytes e buf_e 0 io_buffer_size;
  let rec read_forever n =
    match D.decode d with
    | `R r -> begin
        E.encode e @@ `R r |> function
        | `Ok -> read_forever (succ n)
        | `Partial ->
          assert (E.Manual.rem e = 0);
          Writer.(write (Lazy.force stdout) buf_e
                    ~len:(io_buffer_size - E.Manual.rem e));
          E.Manual.add_bytes e buf_e 0 io_buffer_size;
          while E.encode e `Await <> `Ok do () done;
          read_forever (succ n)
      end
    | `End -> assert false
    | `Error `Header_invalid s ->
      Printf.eprintf "Invalid header %S. Aborting.\n" s; return n
    | `Error `Eof bs ->
      Printf.eprintf "Premature EOF: %S not parsed.\n" bs; return n
    | `Await -> Reader.read r buf >>= function
      | `Eof ->
        Writer.(write (Lazy.force stdout) buf_e
                  ~len:(io_buffer_size - E.Manual.rem e));
        return n
      | `Ok len -> D.Manual.refill_bytes d buf 0 len; read_forever n
  in
  read_forever 0 >>= fun n ->
  Printf.eprintf "Read %d records.\n" n;
  Shutdown.exit 0

let () =
  don't_wait_for @@ main Sys.argv.(1);
  never_returns @@ Scheduler.go ()
