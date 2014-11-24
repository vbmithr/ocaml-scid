open Core.Std

let io_buffer_size = 4096
let header_size = 56
let record_size = 40

let check_header ?(pos=0) bs = let open Bigstring in
  true
  && length bs = header_size
  && sub_shared bs ~pos ~len:4 = (of_string "SCID")
  && unsafe_get_int32_le bs ~pos:(pos+4) = 56
  && unsafe_get_int32_le bs ~pos:(pos+8) = 40
  && unsafe_get_int16_le bs ~pos:(pos+12) = 1
  && unsafe_get_int32_le bs ~pos:(pos+16) = 0

(* The DateTime member variable is a double precision floating-point
    value. The integer part of the value is the number of days since
    midnight, 30 December 1899. The fractional part of the value
    represents time. .5 would represent 12:00 PM. *)
let unix_time_of_sc_time v = (v -. 25571.) *. 86400.
let sc_time_of_unix_time v = v /. 86400. +. 25571.

type t = {
  datetime: float;
  o: float;
  h: float;
  l: float;
  c: float;
  num_trades: int;
  total_volume: int;
  bid_volume: int;
  ask_volume: int
}

let of_bigstring ?(pos=0) buf =
  let open Bigstring in
  let datetime = unsafe_get_int64_t_le ~pos buf |> Int64.float_of_bits in
  let o = unsafe_get_int32_t_le ~pos:(pos+8) buf |> Int32.float_of_bits in
  let h = unsafe_get_int32_t_le ~pos:(pos+12) buf |> Int32.float_of_bits in
  let l = unsafe_get_int32_t_le ~pos:(pos+16) buf |> Int32.float_of_bits in
  let c = unsafe_get_int32_t_le ~pos:(pos+20) buf |> Int32.float_of_bits in
  let num_trades = unsafe_get_int32_le ~pos:(pos+24) buf in
  let total_volume = unsafe_get_int32_le ~pos:(pos+28) buf in
  let bid_volume = unsafe_get_int32_le buf ~pos:(pos+32) in
  let ask_volume = unsafe_get_int32_le buf ~pos:(pos+36) in
  {
    datetime; o; h; l; c; num_trades; total_volume; bid_volume; ask_volume
  }

let to_bigstring r ?(pos=0) buf =
  let open Bigstring in
  r.datetime |> Int64.bits_of_float |> unsafe_set_int64_t_le buf ~pos;
  r.o |> Int32.bits_of_float |> unsafe_set_int32_t_le buf ~pos:(pos+8);
  r.h |> Int32.bits_of_float |> unsafe_set_int32_t_le buf ~pos:(pos+12);
  r.l |> Int32.bits_of_float |> unsafe_set_int32_t_le buf ~pos:(pos+16);
  r.c |> Int32.bits_of_float |> unsafe_set_int32_t_le buf ~pos:(pos+20);
  r.num_trades |> unsafe_set_int32_le buf ~pos:(pos+24);
  r.total_volume |> unsafe_set_int32_le buf ~pos:(pos+28);
  r.bid_volume |> unsafe_set_int32_le buf ~pos:(pos+32);
  r.ask_volume |> unsafe_set_int32_le buf ~pos:(pos+36)

module B = struct
  type src = [ `Fd of UnixLabels.file_descr | `Bigstring of Bigstring.t ]
  type decoder = {
    src: src;
    mutable header_read: [`None | `Good | `Bad];
    mutable i: Bigstring.t;
    mutable i_read: int;
    mutable i_written: int;
  }

  (* This is correct because of the chosen bufsize for the `Fd
     source. A buffer of size 4096 can exactly contain a header and
     101 records. Unparsed bytes can never wrap around the buffer. *)
  let r_end d =
    if d.i_read = d.i_written then `End
    else
      begin
        let pos = (d.i_read mod Bigstring.length d.i) in
        let len = d.i_written - d.i_read in
        d.i_read <- d.i_written;
        `Error (`Bytes_unparsed (Bigstring.sub d.i ~pos ~len))
      end

  let refill d = match d.src with
    | `Bigstring _ -> 0
    | `Fd fd ->
      let bufsize = Bigstring.length d.i in
      let len = min
          (bufsize - (d.i_written mod bufsize))
          (bufsize + d.i_read - d.i_written) in
      let rc =
        try Bigstring.read fd d.i ~pos:d.i_written ~len
        with Bigstring.IOError (rc, End_of_file) -> rc in
      d.i_written <- d.i_written + rc;
      rc

  let rec r_record d =
    if d.i_read = d.i_written
    then (let rc = refill d in if rc = 0 then r_end d else r_record d)
    else if d.i_written - d.i_read >= record_size then
      begin
        d.i_read <- d.i_read + record_size;
        `R (of_bigstring d.i ~pos:(d.i_read mod Bigstring.length d.i))
      end
    else
      let rc = refill d in if rc = 0 then r_end d else r_record d

  let rec r_header d =
    if d.i_read = d.i_written
    then (let rc = refill d in if rc = 0 then r_end d else r_header d)
    else if d.i_written - d.i_read >= header_size then
      begin
        d.i_read <- d.i_read + header_size;
        if check_header d.i then
          (d.header_read <- `Good; r_record d)
        else
          (d.header_read <- `Bad;
           `Error (`Invalid_header (Bigstring.sub d.i ~pos:0 ~len:header_size)))
      end
    else
      let rc = refill d in if rc = 0 then r_end d else r_header d

  let decoder src =
    let i, i_read, i_written = match src with
      | `Bigstring s -> s, 0, Bigstring.length s
      | `Fd _ -> Bigstring.create io_buffer_size, 0, 0
    in
    { src = (src :> src); header_read = `None; i; i_read; i_written; }

  let decode d = match d.header_read with
    | `None -> r_header d
    | `Good -> r_record d
    | `Bad -> r_end d

  (* Encoding *)

  type dst = [ `Fd of UnixLabels.file_descr | `Bigbuffer of Bigbuffer.t ]
end

module Nb = struct
  type src = [ `Fd of UnixLabels.file_descr | `Bigstring of Bigstring.t | `Manual ]
  type decoder = {
    src: src;
    b: Bigstring.t;
    mutable b_pos: int;
    mutable header_read: bool;
    mutable i: Bigstring.t;
    mutable i_pos: int;
    mutable i_max: int;
    mutable k: decoder ->
      [ `R of t | `Await | `End
      | `Error of [`Invalid_header of Bigstring.t | `Bytes_unparsed of Bigstring.t ] ];
  }

  let eoi d =
    d.i <- Bigstring.create 0;
    d.i_pos <- Int.max_value;
    d.i_max <- 0

  let decode_src d s j l =
    if (l = 0) then eoi d else (d.i <- s; d.i_pos <- j; d.i_max <- j + l - 1;)

  let refill k d = match d.src with
    | `Manual -> d.k <- k; `Await
    | `Bigstring _ -> eoi d; k d
    | `Fd fd ->
      let rc =
        try Bigstring.read fd d.i ~pos:0
        with Bigstring.IOError (rc, End_of_file) -> rc
      in
      decode_src d d.i 0 rc;
      k d

  let r_end k d =
    if d.b_pos = 0 then k d `End
    else
      let len = d.b_pos in
      d.b_pos <- 0;
      k d @@ `Error (`Bytes_unparsed (Bigstring.sub d.b ~pos:0 ~len))

  let rec r_record k d =
    if d.i_pos > d.i_max then
      (if Bigstring.length d.i = 0 && d.src <> `Manual
       then r_end k d else refill (r_record k) d)
    else begin
      let want_read = record_size - d.b_pos in
      let can_read = d.i_max - d.i_pos + 1 in
      let len = min can_read want_read in
      Bigstring.blit ~src:d.i ~src_pos:d.i_pos ~dst:d.b ~dst_pos:d.b_pos ~len;
      d.i_pos <- d.i_pos + len;
      k d @@ `R (of_bigstring d.b)
    end

  let rec r_header k d =
    if d.i_pos > d.i_max then
      (if Bigstring.length d.i = 0 && d.src <> `Manual
       then r_end k d else refill (r_header k) d)
    else begin
      let want_read = header_size - d.b_pos in
      let can_read = d.i_max - d.i_pos + 1 in
      let len = min can_read want_read in
      Bigstring.blit ~src:d.i ~src_pos:d.i_pos ~dst:d.b ~dst_pos:d.b_pos ~len;
      d.i_pos <- d.i_pos + len;
      if d.b_pos + len = header_size then
        begin
          d.b_pos <- 0; d.header_read <- true;
          if check_header d.b then r_record k d else `Error (`Invalid_header d.b)
        end
      else (d.b_pos <- d.b_pos + len; r_header k d)
    end

  let rec ret d result =
    d.k <- (if d.header_read then r_record else r_header) ret;
    result

  let decoder src =
    let i, i_pos, i_max = match src with
      | `Manual -> Bigstring.create 0, Int.max_value, 0
      | `Bigstring s -> s, 0, Bigstring.length s - 1
      | `Fd _ -> Bigstring.create io_buffer_size, Int.max_value, 0
    in
    { src = (src :> src);
      b = Bigstring.create header_size;
      b_pos = 0;
      header_read = false;
      i; i_pos; i_max; k = r_header ret }

  let decode d = d.k d

  (* Encode *)

  type dst = [ `Channel of out_channel | `Bigbuffer of Bigbuffer.t | `Manual ]
  type encode = [ `Await | `End | `R of t]
  type encoder =
    { dst: dst;
      mutable o : string;
      mutable o_pos : int;
      mutable o_max : int;
      mutable k :
        encoder -> encode -> [ `Partial | `Ok ] }

  let encode_dst e s j l =
    if (j < 0 || l < 0 || j + l > String.length s) then invalid_arg "bounds";
    e.o <- s; e.o_pos <- j; e.o_max <- j + l - 1

  let partial k e = function `Await -> k e
  | `R _ | `End -> invalid_arg "cannot encode now, use `Await first"

  let flush k e = match e.dst with
  | `Manual -> e.k <- partial k; `Partial
  | `Bigbuffer b -> Bigbuffer.add_substring b e.o 0 e.o_pos; e.o_pos <- 0; k e
  | `Channel oc -> output oc e.o 0 e.o_pos; e.o_pos <- 0; k e

  module Manual = struct
    let src = decode_src
    let dst = encode_dst
  end
end


