open Core.Std
open Types

let io_buffer_size = 4096
let header_len = 56
let record_len = 40

let check_header bs = let open Bigstring in
  true
  && length bs = header_len
  && sub_shared bs ~pos:0 ~len:4 = (of_string "SCID")
  && unsafe_get_int32_le bs ~pos:4 = 56
  && unsafe_get_int32_le bs ~pos:8 = 40
  && unsafe_get_int16_le bs ~pos:12 = 1
  && unsafe_get_int32_le bs ~pos:16 = 0

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

let of_bigstring ?off ?len buf =
  let buf = Cstruct.of_bigarray ?off ?len buf in
  let datetime = get_intraday_record_datetime buf |> Int64.float_of_bits |> unix_time_of_sc_time in
  let o = get_intraday_record_o buf |> Int32.float_of_bits in
  let h = get_intraday_record_h buf |> Int32.float_of_bits in
  let l = get_intraday_record_l buf |> Int32.float_of_bits in
  let c = get_intraday_record_c buf |> Int32.float_of_bits in
  let num_trades = get_intraday_record_num_trades buf |> Int32.to_int_exn in
  let total_volume = get_intraday_record_total_volume buf |> Int32.to_int_exn in
  let bid_volume = get_intraday_record_bid_volume buf |> Int32.to_int_exn in
  let ask_volume = get_intraday_record_ask_volume buf |> Int32.to_int_exn in
  {
    datetime; o; h; l; c; num_trades; total_volume; bid_volume; ask_volume
  }

let to_bigstring r ?off ?len buf =
  let buf = Cstruct.of_bigarray ?off ?len buf in
  r.datetime |> sc_time_of_unix_time |> Int64.bits_of_float |> set_intraday_record_datetime buf;
  r.o |> Int32.bits_of_float |> set_intraday_record_o buf;
  r.h |> Int32.bits_of_float |> set_intraday_record_h buf;
  r.l |> Int32.bits_of_float |> set_intraday_record_l buf;
  r.c |> Int32.bits_of_float |> set_intraday_record_c buf;
  r.num_trades |> Int32.of_int_exn |> set_intraday_record_num_trades buf;
  r.total_volume |> Int32.of_int_exn |> set_intraday_record_total_volume buf;
  r.bid_volume |> Int32.of_int_exn |> set_intraday_record_bid_volume buf;
  r.ask_volume |> Int32.of_int_exn |> set_intraday_record_ask_volume buf

module B = struct
  type src = [ `Channel of in_channel | `Bigstring of Bigstring.t ]
  type decoder = {
    src: src;
    b: Bigstring.t;
    mutable header_read: bool;
    mutable b_pos: int;
    mutable i: Bigstring.t;
    mutable i_pos: int;
    mutable i_max: int;
  }

  let eoi d =
    d.i <- Bigstring.create 0;
    d.i_pos <- Int.max_value;
    d.i_max <- 0

  let refill d = match d.src with
    | `Bigstring _ -> eoi d
    | `Channel ic ->
      let rc =
        try Bigstring.input ic d.i ~pos:0
        with Bigstring.IOError (rc, End_of_file) -> rc in
      if rc = 0 then (eoi d) else (d.i_pos <- 0; d.i_max <- rc - 1;)

  let r_end d = if d.b_pos = 0 then `End
    else
      let len = d.b_pos in
      d.b_pos <- 0;
      `Error (`Bytes_unparsed (Bigstring.sub d.b ~pos:0 ~len))

  let rec r_record d =
    if d.i_pos > d.i_max
    then (if Bigstring.length d.i = 0 then r_end d else (refill d; r_record d))
    else begin
      let want_read = record_len - d.b_pos in
      let can_read = d.i_max - d.i_pos + 1 in
      let len = min can_read want_read in
      Bigstring.blit ~src:d.i ~src_pos:d.i_pos ~dst:d.b ~dst_pos:d.b_pos ~len;
      d.i_pos <- d.i_pos + len;
      if d.b_pos + len = record_len then (d.b_pos <- 0; `R (of_bigstring d.b))
      else (d.b_pos <- d.b_pos + len; r_record d)
    end

  let rec r_header d =
    if d.i_pos > d.i_max
    then (if Bigstring.length d.i = 0 then r_end d else (refill d; r_header d))
    else begin
      let want_read = header_len - d.b_pos in
      let can_read = d.i_max - d.i_pos + 1 in
      let len = min can_read want_read in
      Bigstring.blit ~src:d.i ~src_pos:d.i_pos ~dst:d.b ~dst_pos:d.b_pos ~len;
      d.i_pos <- d.i_pos + len;
      if d.b_pos + len = header_len then
        begin
          d.b_pos <- 0; d.header_read <- true;
          if check_header d.b then r_record d else `Error (`Invalid_header d.b)
        end
      else (d.b_pos <- d.b_pos + len; r_header d)
    end

  let decoder src =
    let i, i_pos, i_max = match src with
      | `Bigstring s -> s, 0, Bigstring.length s - 1
      | `Channel _ -> Bigstring.create io_buffer_size, Int.max_value, 0
    in
    { src = (src :> src);
      b = Bigstring.create header_len;
      b_pos = 0;
      header_read = false;
      i; i_pos; i_max; }

  let decode d = if d.header_read then r_record d else r_header d

  (* Encoding *)

  type dst = [ `Channel of out_channel | `Bigbuffer of Bigbuffer.t ]
end

module Nb = struct
  type src = [ `Channel of in_channel | `Bigstring of Bigstring.t | `Manual ]
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
    if (j < 0 || l < 0 || j + l > Bigstring.length s) then invalid_arg "bounds";
    if (l = 0) then eoi d else
      (d.i <- s; d.i_pos <- j; d.i_max <- j + l - 1;)

  let refill k d = match d.src with
    | `Manual -> d.k <- k; `Await
    | `Bigstring _ -> eoi d; k d
    | `Channel ic ->
      let rc =
        try Bigstring.input ic d.i ~pos:0
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
      let want_read = record_len - d.b_pos in
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
      let want_read = header_len - d.b_pos in
      let can_read = d.i_max - d.i_pos + 1 in
      let len = min can_read want_read in
      Bigstring.blit ~src:d.i ~src_pos:d.i_pos ~dst:d.b ~dst_pos:d.b_pos ~len;
      d.i_pos <- d.i_pos + len;
      if d.b_pos + len = header_len then
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
      | `Channel _ -> Bigstring.create io_buffer_size, Int.max_value, 0
    in
    { src = (src :> src);
      b = Bigstring.create header_len;
      b_pos = 0;
      header_read = false;
      i; i_pos; i_max; k = r_header ret }

  let decode d = d.k d

  module Manual = struct
    let src = decode_src
  end
end


