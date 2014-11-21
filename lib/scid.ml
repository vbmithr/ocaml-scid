cstruct intraday_header {
  uint8_t header[4];
  uint32_t header_size;
  uint32_t record_size;
  uint16_t version;
  uint16_t unused1;
  uint32_t utcstartindex;
  uint8_t reserve[36]
} as little_endian

cstruct intraday_record {
  uint64_t datetime;
  uint32_t o;
  uint32_t h;
  uint32_t l;
  uint32_t c;
  uint32_t num_trades;
  uint32_t total_volume;
  uint32_t bid_volume;
  uint32_t ask_volume
} as little_endian

let valid_header, valid_header_str =
  let hdr = Cstruct.create sizeof_intraday_header in
  Bigarray.Array1.fill hdr.Cstruct.buffer '\000';
  set_intraday_header_header "SCID" 0 hdr;
  set_intraday_header_header_size hdr 56l;
  set_intraday_header_record_size hdr 40l;
  set_intraday_header_version hdr 1;
  set_intraday_header_utcstartindex hdr 0l;
  hdr, Cstruct.to_string hdr

let input_header ?(buf=Bytes.create 56) ic =
  really_input ic buf 0 56;
  if buf <> valid_header_str
  then raise (Invalid_argument "Corrupted SCID file.")

let output_header oc =
  output oc valid_header_str 0 56

(* The DateTime member variable is a double precision floating-point
    value. The integer part of the value is the number of days since
    midnight, 30 December 1899. The fractional part of the value
    represents time. .5 would represent 12:00 PM. *)
let unix_time_of_sc_time v = (v -. 25571.) *. 86400.

let sc_time_of_unix_time v = v /. 86400. +. 25571.

let input_records f ic =
  let recs = ref [] in
  let buf = Bytes.create 56 in
  input_header ~buf ic;
  let record = Cstruct.create 40 in
  try
    while true do
      really_input ic buf 0 40;
      Cstruct.blit_from_string buf 0 record 0 40;
      recs := (f record)::!recs
    done; []
  with End_of_file ->
    !recs

let output_records f oc recs =
    let buf = Cstruct.create 40 in
    let buf_str = Bytes.create 40 in
    output_header oc;
    List.iter (fun r -> f ~buf r;
               Cstruct.blit_to_string buf 0 buf_str 0 40;
               output oc buf_str 0 40) recs

module IDR = struct
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

  let of_cstruct buf =
  let datetime = get_intraday_record_datetime buf
                 |> Int64.float_of_bits
                 |> unix_time_of_sc_time in
  let o = get_intraday_record_o buf
          |> Int32.float_of_bits in
  let h = get_intraday_record_h buf
          |> Int32.float_of_bits in
  let l = get_intraday_record_l buf
          |> Int32.float_of_bits in
  let c = get_intraday_record_c buf
          |> Int32.float_of_bits in
  let num_trades = get_intraday_record_num_trades buf |> Int32.to_int in
  let total_volume = get_intraday_record_total_volume buf |> Int32.to_int in
  let bid_volume = get_intraday_record_bid_volume buf |> Int32.to_int in
  let ask_volume = get_intraday_record_ask_volume buf |> Int32.to_int in
  {
    datetime; o; h; l; c; num_trades; total_volume; bid_volume; ask_volume
  }

  let to_cstruct ~buf r =
    r.datetime
    |> sc_time_of_unix_time
    |> Int64.bits_of_float
    |> set_intraday_record_datetime buf;
    r.o
    |> Int32.bits_of_float
    |> set_intraday_record_o buf;
    r.h
    |> Int32.bits_of_float
    |> set_intraday_record_h buf;
    r.l
    |> Int32.bits_of_float
    |> set_intraday_record_l buf;
    r.c
    |> Int32.bits_of_float
    |> set_intraday_record_c buf;
    r.num_trades
    |> Int32.of_int
    |> set_intraday_record_num_trades buf;
    r.total_volume
    |> Int32.of_int
    |> set_intraday_record_total_volume buf;
    r.bid_volume
    |> Int32.of_int
    |> set_intraday_record_bid_volume buf;
    r.ask_volume
    |> Int32.of_int
    |> set_intraday_record_ask_volume buf

  let of_channel = input_records of_cstruct
  let to_channel = output_records to_cstruct


  module Nb = struct
    type src = [ `Channel of in_channel | `String of string | `Manual ]
    type decoder = {
      src: src;
      mutable buf: bytes;
      mutable i_pos: int;
      mutable i_max: int;
      mutable loc: [`BH of int | `AH of int];
      mutable k:
        decoder -> [ `R of t | `Await | `End | `Error ];
    }

    let eoi d = d.buf <- ""; d.i_pos <- max_int; d.i_max <- 0

    let decoder src = ()

    let r_block
    let decoder src =
      let io_buffer_size = 4096 in
      let i, i_pos, i_max = match src with
        | `Manual -> "", max_int, 0
        | `String s -> s, 0, String.length s - 1
        | `Channel _ -> String.create io_buffer_size, max_int, 0
      in
      { src = (src :> src); i; i_pos; i_max; k = r_lexeme ret }

    let decode d = d.k d
  end

  module Manual = struct
  end
end

module Tick = struct
  type t = {
    datetime: float;
    price: float;
    amount: int;
    direction: [ `Bid | `Ask ]
  }

  let of_cstruct buf =
    let datetime = get_intraday_record_datetime buf
                   |> Int64.float_of_bits
                   |> unix_time_of_sc_time in
    let o = get_intraday_record_o buf
            |> Int32.float_of_bits in
    let total_volume = get_intraday_record_total_volume buf |> Int32.to_int in
    let ask_volume = get_intraday_record_ask_volume buf in
    let direction = if ask_volume = 0l then `Bid else `Ask in
    { datetime; price=o; amount=total_volume; direction }

  let to_cstruct ~buf r =
    let cstr_price = r.price |> Int32.bits_of_float in
    let cstr_amount = r.amount |> Int32.of_int in
    let cstr_datetime = r.datetime |> sc_time_of_unix_time |> Int64.bits_of_float in
    set_intraday_record_datetime buf cstr_datetime;
    set_intraday_record_o buf cstr_price;
    set_intraday_record_h buf cstr_price;
    set_intraday_record_l buf cstr_price;
    set_intraday_record_c buf cstr_price;
    set_intraday_record_num_trades buf 1l;
    set_intraday_record_total_volume buf cstr_amount;
    if r.direction = `Ask
    then
      (set_intraday_record_ask_volume buf cstr_amount;
       set_intraday_record_bid_volume buf 0l)
    else
      (set_intraday_record_bid_volume buf cstr_amount;
       set_intraday_record_ask_volume buf 0l)

  let of_channel = input_records of_cstruct
  let to_channel = output_records to_cstruct
end
