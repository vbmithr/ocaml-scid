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

type sc_record = {
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

type sc_tick_record = {
  datetime: float;
  price: float;
  amount: int
}

type t =
  [
  | `Tick of sc_tick_record list
  | `Normal of sc_record list
  ]

(* The DateTime member variable is a double precision floating-point
    value. The integer part of the value is the number of days since
    midnight, 30 December 1899. The fractional part of the value
    represents time. .5 would represent 12:00 PM. *)
let unix_time_of_sc_time v = (v -. 25571.) *. 86400.

let sc_record_of_cstruct buf =
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

let sc_tick_record_of_cstruct buf =
  let datetime = get_intraday_record_datetime buf
                 |> Int64.float_of_bits
                 |> unix_time_of_sc_time in
  let o = get_intraday_record_o buf
          |> Int32.float_of_bits in
  let total_volume = get_intraday_record_total_volume buf |> Int32.to_int in
  { datetime; price=o; amount=total_volume }

let load ?(tick=false) filename =
  let recs = ref [] in
  let tick_recs = ref [] in
  let buf = String.create 56 in
  let ic = open_in filename in
  really_input ic buf 0 56;
  let hdr = Cstruct.of_string buf in
  if get_intraday_header_header hdr |> Cstruct.to_string <> "SCID"
     || get_intraday_header_version hdr <> 1
     || get_intraday_header_utcstartindex hdr <> 0l
  then
    raise (Invalid_argument "Corrupted SCID file.");
  let record = Cstruct.create 40 in
  try
    while true do
      really_input ic buf 0 40;
      Cstruct.blit_from_string buf 0 record 0 40;
      if tick then
        tick_recs := (sc_tick_record_of_cstruct record)::!tick_recs
      else
        recs := (sc_record_of_cstruct record)::!recs
    done; `Tick []
  with End_of_file ->
    close_in ic;
    if tick then `Tick (List.rev !tick_recs) else `Normal (List.rev !recs)

