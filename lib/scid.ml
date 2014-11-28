open Core.Std

let io_buffer_size = 4096

module H = struct
  type state = [ `Checking of int | `Valid | `Invalid ]
  let size = 56

  let char_valid c = function
    | 0 -> c = 'S' | 1 -> c = 'C' | 2 -> c = 'I' | 3 -> c = 'D'
    | 4 -> c = '8' | 8 -> c = '(' | 12 -> c = '\001'
    | n when Int.Set.(mem (of_list [5;6;7;9;10;11]) n) -> c = '\000'
    | _ -> true

  let check b st pos len =
    let rec inner st c = match st with
      | `Valid -> `Valid
      | `Invalid -> `Invalid
      | `Checking n ->
        if char_valid c n then
          if n < 55 then `Checking (n+1) else `Valid
        else `Invalid in
    if pos < 0 || len < 0 || pos + len > Bytes.length b
    then invalid_arg "check"
    else let st = ref st in
      for i = pos to pos + len - 1 do
        st := inner !st b.[i]
      done; !st

  let write b pos =
    Bytes.blit "SCID8\000\000\000(\000\000\000\001" 0 b pos 13;
    for i = 13 to size - 1 do Bytes.set b i '\000' done
end

module R = struct
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

  let empty =
    { datetime = 0.; o = 0.; h = 0.; l = 0.; c = 0.;
      num_trades = 0; total_volume = 0; bid_volume = 0;
      ask_volume = 0;
    }

  let size = 40

  (* The DateTime member variable is a double precision floating-point
      value. The integer part of the value is the number of days since
      midnight, 30 December 1899. The fractional part of the value
      represents time. .5 would represent 12:00 PM. *)
  let unix_time_of_sc_time v = (v -. 25571.) *. 86400.
  let sc_time_of_unix_time v = v /. 86400. +. 25571.

  let read buf pos =
    let open EndianBytes.LittleEndian in
    let datetime = get_int64 buf pos |> Int64.float_of_bits in
    let o = get_int32 buf (pos+8) |> Int32.float_of_bits in
    let h = get_int32 buf (pos+12) |> Int32.float_of_bits in
    let l = get_int32 buf (pos+16) |> Int32.float_of_bits in
    let c = get_int32 buf (pos+20) |> Int32.float_of_bits in
    let num_trades = get_int32 buf (pos+24) |> Int32.to_int_exn in
    let total_volume = get_int32 buf (pos+28) |> Int32.to_int_exn in
    let bid_volume = get_int32 buf (pos+32) |> Int32.to_int_exn in
    let ask_volume = get_int32 buf (pos+36) |> Int32.to_int_exn in
    {
      datetime; o; h; l; c; num_trades; total_volume; bid_volume; ask_volume
    }

  let write r buf pos =
    let open EndianBytes.LittleEndian in
    r.datetime |> Int64.bits_of_float |> set_int64 buf pos;
    r.o |> Int32.bits_of_float |> set_int32 buf (pos+8);
    r.h |> Int32.bits_of_float |> set_int32 buf (pos+12);
    r.l |> Int32.bits_of_float |> set_int32 buf (pos+16);
    r.c |> Int32.bits_of_float |> set_int32 buf (pos+20);
    r.num_trades |> Int32.of_int_exn |> set_int32 buf (pos+24);
    r.total_volume |> Int32.of_int_exn |> set_int32 buf (pos+28);
    r.bid_volume |> Int32.of_int_exn |> set_int32 buf (pos+32);
    r.ask_volume |> Int32.of_int_exn |>set_int32 buf (pos+36)
end

module D = struct
  type src = [ `Channel of in_channel | `Bytes of Bytes.t | `Manual ]
  type e = [ `Bytes_unparsed of Bytes.t | `Header_invalid ]
  type t = {
    src: src;
    partial: Bytes.t;
    mutable p_partial: int;
    mutable st: [ `Header of int | `Record of int | `Soi | `Eoi ];
    mutable buf: bytes;
    mutable pos: int;
    mutable max: int;
    mutable k: t -> [ `Yield of R.t | `Await | `End | `Error of e ];
  }

  let new_st st n = match st with
    | `Soi -> if n < H.size then `Header n else `Record ((n - H.size) mod R.size)
    | `Header i -> if i + n < 55 then `Header (i+n) else `Record ((i + n - H.size) mod R.size)
    | `Record i -> `Record ((i + n) mod R.size)
    | `Eoi -> `Eoi

  let eoi d = d.buf <- ""; d.pos <- Int.max_value; d.max <- 0; d.st <- `Eoi
  module Manual = struct
    let src d b p l =
      if p < 0 || l < 0 || p + l > Bytes.length b then invalid_arg "bounds";
      if l = 0 then eoi d
      else d.buf <- b; d.pos <- p ; d.max <- p + l - 1
  end

  let refill k d =
    assert (d.pos > d.max);
    match d.src with
    | `Bytes _ -> eoi d; k d
    | `Manual -> d.k <- k; `Await
    | `Channel ic ->
      let rc = input ic d.buf 0 (Bytes.length d.buf) in
      Manual.src d d.buf 0 rc; k d

  let rec check_header k d =
    let check n =
      match H.(check d.buf (`Checking n) d.pos n) with
      | `Invalid -> d.st <- new_st d.st n; d.pos <- d.pos + n; `Error `Header_invalid
      | `Valid -> d.st <- new_st d.st n; d.pos <- d.pos + n; k d
      | `Checking n ->
        d.pos <- d.pos + n;
        if d.src <> `Manual then
          if d.st = `Header 0 then `End else `Error `Header_invalid
        else (d.st <- new_st d.st n; `Await) in
    let can_read = d.max - d.pos + 1 in
    if can_read < 1 then refill (check_header k) d else check can_read

  let rec r_record k d =
    match d.st with
    | `Record n ->
      if n = R.size && d.max - d.pos + 1 >= R.size then begin
        d.pos <- d.pos + R.size;
        `Yield (R.read d.buf d.pos)
      end
      else begin
        let can_write = min n (d.max - d.pos + 1) in
        Bytes.blit d.buf d.pos d.partial n can_write;
        if can_write = n then
          (d.st <- `Record 0; `Yield (R.read d.partial 0))
        else
          (d.st <- `Record (n - can_write);
           refill (r_record k) d)
      end
    | `Soi | `Header _ -> check_header (r_record k) d
    | `Eoi when d.p_partial <> 0 ->
      `Error (`Bytes_unparsed (Bytes.sub d.partial 0 d.p_partial))
    | `Eoi -> `End

  let rec ret d result = d.k <- r_record ret; result
  let make src =
    let buf, pos, max = match src with
      | `Channel _ -> Bytes.create io_buffer_size, 0, 0
      | `Bytes b -> b, 0, Bytes.length b - 1
      | `Manual -> "", Int.max_value, 0
    in
    { src = (src :> src);
      partial = Bytes.create H.size; p_partial = 0;
      st = `Header 0; buf; pos; max;
      k = r_record ret
    }

  let decode d = d.k d
end

(* Encode *)

module E = struct
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
  type encode = [ `Await | `End | `Yield of R.t]
  type t = {
    dst: dst;
    partial: Bytes.t;
    mutable st: [ `Header of int | `Record of int ];
    mutable buf : Bytes.t;
    mutable pos : int;
    mutable max : int;
    mutable k : t -> encode -> [ `Ok | `Partial ] }

  let partial k e = function `Await -> k e
  | `Yield _ | `End -> invalid_arg "cannot encode now, use `Await first"

  let flush k e = match e.dst with
  | `Manual -> e.k <- partial k; `Partial
  | `Buffer b -> Buffer.add_substring b e.buf 0 e.pos; e.pos <- 0; k e
  | `Channel oc -> output oc e.buf 0 e.pos; e.pos <- 0; k e

  let rec _encode k e v = match v with
    | `Await -> k e
    | `End -> flush k e
    | `Yield r -> match e.st with
      | `Header n ->
        if e.max - e.pos + 1 >= H.size
        then (H.write e.buf e.pos; e.pos <- e.pos + H.size;
              e.st <- `Record 0; _encode k e v)
        else
          begin
            H.write e.partial 0;
            let can_write = e.max - e.pos + 1 in
            Bytes.blit e.partial 0 e.buf e.pos can_write;
            e.max <- e.max + can_write;
            e.st <- `Header (n + can_write);
            flush (fun e -> _encode k e v) e
          end
      | `Record n ->
        if e.max - e.pos + 1 >= R.size
        then (R.write r e.buf e.pos; e.pos <- e.pos + R.size; k e)
        else
          begin
            R.write r e.partial 0;
            let can_write = e.max - e.pos + 1 in
            Bytes.blit e.partial 0 e.buf e.pos can_write;
            e.max <- e.max + can_write;
            e.st <- `Record (n + can_write);
            flush (fun e -> _encode k e v) e
          end

  let rec ret e = e.k <- _encode ret; `Ok
  let make dst =
    let buf, pos, max = match dst with
      | `Manual -> "", Int.max_value, 0
      | `Channel _ | `Buffer _ -> Bytes.create io_buffer_size, 0, io_buffer_size - 1
    in
    { dst = (dst :> dst); buf; pos; max; partial = Bytes.create H.size;
      st = `Header 0; k = _encode ret }

  let encode e v = e.k e (v :> encode)

  module Manual = struct
    let dst e buf pos max =
      if pos < 0 || max < 0 || pos + max > Bytes.length buf then invalid_arg "bounds"
      else e.buf <- buf; e.pos <- pos; e.max <- pos + max - 1

    let dst_rem e = e.max - e.pos + 1
  end
end
