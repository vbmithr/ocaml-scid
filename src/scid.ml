(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let io_buffer_size = 4096

module ISet = Set.Make(Int)

module H = struct
  let size = 56

  let init = function
  | 0 -> 'S' | 1 -> 'C' | 2 -> 'I' | 3 -> 'D'
  | 4 -> '8' | 8 -> '(' | 12 -> '\001'
  | _ -> '\000'

  let valid = String.init size init

  let char_valid c = function
  | 0 -> c = 'S' | 1 -> c = 'C' | 2 -> c = 'I' | 3 -> c = 'D'
  | 4 -> c = '8' | 8 -> c = '(' | 12 -> c = '\001'
  | n when ISet.(mem n (of_list [5;6;7;9;10;11])) -> c = '\000'
  | _ -> true

  let check b pos =
    try for i = pos to pos + size - 1 do
        if not @@ char_valid (String.get b i) (i-pos)
        then failwith (Printf.sprintf "Char at pos %d should not be %C" i (String.get b i))
      done; (Ok ())
    with Failure s -> (Error s)

  let write ?(start=0) ?(len=size) b pos =
    if pos + len > Bytes.length b then invalid_arg "bounds";
    String.blit valid start b pos len
end

module R = struct
  type t = {
    datetime: float;
    o: float;
    h: float;
    l: float;
    c: float;
    num_trades: int64;
    total_volume: int64;
    bid_volume: int64;
    ask_volume: int64;
  }

  let pp fmt { datetime ; o ; h ; l ; c ; num_trades ;
               total_volume ; bid_volume ; ask_volume ; } =
    Format.fprintf fmt ".2%f %f %f %f %f %Ld %Ld %Ld %Ld"
      datetime o h l c num_trades total_volume bid_volume ask_volume

  let size = 40

  let mk_with_buf bufsize f =
    let tmpbuf = Bytes.make bufsize '\000' in
    f tmpbuf

  let get_uint32 tmpbuf b p =
    Bytes.blit_string b p tmpbuf 0 4;
    EndianBytes.LittleEndian.get_int64 tmpbuf 0

  let read buf pos =
    let open EndianString.LittleEndian in
    let get_uint32 = mk_with_buf 8 get_uint32 in
    let datetime = get_double buf pos in
    let o = get_int32 buf (pos+8) |> Int32.float_of_bits in
    let h = get_int32 buf (pos+12) |> Int32.float_of_bits in
    let l = get_int32 buf (pos+16) |> Int32.float_of_bits in
    let c = get_int32 buf (pos+20) |> Int32.float_of_bits in
    let num_trades = get_uint32 buf (pos+24) in
    let total_volume = get_uint32 buf (pos+28) in
    let bid_volume = get_uint32 buf (pos+32) in
    let ask_volume = get_uint32 buf (pos+36) in
    {
      datetime; o; h; l; c; num_trades; total_volume; bid_volume; ask_volume
    }

  let read_bytes buf pos =
    read (Bytes.unsafe_to_string buf) pos

  let write r buf pos =
    let open EndianBytes.LittleEndian in
    let set_int64_uint32 tmpbuf b p i =
      set_int64 tmpbuf 0 i;
      Bytes.blit tmpbuf 0 b p 4 in
    let set_int64_uint32 = mk_with_buf 8 set_int64_uint32 in
    r.datetime |> Int64.bits_of_float |> set_int64 buf pos;
    r.o |> Int32.bits_of_float |> set_int32 buf (pos+8);
    r.h |> Int32.bits_of_float |> set_int32 buf (pos+12);
    r.l |> Int32.bits_of_float |> set_int32 buf (pos+16);
    r.c |> Int32.bits_of_float |> set_int32 buf (pos+20);
    r.num_trades |> set_int64_uint32 buf (pos+24);
    r.total_volume |> set_int64_uint32 buf (pos+28);
    r.bid_volume |> set_int64_uint32 buf (pos+32);
    r.ask_volume |> set_int64_uint32 buf (pos+36)

  let compare r r' =
    let b = Bytes.make size '\000' in
    let b' = Bytes.make size '\000' in
    write r b 0;
    write r' b' 0;
    compare b b'

  let equal r r' = compare r r' = 0
end

type auto
(** type of auto sources *)

type manual
(** type of manual sources *)

module D = struct
  type _ src =
    | Channel : in_channel -> auto src
    | String : string -> auto src
    | Bytes : Bytes.t -> auto src
    | Manual : manual src

  type error =
    | Header_invalid of string
    | Eof of string

  let pp_error ppf = function
  | Header_invalid msg -> Format.fprintf ppf "Header invalid: %s" msg
  | Eof msg -> Format.fprintf ppf "End of file: %s" msg

  type decode_result =
    | R of R.t
    | Await
    | End
    | Error of error

  type 'a t = {
    src: 'a src;
    partial: Bytes.t;
    mutable p_pos: int;
    mutable eoi: bool;
    mutable st: [ `H | `R ];
    mutable buf: Bytes.t;
    mutable pos: int;
    mutable max: int;
    mutable k: 'a t -> decode_result;
  }

  let eoi d =
    d.buf <- Bytes.create 0; d.pos <- max_int; d.max <- 0; d.eoi <- true
  module Manual = struct
    let refill_string d b p l =
      if p < 0 || l < 0 || p + l > String.length b then invalid_arg "bounds";
      if l = 0 then eoi d else
        d.buf <- Bytes.unsafe_of_string b; d.pos <- p ; d.max <- p + l - 1
    let refill_bytes d b p l =
      if p < 0 || l < 0 || p + l > Bytes.length b then invalid_arg "bounds";
      if l = 0 then eoi d else d.buf <- b; d.pos <- p ; d.max <- p + l - 1
  end

  let refill :
    type a. (a t -> decode_result) -> a t -> decode_result = fun k d ->
    match d.src with
    | String _ -> eoi d; k d
    | Bytes _ -> eoi d; k d
    | Manual -> d.k <- k; Await
    | Channel ic ->
      let rc = input ic d.buf 0 (Bytes.length d.buf) in
      Manual.refill_bytes d d.buf 0 rc; k d

  let rec r_header k d =
    let can_read = d.max - d.pos + 1 in
    assert (d.st = `H);
    if d.p_pos = 0 && can_read >= H.size then
      let pos = d.pos in d.pos <- d.pos + H.size; d.st <- `R;
      match H.check (Bytes.unsafe_to_string d.buf) pos with
      | Ok () -> _decode k d
      | Error _ ->
          k d @@ Error (Header_invalid (Bytes.sub_string d.buf pos H.size))
    else
      let len = min can_read (H.size - d.p_pos) in
      Bytes.blit d.buf d.pos d.partial d.p_pos len;
      d.pos <- d.pos + len;
      d.p_pos <- (d.p_pos + len) mod H.size;
      if d.p_pos = 0 then begin d.st <- `R;
        match H.check (Bytes.unsafe_to_string d.partial) 0 with
        | Ok () -> _decode k d
        | Error _ ->
            k d @@ Error (Header_invalid Bytes.(sub_string d.partial 0 H.size))
      end
      else _decode k d

  and r_record k d =
    let can_read = d.max - d.pos + 1 in
    if d.p_pos = 0 && can_read >= R.size then
      let pos = d.pos in d.pos <- d.pos + R.size; k d (R (R.read_bytes d.buf pos))
    else
      let len = min can_read (R.size - d.p_pos) in
      Bytes.blit d.buf d.pos d.partial d.p_pos len;
      d.pos <- d.pos + len;
      d.p_pos <- (d.p_pos + len) mod R.size;
      if d.p_pos = 0 then k d (R (R.read_bytes d.partial 0)) else _decode k d

  and _decode k d =
    if d.eoi then
      match d.st with
      | `H when d.p_pos = 0 -> End
      | `R when d.p_pos = 0 -> End
      | _ ->
        let l = d.p_pos in
        d.p_pos <- 0;
        Error (Eof Bytes.(sub_string d.partial 0 l))
    else if d.max - d.pos + 1 < 1 then refill (_decode k) d
    else if d.st = `H then r_header k d
    else r_record k d

  let rec ret d result = d.k <- _decode ret; result

  let make' src buf pos max =
    { src = src; eoi = false;
      partial = Bytes.create H.size; p_pos = 0;
      st = `H; buf; pos; max;
      k = _decode ret
    }

  let make : type a. a src -> a t = fun src ->
    let buf, pos, max = match src with
    | Manual -> Bytes.create 0, 1, 0
    | Channel _ -> Bytes.create io_buffer_size, 1, 0
    | String s -> Bytes.unsafe_of_string s, 0, String.length s - 1
    | Bytes b -> b, 0, Bytes.length b - 1
    in
    make' src buf pos max

  let of_string s =
    make' (String s) (Bytes.unsafe_of_string s) 0 (String.length s - 1)
  let of_bytes b =
    make' (Bytes b) b 0 (Bytes.length b - 1)
  let of_channel ch =
    make' (Channel ch) (Bytes.create io_buffer_size) 1 0
  let manual () =
    make' Manual (Bytes.create 0) 1 0

  let decode d = d.k d
end

(* Encode *)

module E = struct
  type _ dst =
    | Channel : out_channel -> auto dst
    | Buffer : Buffer.t -> auto dst
    | Manual : manual dst

  type encode =
    | Await
    | End
    | R of R.t

  type encode_result = [`Ok | `Partial]

  type 'a t = {
    dst: 'a dst;
    partial: Bytes.t;
    mutable p_pos: int;
    mutable st: [ `H | `R ];
    mutable buf : Bytes.t;
    mutable pos : int;
    mutable max : int;
    mutable k : 'a t -> encode -> encode_result
  }

  module Manual = struct
    let add_bytes e buf pos max =
      if pos < 0 || max < 0 || pos + max > Bytes.length buf then invalid_arg "bounds"
      else e.buf <- buf; e.pos <- pos; e.max <- pos + max - 1
    let rem e = e.max - e.pos + 1
  end

  let partial k e = function
  | Await -> k e
  | R _
  | End -> invalid_arg "cannot encode now, use Await first"

  let flush :
    type a. (a t -> encode_result) -> a t -> encode_result =
    fun k e -> match e.dst with
    | Manual -> e.k <- partial k; `Partial
    | Buffer b -> Buffer.add_subbytes b e.buf 0 e.pos; e.pos <- 0; k e
    | Channel oc -> output oc e.buf 0 e.pos; e.pos <- 0; k e

  let rec encode_h k e =
    let can_write = e.max - e.pos + 1 in
    if can_write < 1 then flush (encode_h k) e
    else begin
      assert (e.st <> `R);
      if e.p_pos = 0 && can_write >= H.size
      then (H.write e.buf e.pos; e.pos <- e.pos + H.size; e.st <- `R; k e)
      else
        let len = min can_write (H.size - e.p_pos) in
        H.write e.buf e.pos ~start:e.p_pos ~len;
        e.pos <- e.pos + len;
        e.p_pos <- (e.p_pos + len) mod H.size;
        if e.p_pos = 0 then (e.st <- `R; k e) else (encode_h k e)
    end

  let rec encode_r k r e =
    let can_write = e.max - e.pos + 1 in
    if can_write < 1 then flush (encode_r k r) e
    else begin
      assert (e.st = `R);
      if e.p_pos = 0 && can_write >= R.size
      then (R.write r e.buf e.pos; e.pos <- e.pos + R.size; k e)
      else
        let len = min can_write (R.size - e.p_pos) in
        if e.p_pos = 0 then R.write r e.partial 0;
        Bytes.blit e.partial e.p_pos e.buf e.pos len;
        e.pos <- e.pos + len;
        e.p_pos <- (e.p_pos + len) mod R.size;
        if e.p_pos = 0 then (k e) else (encode_r k r e)
    end

  let _encode k e v = match v with
  | End -> flush k e
  | Await -> k e
  | R r ->
    if e.st = `H then encode_h (encode_r k r) e
    else encode_r k r e

  let rec ret e = e.k <- _encode ret; `Ok

  let make' dst buf pos max =
    { dst = dst; buf; pos; max; partial = Bytes.create H.size; p_pos = 0;
      st = `H; k = _encode ret }

  let make : type a. a dst -> a t = fun dst ->
    let buf, pos, max = match dst with
    | Manual -> Bytes.create 0, 1, 0
    | Channel _ -> Bytes.create io_buffer_size, 0, io_buffer_size - 1
    | Buffer _ -> Bytes.create io_buffer_size, 0, io_buffer_size - 1
    in
    make' dst buf pos max

  let of_channel ch =
    make' (Channel ch) (Bytes.create io_buffer_size) 0 (io_buffer_size - 1)
  let of_buffer buf =
    make' (Buffer buf) (Bytes.create io_buffer_size) 0 (io_buffer_size - 1)
  let manual () =
    make' Manual (Bytes.create 0) 1 0

  let encode e v = e.k e (v :> encode)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
