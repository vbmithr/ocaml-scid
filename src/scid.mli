(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {1 SCID objects} *)

module H : sig
  val valid : string
  (** The valid SCID header. *)

  val size : int
  (** [size] is the size of the SCID header, in bytes. *)

  val check : Bytes.t -> int -> (unit, string) Result.result
  (** [check b p] is [Ok ()] if [b] contains a valid header starting at
      [p], and [Error msg] otherwise *)

  val write : ?start:int -> ?len:int -> Bytes.t -> int -> unit
  (** [write ~start ~len b p] write the portion of the valid SCID
      header starting at [start], of length [len], to [b] at [p]. *)
end

module R : sig
  type t = {
    datetime : float;
    o : float;
    h : float;
    l : float;
    c : float;
    num_trades : int64;
    total_volume : int64;
    bid_volume : int64;
    ask_volume : int64;
  }
  (** SierraChart's s_IntradayRecord *)

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int
  (** [compare r r'] is [Pervasives.compare] of their serialized
      representation. *)

  val size : int
  (** [size] is the size of a (serialized) record, in bytes. *)

  val read : Bytes.t -> int -> t
  (** [read ~pos b] is the record serialized in [b] starting at
      [pos]. *)

  val write : t -> Bytes.t -> int -> unit
  (** [write t ~pos b] writes [t] in [b] at [pos]. *)
end

(** {1 Decoding} *)

module D : sig
  type src =
    | Channel of in_channel
    | String of string
    | Manual
  (** The type for input sources. *)

  type error =
    | Header_invalid of string
    | Eof of string
    (** The type for errors. *)

  val pp_error : Format.formatter -> error -> unit

  type t
  (** The type for decoders. *)

  val make : src -> t
  (** [decoder src] is a decoder that inputs from src. *)

  type decode_result =
    | R of R.t
    | Await
    | End
    | Error of error

  val decode : t -> decode_result
  (** [decode d] is:
      {ul
      {- [`Await] iff [d] has a [`Manual] input source and awaits
         for more input. The client must use {!Manual.src} to provide it.}
      {- [`R r], if a record [r] was decoded.}
      {- [`End], if the end of input was reached.}
      {- [`Error], if an error occured. If you are interested in
         a best-effort decoding you can still continue to decode
         after an error.}}

      {b Note.} Repeated invocation always eventually returns [`End], even in
      case of errors. *)

  (** Manual sources. *)
  module Manual : sig
    val refill_string : t -> string -> int -> int -> unit
    (** [refill_string d s k l] provides [d] with [l] bytes to read,
        starting at [k] in [s]. This byte range is read by calls to {!decode}
        with [d] until [`Await] is returned. To signal the end of input
        call the function with [l = 0].

        {b Warning.} Do not use with non-[`Manual] decoder sources. *)

    val refill_bytes : t -> Bytes.t -> int -> int -> unit
    (** See {!refill_string}. *)
  end
end

(** {1 Encoding} *)

module E : sig
  type dst =
    | Channel of out_channel
    | Buffer of Buffer.t
    | Manual
  (** The type for output destinations. *)

  type t
  (** The type for R encoders. *)

  val make : dst -> t
  (** [encoder dst] is an encoder that outputs to [dst]. *)

  type encode =
    | Await
    | End
    | R of R.t

  val encode : t -> encode -> [ `Ok | `Partial ]
  (** [encode e v] is :
      {ul
      {- [`Partial] iff [e] has a [`Manual] destination and needs
      more output storage. The client must use {!Manual.dst} to provide
      a new buffer and then call {!encode} with [`Await] until [`Ok]
      is returned.}
      {- [`Ok] when the encoder is ready to encode a new [`R] or [`End].}}

      For [`Manual] destinations, encoding [`End] always returns
      [`Partial], continue with [`Await] until [`Ok] is
      returned at which point [Manual.dst_rem e] is guaranteed to be
      the size of the last provided buffer (i.e. nothing was written).

      {b Raises.} [Invalid_argument] if a non well-formed sequence
      of lexemes is encoded or if a [`Lexeme] or [`End] is encoded after
      a [`Partial] encode. *)

  (** Manual destinations. *)
  module Manual : sig
    val add_bytes : t -> Bytes.t -> int -> int -> unit
    (** [add_bytes e s k l] provides [e] with [l] bytes to write,
        starting at [k] in [s]. This byte range is written by calls to
        {!encode} with [e] until [`Partial] is returned.  Use {!rem}
        to know the remaining number of non-written free bytes in [s].

        {b Warning.} Do not use with non-[`Manual] encoder destinations.
    *)

    val rem : t -> int
    (** [rem e] is the remaining number of non-written, free bytes
        in the last buffer provided with {!add_bytes}. *)
  end
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
