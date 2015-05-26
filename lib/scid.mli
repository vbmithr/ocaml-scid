(** {1 SCID objects} *)

module H : sig
  val valid : string
  (** The valid SCID header. *)

  val size : int
  (** [size] is the size of the SCID header, in bytes. *)

  val check : Bytes.t -> int -> [ `Ok | `Error of string ]
  (** [check b p] is [`Ok] if [b] contains a valid header starting at
      [p], and [`Error] otherwise *)

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
  } [@@deriving show,create]
  (** SierraChart's s_IntradayRecord *)

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
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  (** The type for input sources. *)

  type e = [ `Header_invalid of string | `Eof of string ] [@@deriving show]
  (** The type for errors. *)

  type t
  (** The type for decoders. *)

  val make : [< src] -> t
  (** [decoder src] is a decoder that inputs from src. *)

  val decode : t -> [ `R of R.t | `Await | `End | `Error of e ]
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
  type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]
  (** The type for output destinations. *)

  type t
  (** The type for R encoders. *)

  val make : [< dst] -> t
  (** [encoder dst] is an encoder that outputs to [dst]. *)

  val encode : t -> [< `Await | `End | `R of R.t ] -> [ `Ok | `Partial ]
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
