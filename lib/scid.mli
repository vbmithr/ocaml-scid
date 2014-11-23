open Core.Std

type t = {
  datetime : float;
  o : float;
  h : float;
  l : float;
  c : float;
  num_trades : int;
  total_volume : int;
  bid_volume : int;
  ask_volume : int;
}
(** SierraChart's s_IntradayRecord *)

val of_bigstring : ?pos:int -> Bigstring.t -> t
val to_bigstring : t -> ?pos:int -> Bigstring.t -> unit

val header_size : int
val record_size : int
val check_header : ?pos:int -> Bigstring.t -> bool

(** Blocking streaming codec. *)
module B : sig

  (** {1 Decoding} *)

  type src = [ `Channel of in_channel | `Bigstring of Bigstring.t ]
  (** The type for input sources. *)

  type decoder
  (** The type for R decoders. *)

  val decoder : [< src] -> decoder
  (** [decoder src] is a decoder that inputs from [src]. *)

  val decode : decoder ->
    [ `R of t | `End
    | `Error of [`Invalid_header of Bigstring.t | `Bytes_unparsed of Bigstring.t ] ]
  (** [decode d] is :
      {ul
      {- [`R r], if a record [r] was decoded.}
      {- [`End], if the end of input was reached.}
      {- [`Error reason], if an error occured. If you are interested in
         a best-effort decoding you can still continue to decode
         after an error.}}

      {b Note.} Repeated invocation always eventually returns [`End], even in
      case of errors. *)

  (** {1 Encoding} *)

  type dst = [ `Channel of out_channel | `Bigbuffer of Bigbuffer.t ]
  (** The type for output destinations. *)

  (* type encoder *)
  (* (\** The type for R encoders. *\) *)

  (* val encoder : [< dst] -> encoder *)
  (* (\** [encoder dst] is an encoder that outputs to [dst]. *\) *)

  (* val encode : encoder -> [< `R of t | `End ] -> unit *)
  (* (\** [encode e v] encodes [v] on [e]. *)

  (*     {b Raises.} [Invalid_argument] if a non well-formed sequence *)
  (*     of records is encoded. *\) *)
end

(** Non-blocking streaming codec. *)
module Nb : sig

  (** {1 Decoding} *)

  type src = [ `Channel of in_channel | `Bigstring of Bigstring.t | `Manual ]
  (** The type for input sources. *)

  type decoder
  (** The type for R decoders. *)

  val decoder : [< src] -> decoder
  (** [decoder src] is a decoder that inputs from src. *)

  val decode : decoder ->
    [ `R of t | `Await | `End
    | `Error of [`Invalid_header of Bigstring.t | `Bytes_unparsed of Bigstring.t ] ]
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

  (** {1 Encoding} *)

  type dst = [ `Channel of out_channel | `Bigbuffer of Bigbuffer.t | `Manual ]
  (** The type for output destinations. *)

  type encoder
  (** The type for R encoders. *)

  (* val encoder : [< dst] -> encoder *)
  (* (\** [encoder dst] is an encoder that outputs to [dst]. *\) *)

  (* val encode : encoder -> [< `Await | `R of t | `End ] -> *)
  (*   [ `Ok | `Partial ] *)
  (* (\** [encode e v] is : *)
  (*     {ul *)
  (*     {- [`Partial] iff [e] has a [`Manual] destination and needs *)
  (*         more output storage. The client must use {!Manual.dst} to provide *)
  (*         a new buffer and then call {!encode} with [`Await] until [`Ok] *)
  (*         is returned.} *)
  (*     {- [`Ok] when the encoder is ready to encode a new [`R] or [`End].}} *)

  (*     For [`Manual] destinations, encoding [`End] always returns *)
  (*     [`Partial], continue with [`Await] until [`Ok] is *)
  (*     returned at which point [Manual.dst_rem e] is guaranteed to be *)
  (*     the size of the last provided buffer (i.e. nothing was written). *)

  (*     {b Raises.} [Invalid_argument] if a non well-formed sequence *)
  (*     of records is encoded or if a [`R] or [`End] is encoded after *)
  (*     a [`Partial] encode. *\) *)

 (** {1 Manual sources and destinations} *)

  (** Manual sources and destinations. *)
  module Manual : sig

    val src : decoder -> Bigstring.t -> int -> int -> unit
    (** [src d s k l] provides [d] with [l] bytes to read,
        starting at [k] in [s]. This byte range is read by calls to {!decode}
        with [d] until [`Await] is returned. To signal the end of input
        call the function with [l = 0].

        {b Warning.} Do not use with non-[`Manual] decoder sources. *)

    (* val dst : encoder -> Bigstring.t -> int -> int -> unit *)
    (* (\** [dst e s k l] provides [e] with [l] bytes to write, *)
    (*     starting at [k] in [s]. This byte range is written by calls *)
    (*     to {!encoder} with [e] until [`Partial] is returned. *)
    (*     Use {!dst_rem} to know the remaining number of non-written *)
    (*     free bytes in [s]. *)

    (*     {b Warning.} Do not use with non-[`Manual] encoder destinations. *)
    (* *\) *)

    (* val dst_rem : encoder -> int *)
    (* (\** [dst_rem e] is the remaining number of non-written, *)
    (*     free bytes in the last buffer provided with {!dst}. *\) *)
  end
end
