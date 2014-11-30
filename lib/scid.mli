module H : sig
  val valid : string
  (** A valid SCID header. *)

  val size : int
  (** [size] is the size of the SCID header, in Bytes.t. *)

  val check : Bytes.t -> int -> [ `Ok | `Error of string ]
  (** [check b p] is the [`Ok] if [b] contains a valid header starting
      at position [p], and [`Error] otherwise *)

  val write : ?start:int -> ?len:int -> Bytes.t -> int -> unit
  (** [write ~start ~len b p] write a portion of a valid SCID header
      starting from offset [start], of length [len], to [b] at offset
      [p]. *)
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

  val size : int
  (** [size] is the size of a (serialized) record, in Bytes.t. *)

  val empty : t
  (** [empty] is a record where all fields are zero. *)

  val read : Bytes.t -> int -> t
  (** [read ~pos b] is the record serialized in [b] at offset
      [pos]. *)

  val write : t -> Bytes.t -> int -> unit
  (** [write t ~pos b] writes [t] in [b] at offset [pos]. *)
end

(** {1 Decoding} *)

module D : sig
  type src = [ `Channel of in_channel | `String of string | `Manual ]
  (** The type for input sources. *)

  type e = [ `Header_invalid of string | `Eof of string ]
  (** The type of errors. *)

  type t
  (** The type for decoders. *)

  val make : [< src] -> t
  (** [decoder src] is a decoder that inputs from src. *)

  val decode : t -> [ `R of R.t | `Await | `End | `Error of e ]

  module Manual : sig
    val refill_string : t -> string -> int -> int -> unit
    val refill_bytes : t -> Bytes.t -> int -> int -> unit
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

  module Manual : sig
    val add_string : t -> string -> int -> int -> unit
    val add_bytes : t -> Bytes.t -> int -> int -> unit
    val rem : t -> int
  end
end
