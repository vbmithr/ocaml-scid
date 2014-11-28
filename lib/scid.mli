module H : sig
  type state = [ `Checking of int | `Valid | `Invalid ]

  val size : int
  (** [size] is the size of the SCID header, in Bytes.t. *)

  val check : Bytes.t -> state -> int -> int -> state
  (** [check b st p l] is the new state computed from [st], checking
      [l] bytes of [b] starting at [p] *)

  val write : Bytes.t -> int -> unit
  (** [write b p] write a valid SCID header in [b] at offset [p]. *)
end

module R : sig
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
  type src = [ `Channel of in_channel | `Bytes of Bytes.t | `Manual ]
  (** The type for input sources. *)

  type e = [ `Header_invalid | `Bytes_unparsed of Bytes.t ]
  (** The type of errors. *)

  type t
  (** The type for decoders. *)

  val make : [< src] -> t
  (** [decoder src] is a decoder that inputs from src. *)

  val decode : t -> [ `Yield of R.t | `Await | `End | `Error of e ]

  module Manual : sig
    val src : t -> Bytes.t -> int -> int -> unit
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

  val encode : t -> [< `Await | `End | `Yield of R.t ] -> [ `Ok | `Partial ]

  module Manual : sig
    val dst : t -> Bytes.t -> int -> int -> unit
    val dst_rem : t -> int
  end
end
