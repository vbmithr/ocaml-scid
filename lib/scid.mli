module IDR : sig
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

  val of_cstruct : Cstruct.t -> t
  val to_cstruct : buf:Cstruct.t -> t -> unit

  val of_channel : in_channel -> t list
  val to_channel : out_channel -> t list -> unit
end

module Tick : sig
  type t = {
    datetime : float;
    price : float;
    amount : int;
    direction : [ `Ask | `Bid ];
  }
  (** Custom type representing just one tick. (Not in SierraChart.) *)

  val of_cstruct : Cstruct.t -> t
  val to_cstruct : buf:Cstruct.t -> t -> unit

  val of_channel : in_channel -> t list
  val to_channel : out_channel -> t list -> unit
end
