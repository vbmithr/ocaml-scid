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

  val of_cstruct : Cstruct.t -> t
  val to_cstruct : buf:Cstruct.t -> t -> unit

  val of_channel : in_channel -> t list
  val to_channel : out_channel -> t list -> unit
end
