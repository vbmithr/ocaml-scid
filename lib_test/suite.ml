open Core.Std
open OUnit2

module Decode = struct
  let buf0 = Bigstring.create 0
  let buf3 = Bigstring.create 3
  let good_hdr = Bigstring.create 56
  let bad_hdr =
    let b = Bigstring.init 56 (fun _ -> '\000') in
    let h = "SCID\070\000\000\000\050\000\000\000\001\000" in
    Bigstring.From_string.blit ~src:h ~dst:b ~src_pos:0 ~dst_pos:0 ~len:14;
    b

  let empty ctx =
    let d = Scid.B.decoder @@ `Bigstring buf0 in
    assert_equal (Scid.B.decode d) `End

  let empty_nb ctx =
    let d = Scid.Nb.decoder @@ `Bigstring buf0 in
    assert_equal (Scid.Nb.decode d) `End

  let empty_nb_manual ctx =
    let d = Scid.Nb.decoder @@ `Manual in
    assert_equal (Scid.Nb.decode d) `Await;
    Scid.Nb.Manual.src d @@ Bigsubstring.create buf0;
    assert_equal (Scid.Nb.decode d) `Await

  let invalid_hdr ctx =
    let d = Scid.B.decoder @@ `Bigstring buf3 in
    assert_equal (Scid.B.decode d) (`Error (`Bytes_unparsed buf3));
    assert_equal (Scid.B.decode d) `End;
    let d = Scid.B.decoder @@ `Bigstring bad_hdr in
    assert_equal (Scid.B.decode d) (`Error (`Invalid_header bad_hdr));
    assert_equal (Scid.B.decode d) `End

  let invalid_hdr_nb ctx =
    let d = Scid.Nb.decoder @@ `Bigstring buf3 in
    assert_equal (Scid.Nb.decode d) (`Error (`Bytes_unparsed buf3));
    assert_equal (Scid.Nb.decode d) `End;
    let d = Scid.Nb.decoder @@ `Bigstring bad_hdr in
    assert_equal (Scid.Nb.decode d) (`Error (`Invalid_header bad_hdr));
    assert_equal (Scid.Nb.decode d) `End
end

module Encode = struct
end

let suite =
  "decode" >:::
  Decode.[
    "empty" >:: empty;
    "empty_nb" >:: empty_nb;
    "empty_nb_manual" >:: empty_nb_manual;
    "invalid_hdr" >:: invalid_hdr;
    "invalid_hdr_nb" >:: invalid_hdr_nb;
  ]

let () = run_test_tt_main suite

