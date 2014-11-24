open Core.Std
open OUnit2

let random_r = Bigstring.create Scid.record_size

let decode_recode ctx =
  let r = Bigstring.create Scid.record_size in
  Scid.(to_bigstring (of_bigstring random_r ~pos:0) ~pos:0 r);
  assert_equal random_r r

module Decode = struct
  let buf0 = Bigstring.create 0
  let buf3 = Bigstring.create 3
  let bad_hdr = Bigstring.init Scid.header_size (fun _ -> '\000')
  let good_hdr =
    let b = Bigstring.init Scid.header_size (fun _ -> '\000') in
    let h = "SCID8\000\000\000(\000\000\000\001\000" in
    Bigstring.From_string.blito ~src:h ~dst:b ();
    b

  let printer = function
    | `End -> "End"
    | `R r -> "R"
    | `Error (`Header_invalid hdr) ->
      Printf.sprintf "Invalid_hdr %S" (Bigstring.to_string hdr)
    | `Error (`Bytes_unparsed hdr) ->
      Printf.sprintf "Bytes_unparsed %S" (Bigstring.to_string hdr)
    | `Await n -> Printf.sprintf "Await %d" n

  let empty ctx =
    let d = Scid.B.decoder @@ `Bigstring buf0 in
    assert_equal ~printer `End (Scid.B.decode d)

  let empty_nb ctx =
    let d = Scid.Nb.decoder @@ `Bigstring buf0 in
    assert_equal ~printer `End (Scid.Nb.decode d)

  let empty_nb_manual ctx =
    let d = Scid.Nb.decoder @@ `Manual in
    assert_equal ~printer (`Await 0) (Scid.Nb.decode d);
    Scid.Nb.Manual.src d buf0 0 0;
    assert_equal ~printer (`Await 0) (Scid.Nb.decode d)

  let incomplete_hdr ctx =
    let d = Scid.B.decoder @@ `Bigstring buf3 in
    assert_equal ~printer (`Error (`Bytes_unparsed buf3)) (Scid.B.decode d);
    assert_equal ~printer `End (Scid.B.decode d)

  let incomplete_hdr_fd ctx =
    let (rd, wr) = Unix.pipe () in
    let nb_written = Bigstring.write wr buf3 in
    Unix.close wr;
    assert_equal 3 nb_written;
    let d = Scid.B.decoder @@ `Fd rd in
    assert_equal ~printer (`Error (`Bytes_unparsed buf3)) (Scid.B.decode d);
    assert_equal ~printer `End (Scid.B.decode d);
    Unix.close rd

  let valid_hdr ctx =
    let d = Scid.B.decoder @@ `Bigstring good_hdr in
    assert_equal ~printer `End (Scid.B.decode d)

  let invalid_hdr ctx =
    let d = Scid.B.decoder @@ `Bigstring bad_hdr in
    assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (Scid.B.decode d);
    assert_equal ~printer `End (Scid.B.decode d)

  let valid_hdr_fd ctx =
    let (rd, wr) = Unix.pipe () in
    let nb_written = Bigstring.write wr good_hdr in
    Unix.close wr;
    assert_equal Scid.header_size nb_written;
    let d = Scid.B.decoder @@ `Fd rd in
    assert_equal ~printer `End (Scid.B.decode d);
    Unix.close rd

  let invalid_hdr_fd ctx =
    let (rd, wr) = Unix.pipe () in
    let nb_written = Bigstring.write wr bad_hdr in
    Unix.close wr;
    assert_equal Scid.header_size nb_written;
    let d = Scid.B.decoder @@ `Fd rd in
    assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (Scid.B.decode d);
    assert_equal ~printer `End (Scid.B.decode d);
    Unix.close rd

  let incomplete_hdr_nb ctx =
    let d = Scid.Nb.decoder @@ `Bigstring buf3 in
    assert_equal ~printer (`Error (`Bytes_unparsed buf3)) (Scid.Nb.decode d);
    assert_equal ~printer `End (Scid.Nb.decode d)

  let invalid_hdr_nb ctx =
    let d = Scid.Nb.decoder @@ `Bigstring bad_hdr in
    assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (Scid.Nb.decode d);
    assert_equal ~printer `End (Scid.Nb.decode d)

  let incomplete_hdr_fd_nb ctx =
    let (rd, wr) = Unix.pipe () in
    let nb_written = Bigstring.write wr buf3 in
    Unix.close wr;
    assert_equal 3 nb_written;
    let d = Scid.Nb.decoder @@ `Fd rd in
    assert_equal ~printer (`Error (`Bytes_unparsed buf3)) (Scid.Nb.decode d);
    assert_equal ~printer `End (Scid.Nb.decode d);
    Unix.close rd

  let invalid_hdr_fd_nb ctx =
    let (rd, wr) = Unix.pipe () in
    let nb_written = Bigstring.write wr bad_hdr in
    Unix.close wr;
    assert_equal Scid.header_size nb_written;
    let d = Scid.Nb.decoder @@ `Fd rd in
    assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (Scid.Nb.decode d);
    assert_equal ~printer `End (Scid.Nb.decode d);
    Unix.close rd

  let incomplete_hdr_manual ctx =
    let d = Scid.Nb.decoder `Manual in
    assert_equal ~printer (`Await 0) (Scid.Nb.decode d);
    Scid.Nb.Manual.src d buf3 0 3;
    assert_equal ~printer (`Await 0) (Scid.Nb.decode d);
    assert_equal ~printer (`Await 0) (Scid.Nb.decode d)

  let invalid_hdr_manual ctx =
    let d = Scid.Nb.decoder `Manual in
    Scid.Nb.Manual.src d bad_hdr 0 Scid.header_size;
    assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (Scid.Nb.decode d);
    assert_equal ~printer (`Await 56) (Scid.Nb.decode d)
end

module Encode = struct
end

let suite =
  "decode" >:::
  Decode.[
    "decode_recode" >:: decode_recode;
    "empty" >:: empty;
    "empty_nb" >:: empty_nb;
    "empty_nb_manual" >:: empty_nb_manual;

    "incomplete_hdr" >:: incomplete_hdr;
    "incomplete_hdr_fd" >:: incomplete_hdr_fd;
    "invalid_hdr" >:: invalid_hdr;
    "invalid_hdr_fd" >:: invalid_hdr_fd;
    "valid_hdr" >:: valid_hdr;
    "valid_hdr_fd" >:: valid_hdr_fd;

    "incomplete_hdr_nb" >:: incomplete_hdr_nb;
    "invalid_hdr_nb" >:: invalid_hdr_nb;
    "incomplete_hdr_fd_nb" >:: incomplete_hdr_fd_nb;
    "invalid_hdr_fd_nb" >:: invalid_hdr_fd_nb;
    "incomplete_hdr_manual" >:: incomplete_hdr_manual;
    "invalid_hdr_manual" >:: invalid_hdr_manual;
  ]

let () = run_test_tt_main suite

