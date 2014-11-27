open Core.Std
open OUnit2
open Scid

let random_r = Bigstring.create R.size

let decode_recode ctx =
  let r = Bigstring.create R.size in
  (R.write (R.read random_r ~pos:0) ~pos:0 r);
  assert_equal random_r r

type ret_noawait =
  [ `End
  | `Error of
      [ `Bytes_unparsed of Core.Std.Bigstring.t
      | `Header_invalid of Core.Std.Bigstring.t ]
  | `R of R.t ]

type ret =
  [ `Await of int * int
  | `End
  | `Error of
      [ `Bytes_unparsed of Core.Std.Bigstring.t
      | `Header_invalid of Core.Std.Bigstring.t ]
  | `R of R.t ]

let buf0 = Bigstring.create 0
let buf3 = Bigstring.create 3
let bad_hdr = Bigstring.init H.size (fun _ -> '\000')
let good_hdr =
  let b = Bigstring.init H.size (fun _ -> '\000') in
  let h = "SCID8\000\000\000(\000\000\000\001" in
  Bigstring.From_string.blito ~src:h ~dst:b ();
  b

let h_plus_0_5b, h_plus_1b, h_plus_1_5b, h_plus_2b  =
  let a = Bigstring.create (H.size + 1) in
  let b = Bigstring.create (H.size + R.size) in
  let c = Bigstring.create (H.size + R.size + 1) in
  let d = Bigstring.create (H.size + 2 * R.size) in
  Bigstring.blito ~src:good_hdr ~dst:a ();
  Bigstring.blito ~src:good_hdr ~dst:b ();
  Bigstring.blito ~src:good_hdr ~dst:c ();
  Bigstring.blito ~src:good_hdr ~dst:d ();
  a, b, c, d

let h_plus_incompleteb = Bigstring.sub h_plus_1b ~pos:0 ~len:90

let printer = function
  | `End -> "End"
  | `R r -> "R"
  | `Error (`Header_invalid hdr) ->
    Printf.sprintf "Invalid_hdr %S" (Bigstring.to_string hdr)
  | `Error (`Bytes_unparsed hdr) ->
    Printf.sprintf "Bytes_unparsed %S" (Bigstring.to_string hdr)
  | `Await (pos, len) -> Printf.sprintf "Await %d %d" pos len

let cmp_kind a b = match a, b with
  | `End, `End -> true
  | `R _, `R _ -> true
  | `Error (`Header_invalid _), `Error (`Header_invalid _) -> true
  | `Error (`Bytes_unparsed _), `Error (`Bytes_unparsed _) -> true
  | `Await _, `Await _ -> true
  | _ -> false

let empty ctx =
  let d = B.decoder @@ `Bigstring buf0 in
  assert_equal ~printer `End (B.decode d)

let empty_nb_manual ctx =
  let d = Nb.decoder @@ `Manual buf0 in
  assert_equal ~msg:"before_feed" ~printer (`End) (Nb.decode d);
  Nb.Manual.src d buf0 0;
  assert_equal ~msg:"after_feed" ~printer (`End) (Nb.decode d)

let incomplete_hdr ctx =
  let d = B.decoder @@ `Bigstring buf3 in
  assert_equal ~printer (`Error (`Bytes_unparsed buf3)) (B.decode d);
  assert_equal ~printer `End (B.decode d)

let incomplete_hdr_fd ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Bigstring.write wr buf3 in
  Unix.close wr;
  assert_equal 3 nb_written;
  let d = B.decoder @@ `Fd rd in
  assert_equal ~printer (`Error (`Bytes_unparsed buf3)) (B.decode d);
  assert_equal ~printer `End (B.decode d);
  Unix.close rd

let valid_hdr ctx =
  let d = B.decoder @@ `Bigstring good_hdr in
  assert_equal ~printer `End (B.decode d)

let invalid_hdr ctx =
  let d = B.decoder @@ `Bigstring bad_hdr in
  assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (B.decode d);
  assert_equal ~printer `End (B.decode d)

let valid_hdr_fd ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Bigstring.write wr good_hdr in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = B.decoder @@ `Fd rd in
  assert_equal ~printer `End (B.decode d);
  Unix.close rd

let invalid_hdr_fd ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Bigstring.write wr bad_hdr in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = B.decoder @@ `Fd rd in
  assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (B.decode d);
  assert_equal ~printer `End (B.decode d);
  Unix.close rd

let incomplete_r ctx =
  let d = B.decoder @@ `Bigstring h_plus_0_5b in
  assert_equal ~printer
    ~cmp:(cmp_kind :> ret_noawait -> ret_noawait -> bool)
    (`Error (`Bytes_unparsed buf3)) (B.decode d);
  assert_equal ~printer `End (B.decode d)

let incomplete_hdr_fd_nb ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Bigstring.write wr buf3 in
  Unix.close wr;
  assert_equal 3 nb_written;
  let d = Nb.decoder @@ `Fd rd in
  assert_equal ~printer (`Error (`Bytes_unparsed buf3)) (Nb.decode d);
  assert_equal ~printer `End (Nb.decode d);
  Unix.close rd

let invalid_hdr_fd_nb ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Bigstring.write wr bad_hdr in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = Nb.decoder @@ `Fd rd in
  assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (Nb.decode d);
  assert_equal ~printer `End (Nb.decode d);
  Unix.close rd

let incomplete_hdr_manual ctx =
  let d = Nb.decoder @@ `Manual buf3 in
  assert_equal ~printer (`Await (0,3)) (Nb.decode d);
  Nb.Manual.src d buf3 3;
  assert_equal ~msg:"first await" ~printer (`Await (0,0)) (Nb.decode d);
  assert_equal ~msg:"second await" ~printer (`Await (0,0)) (Nb.decode d)

let invalid_hdr_manual ctx =
  let d = Nb.decoder @@ `Manual bad_hdr in
  Nb.Manual.src d bad_hdr H.size;
  assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (Nb.decode d);
  assert_equal ~printer (`Await (0,56)) (Nb.decode d);
  Nb.Manual.src d bad_hdr H.size;
  assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (Nb.decode d);
  assert_equal ~printer (`Await (0,56)) (Nb.decode d)

let incomplete_r_nb ctx =
  let d = Nb.decoder @@ `Manual h_plus_0_5b in
  assert_equal ~msg:"before_feed" ~printer (`Await (0,57)) (Nb.decode d);
  Nb.Manual.src d h_plus_0_5b 57;
  assert_equal ~msg:"after_feed" ~printer (`Await (0,56)) (Nb.decode d)

let complete_2b_manual ctx =
  let d = Nb.decoder @@ `Manual h_plus_2b in
  Nb.Manual.src d h_plus_2b (Bigstring.length h_plus_2b);
  assert_equal ~msg:"first" ~printer ~cmp:cmp_kind (`R R.empty) (Nb.decode d);
  assert_equal ~msg:"second" ~printer ~cmp:cmp_kind (`R R.empty) (Nb.decode d)

let printer = function
  | `Ok -> "Ok"
  | `Flush -> "Flush"

let decode_recode_2b_manual ctx =
  let b = Bigstring.create (2 * R.size) in
  let d = Nb.decoder @@ `Manual h_plus_2b in
  Nb.Manual.src d h_plus_2b (Bigstring.length h_plus_2b);
  let r1 = match Nb.decode d with `R r -> r | _ -> assert false in
  let r2 = match Nb.decode d with `R r -> r | _ -> assert false in
  let e = Nb.encoder @@ `Manual b in
  Nb.Manual.dst e b 0 (Bigstring.length b);
  assert_equal ~msg:"encode r1" ~printer `Ok (Nb.encode e @@ `R r1);
  assert_equal ~msg:"encode r2" ~printer `Ok (Nb.encode e @@ `R r2);
  assert_equal ~msg:"encode End" ~printer `Flush (Nb.encode e `End);
  assert_equal ~msg:"flush" ~printer `Ok (Nb.encode e `Flushed)

let suite =
  "decode" >:::

  [
    "decode_recode" >:: decode_recode;
    "empty" >:: empty;
    "empty_nb_manual" >:: empty_nb_manual;

    "incomplete_hdr" >:: incomplete_hdr;
    "incomplete_hdr_fd" >:: incomplete_hdr_fd;
    "invalid_hdr" >:: invalid_hdr;
    "invalid_hdr_fd" >:: invalid_hdr_fd;
    "valid_hdr" >:: valid_hdr;
    "valid_hdr_fd" >:: valid_hdr_fd;
    "incomplete_r" >:: incomplete_r;

    "incomplete_hdr_fd_nb" >:: incomplete_hdr_fd_nb;
    "invalid_hdr_fd_nb" >:: invalid_hdr_fd_nb;
    "incomplete_hdr_manual" >:: incomplete_hdr_manual;
    "invalid_hdr_manual" >:: invalid_hdr_manual;
    "incomplete_r_nb" >:: incomplete_r_nb;
    "complete_2b_manual" >:: complete_2b_manual;

    "decode_recode_2b_manual" >:: decode_recode_2b_manual;
  ]

let () = run_test_tt_main suite

