open OUnit2
open Scid

let random_r = Bytes.create R.size

let decode_recode ctx =
  let r = Bytes.create R.size in
  R.(write (read random_r 0) r 0);
  assert_equal ~printer:(Printf.sprintf "%S") random_r r

type ret =
  [ `Await of int * int
  | `End
  | `Error of
      [ `Bytes_unparsed of Bytes.t
      | `Header_invalid ]
  | `Yield of R.t ]

let buf0 = Bytes.create 0
let buf3 = Bytes.create 3
let bad_hdr = Bytes.init H.size (fun _ -> '\000')
let good_hdr =
  let b = Bytes.create H.size in
  H.write b 0; b

let h_plus_0_5b, h_plus_1b, h_plus_1_5b, h_plus_2b  =
  let a = Bytes.create (H.size + 1) in
  let b = Bytes.create (H.size + R.size) in
  let c = Bytes.create (H.size + R.size + 1) in
  let d = Bytes.create (H.size + 2 * R.size) in
  H.(write a 0; write b 0; write c 0; write d 0);
  a, b, c, d

let h_plus_incompleteb = Bytes.sub h_plus_1b 0 90

let printer = function
  | `End -> "End"
  | `Yield r -> "Yield"
  | `Error `Header_invalid -> "Header_invalid"
  | `Error (`Bytes_unparsed hdr) -> Printf.sprintf "Bytes_unparsed %S" (Bytes.to_string hdr)
  | `Await -> "Await"

let cmp_kind a b = match a, b with
  | `End, `End -> true
  | `Yield _, `Yield _ -> true
  | `Error `Header_invalid, `Error `Header_invalid -> true
  | `Error (`Bytes_unparsed _), `Error (`Bytes_unparsed _) -> true
  | `Await, `Await -> true
  | _ -> false

let empty ctx =
  let d = D.make @@ `Bytes buf0 in
  assert_equal ~printer `End (D.decode d)

let empty_manual ctx =
  let d = D.make @@ `Manual in
  assert_equal ~msg:"before_feed" ~printer `Await (D.decode d);
  D.Manual.src d buf0 0 0;
  assert_equal ~msg:"after_feed" ~printer `End (D.decode d)

let incomplete_hdr ctx =
  let d = D.make @@ `Bytes buf3 in
  assert_equal ~printer (`Error `Header_invalid) (D.decode d);
  assert_equal ~printer `End (D.decode d)

let incomplete_hdr_ch ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write wr buf3 0 3 in
  Unix.close wr;
  assert_equal 3 nb_written;
  let d = D.make @@ `Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (`Error `Header_invalid) (D.decode d);
  assert_equal ~printer `End (D.decode d);
  Unix.close rd

let valid_hdr ctx =
  let d = D.make @@ `Bytes good_hdr in
  assert_equal ~printer `End (D.decode d)

let invalid_hdr ctx =
  let d = D.make @@ `Bytes bad_hdr in
  assert_equal ~printer (`Error `Header_invalid) (D.decode d);
  assert_equal ~printer `End (D.decode d)

let valid_hdr_fd ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write wr good_hdr 0 H.size in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = D.make @@ `Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer `End (D.decode d);
  Unix.close rd

let invalid_hdr_fd ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write wr bad_hdr 0 H.size in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = D.make @@ `Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (`Error `Header_invalid) (D.decode d);
  assert_equal ~printer `End (D.decode d);
  Unix.close rd

let incomplete_r ctx =
  let d = D.make @@ `Bytes h_plus_0_5b in
  assert_equal ~printer ~cmp:cmp_kind
    (`Error (`Bytes_unparsed buf3)) (D.decode d);
  assert_equal ~printer `End (D.decode d)

let incomplete_hdr_fd_nb ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write wr buf3 0 3 in
  Unix.close wr;
  assert_equal 3 nb_written;
  let d = D.make @@ `Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (`Error (`Bytes_unparsed buf3)) (D.decode d);
  assert_equal ~printer `End (D.decode d);
  Unix.close rd

let invalid_hdr_fd_nb ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write wr bad_hdr 0 H.size in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = D.make @@ `Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (`Error `Header_invalid) (D.decode d);
  assert_equal ~printer `End (D.decode d);
  Unix.close rd

let incomplete_hdr_manual ctx =
  let d = D.make @@ `Manual in
  assert_equal ~printer `Await (D.decode d);
  D.Manual.src d buf3 0 3;
  assert_equal ~msg:"first await" ~printer `Await (D.decode d);
  assert_equal ~msg:"second await" ~printer `Await (D.decode d)

let invalid_hdr_manual ctx =
  let d = D.make @@ `Manual in
  D.Manual.src d bad_hdr 0 H.size;
  assert_equal ~printer (`Error `Header_invalid) (D.decode d);
  assert_equal ~printer `Await (D.decode d);
  D.Manual.src d bad_hdr 0 H.size;
  assert_equal ~printer (`Error `Header_invalid) (D.decode d);
  assert_equal ~printer `Await (D.decode d)

let incomplete_r_nb ctx =
  let d = D.make @@ `Manual in
  assert_equal ~msg:"before_feed" ~printer `Await (D.decode d);
  D.Manual.src d h_plus_0_5b 0 57;
  assert_equal ~msg:"after_feed" ~printer `Await (D.decode d)

let complete_2b_manual ctx =
  let d = D.make @@ `Manual in
  D.Manual.src d h_plus_2b 0 (Bytes.length h_plus_2b);
  assert_equal ~msg:"first" ~printer ~cmp:cmp_kind (`Yield R.empty) (D.decode d);
  assert_equal ~msg:"second" ~printer ~cmp:cmp_kind (`Yield R.empty) (D.decode d)

let printer = function
  | `Ok -> "Ok"
  | `Partial -> "Partial"

let decode_recode_2b_manual ctx =
  let b = Bytes.create (2 * R.size) in
  let d = D.make @@ `Manual in
  D.Manual.src d h_plus_2b 0 (Bytes.length h_plus_2b);
  let r1 = match D.decode d with `Yield r -> r | _ -> assert false in
  let r2 = match D.decode d with `Yield r -> r | _ -> assert false in
  let e = E.make @@ `Manual in
  E.Manual.dst e b 0 (Bytes.length b);
  assert_equal ~msg:"encode r1" ~printer `Ok (E.encode e @@ `Yield r1);
  assert_equal ~msg:"encode r2" ~printer `Ok (E.encode e @@ `Yield r2);
  assert_equal ~msg:"encode End" ~printer `Partial (E.encode e `End);
  assert_equal ~msg:"flush" ~printer `Ok (E.encode e `Await)

let suite =
  "decode" >:::

  [
    "decode_recode" >:: decode_recode;
    "empty" >:: empty;
    "empty_manual" >:: empty_manual;

    "incomplete_hdr" >:: incomplete_hdr;
    "incomplete_hdr_ch" >:: incomplete_hdr_ch;
    (* "invalid_hdr" >:: invalid_hdr; *)
    (* "invalid_hdr_fd" >:: invalid_hdr_fd; *)
    (* "valid_hdr" >:: valid_hdr; *)
    (* "valid_hdr_fd" >:: valid_hdr_fd; *)
    (* "incomplete_r" >:: incomplete_r; *)

    (* "incomplete_hdr_fd_nb" >:: incomplete_hdr_fd_nb; *)
    (* "invalid_hdr_fd_nb" >:: invalid_hdr_fd_nb; *)
    (* "incomplete_hdr_manual" >:: incomplete_hdr_manual; *)
    (* "invalid_hdr_manual" >:: invalid_hdr_manual; *)
    (* "incomplete_r_nb" >:: incomplete_r_nb; *)
    (* "complete_2b_manual" >:: complete_2b_manual; *)

    (* "decode_recode_2b_manual" >:: decode_recode_2b_manual; *)
  ]

let () = run_test_tt_main suite

