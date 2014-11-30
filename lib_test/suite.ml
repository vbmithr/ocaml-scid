open OUnit2
open Scid

let r = String.make R.size '\001'

type ret =
  [ `Await
  | `End
  | `Error of
      [ `Eof of String.t
      | `Header_invalid of String.t ]
  | `R of R.t ]

let buf0 = String.make 0 '\000'
let buf3 = String.make 3 '\000'
let bad_hdr = String.init H.size (fun _ -> '\000')
let good_hdr =
  let b = String.make H.size '\000' in
  H.write b 0; b

let h_plus_0_5b, h_plus_1b, h_plus_1_5b, h_plus_2b  =
  let a = String.make (H.size + 1) '\000' in
  let b = String.make (H.size + R.size) '\000' in
  let c = String.make (H.size + R.size + 1) '\000' in
  let d = String.make (H.size + 2 * R.size) '\000' in
  H.(write a 0; write b 0; write c 0; write d 0);
  a, b, c, d

let h_plus_incompleteb = String.sub h_plus_1b 0 90

let printer = function
  | `End -> "End"
  | `R r -> "Yield"
  | `Error (`Header_invalid hdr)-> Printf.sprintf "Header_invalid %S" hdr
  | `Error (`Eof s) -> Printf.sprintf "Eof %S" s
  | `Await -> "Await"

let cmp a b = match a, b with
  | `End, `End -> true
  | `R _, `R _ -> true
  | `Error `Header_invalid _, `Error `Header_invalid _ -> true
  | `Error (`Eof _), `Error (`Eof _) -> true
  | `Await, `Await -> true
  | _ -> false

let decode_recode ctx =
  let r' = String.make R.size '\000' in
  R.(write (read r 0) r' 0);
  assert_equal ~printer:(Printf.sprintf "%S") r r'

let chk_hdr ctx =
  let printer = function
    | `Ok -> "Ok"
    | `Error s -> Printf.sprintf "Error: %s" s in
  assert_equal ~printer ~msg:"good_hdr" `Ok (H.check good_hdr 0);
  assert_equal ~printer ~msg:"bad_hdr"
    (`Error (Printf.sprintf "Char at pos 0 should not be %C" '\000'))
    (H.check bad_hdr 0)

let empty ctx =
  let d = D.make @@ `String buf0 in
  assert_equal ~printer `End (D.decode d)

let empty_manual ctx =
  let d = D.make @@ `Manual in
  assert_equal ~msg:"before_feed" ~printer `Await (D.decode d);
  D.Manual.refill_string d buf0 0 0;
  assert_equal ~msg:"after_feed" ~printer `End (D.decode d)

let incomplete_hdr ctx =
  let d = D.make @@ `String buf3 in
  assert_equal ~printer (`Error (`Eof buf3)) (D.decode d);
  assert_equal ~printer `End (D.decode d)

let incomplete_hdr_ch ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write wr buf3 0 3 in
  Unix.close wr;
  assert_equal 3 nb_written;
  let d = D.make @@ `Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (`Error (`Eof buf3)) (D.decode d);
  assert_equal ~printer `End (D.decode d);
  Unix.close rd

let valid_hdr ctx =
  let d = D.make @@ `String good_hdr in
  assert_equal ~printer `End (D.decode d)

let valid_hdr_ch ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write wr good_hdr 0 H.size in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = D.make @@ `Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer `End (D.decode d);
  Unix.close rd

let invalid_hdr ctx =
  let d = D.make @@ `String bad_hdr in
  assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (D.decode d);
  assert_equal ~printer `End (D.decode d)

let invalid_hdr_ch ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write wr bad_hdr 0 H.size in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = D.make @@ `Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (D.decode d);
  assert_equal ~printer `End (D.decode d);
  Unix.close rd

let incomplete_r ctx =
  let d = D.make @@ `String h_plus_0_5b in
  assert_equal ~printer ~cmp (`Error (`Eof buf3)) (D.decode d);
  assert_equal ~printer `End (D.decode d)

let incomplete_hdr_fd_nb ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write wr buf3 0 3 in
  Unix.close wr;
  assert_equal 3 nb_written;
  let d = D.make @@ `Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (`Error (`Eof buf3)) (D.decode d);
  assert_equal ~printer `End (D.decode d);
  Unix.close rd

let invalid_hdr_fd_nb ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write wr bad_hdr 0 H.size in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = D.make @@ `Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (D.decode d);
  assert_equal ~printer `End (D.decode d);
  Unix.close rd

let incomplete_hdr_manual ctx =
  let d = D.make `Manual in
  assert_equal ~printer `Await (D.decode d);
  D.Manual.refill_string d buf3 0 3;
  assert_equal ~msg:"first await" ~printer `Await (D.decode d);
  assert_equal ~msg:"second await" ~printer `Await (D.decode d)

let invalid_hdr_manual ctx =
  let d = D.make `Manual in
  D.Manual.refill_string d bad_hdr 0 H.size;
  assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (D.decode d);
  assert_equal ~printer `Await (D.decode d);
  D.Manual.refill_string d bad_hdr 0 H.size;
  assert_equal ~printer (`Error (`Header_invalid bad_hdr)) (D.decode d);
  assert_equal ~printer `Await (D.decode d)

let incomplete_r_nb ctx =
  let d = D.make `Manual in
  assert_equal ~msg:"before_feed" ~printer `Await (D.decode d);
  D.Manual.refill_string d h_plus_0_5b 0 57;
  assert_equal ~msg:"after_feed" ~printer `Await (D.decode d)

let complete_2b_manual ctx =
  let d = D.make `Manual in
  D.Manual.refill_string d h_plus_2b 0 (String.length h_plus_2b);
  assert_equal ~msg:"first" ~printer ~cmp (`R R.empty) (D.decode d);
  assert_equal ~msg:"second" ~printer ~cmp (`R R.empty) (D.decode d)

let decode_small_buf ctx =
  let d = D.make `Manual in
  let b = Bytes.create 1 in
  for i = 0 to H.size + R.size - 1 do
    Bytes.set b 0 H.valid.[i mod H.size];
    D.Manual.refill_bytes d b 0 1;
    if i < H.size + R.size - 1
    then assert_equal ~msg:"intermediate decode" `Await (D.decode d)
    else assert_equal ~msg:"last_decode" ~cmp (`R R.empty) (D.decode d)
  done

let printer = function
  | `Ok -> "Ok"
  | `Partial -> "Partial"

let encode_empty ctx =
  let b = Bytes.create 0 in
  let e = E.make `Manual in
  assert_equal ~msg:"before adding bytes" `Partial (E.encode e @@ `R R.empty);
  E.Manual.add_bytes e b 0 0;
  assert_equal ~msg:"after adding empty buf" `Partial (E.encode e `Await);
  assert_equal ~msg:"after adding empty buf, second await" `Partial (E.encode e `Await);
  E.Manual.add_string e h_plus_2b 0 (String.length h_plus_2b);
  assert_equal ~msg:"first rem" (String.length h_plus_2b) (E.Manual.rem e);
  assert_equal ~msg:"after adding real buf" `Ok (E.encode e `Await);
  assert_equal R.size (E.Manual.rem e);
  assert_equal ~msg:"after encoding End" `Partial (E.encode e `End);
  assert_equal ~msg:"after final Await" `Ok (E.encode e `Await);
  assert_equal ~msg:"second rem" (String.length h_plus_2b) (E.Manual.rem e)

let encode_small_buf ctx =
  let b = Bytes.create 1 in
  let e = E.make `Manual in
  assert_equal ~msg:"before adding bytes" `Partial (E.encode e @@ `R R.empty);
  for i = 0 to H.size + R.size - 1 do
    E.Manual.add_bytes e b 0 1;
    if i < H.size + R.size - 1
    then assert_equal ~msg:(Printf.sprintf "loop %d" i) `Partial (E.encode e `Await)
    else assert_equal ~msg:(Printf.sprintf "loop %d" i) `Ok (E.encode e `Await)
  done;
  assert_equal ~msg:"loop final" `Ok (E.encode e `Await);
  assert_equal ~msg:"loop final" `Ok (E.encode e `Await)

let decode_recode_2b_manual ctx =
  let b = String.make (String.length h_plus_2b) '\000' in
  let d = D.make `Manual in
  D.Manual.refill_string d h_plus_2b 0 (String.length h_plus_2b);
  let r1 = match D.decode d with `R r -> r | _ -> assert false in
  let r2 = match D.decode d with `R r -> r | _ -> assert false in
  let e = E.make `Manual in
  E.Manual.add_string e b 0 (String.length b);
  assert_equal ~msg:"encode r1" ~printer `Ok (E.encode e @@ `R r1);
  assert_equal R.size (E.Manual.rem e);
  assert_equal ~msg:"encode r2" ~printer `Ok (E.encode e @@ `R r2);
  assert_equal 0 (E.Manual.rem e);
  assert_equal b h_plus_2b;
  assert_equal ~msg:"encode End" ~printer `Partial (E.encode e `End);
  assert_equal ~msg:"flush" ~printer `Ok (E.encode e `Await);
  assert_equal ~msg:"flush2" ~printer `Ok (E.encode e `Await)

let suite =
  "scid" >:::

  [
    "decode_recode" >:: decode_recode;
    "chk_hdr" >:: chk_hdr;
    "empty" >:: empty;
    "empty_manual" >:: empty_manual;

    "incomplete_hdr" >:: incomplete_hdr;
    "incomplete_hdr_ch" >:: incomplete_hdr_ch;
    "invalid_hdr" >:: invalid_hdr;
    "invalid_hdr_ch" >:: invalid_hdr_ch;
    "valid_hdr" >:: valid_hdr;
    "valid_hdr_ch" >:: valid_hdr_ch;
    "incomplete_r" >:: incomplete_r;

    "incomplete_hdr_fd_nb" >:: incomplete_hdr_fd_nb;
    "invalid_hdr_fd_nb" >:: invalid_hdr_fd_nb;
    "incomplete_hdr_manual" >:: incomplete_hdr_manual;
    "invalid_hdr_manual" >:: invalid_hdr_manual;
    "incomplete_r_nb" >:: incomplete_r_nb;
    "complete_2b_manual" >:: complete_2b_manual;
    "decode_small_buf" >:: decode_small_buf;

    "encode_small_buf" >:: encode_small_buf;
    "encode_empty" >:: encode_empty;
    "decode_recode_2b_manual" >:: decode_recode_2b_manual;
  ]

let () = run_test_tt_main suite

