open OUnit2
open Scid

let io_buffer_size = 4096
let max_uint32 = Int64.(pred (shift_left 1L 32))

let random_r () =
  let open Random in
  R.{
    datetime = float max_float;
    o = float max_float;
    h = float max_float;
    l = float max_float;
    c = float max_float;
    num_trades = int64 max_uint32;
    total_volume = int64 max_uint32;
    bid_volume = int64 max_uint32;
    ask_volume = int64 max_uint32;
  }

let fill_random_r ?len b pos =
  let len = match len with | Some l -> l | None -> Bytes.length b - pos in
  if pos < 0 || len < 0 || pos + len > Bytes.length b then invalid_arg "bounds"
  else
    let nb_rec = len / R.size in
    for i = 0 to nb_rec - 1 do
      let r = random_r () in
      R.write r b (pos + i * R.size);
    done;
    if len mod R.size > 0 then
      let b' = Bytes.make R.size '\000' in
      let r = random_r () in
      R.write r b' 0;
      Bytes.blit b' 0 b (pos + nb_rec * R.size) (len mod R.size)

let random_char _ = Random.bits () mod 256 |> Char.chr
let r = String.init R.size random_char

type ret =
  [ `Await
  | `End
  | `Error of
      [ `Eof of String.t
      | `Header_invalid of String.t ]
  | `R of R.t ]

let buf0 = Bytes.(init 0 random_char |> unsafe_to_string)
let buf3 = Bytes.(init 3 random_char |> unsafe_to_string)

let bad_hdr =
  Bytes.init H.size (fun i -> if i = 0 then '\000' else random_char i) |>
  Bytes.unsafe_to_string

let good_hdr =
  let b = Bytes.make H.size '\000' in
  H.write b 0;
  Bytes.unsafe_to_string b

let h_plus_0_5b, h_plus_1b, h_plus_1_5b, h_plus_2b  =
  let a = Bytes.init (H.size + 1) random_char in
  let b = Bytes.init (H.size + R.size) random_char in
  let c = Bytes.init (H.size + R.size + 1) random_char in
  let d = Bytes.init (H.size + 2 * R.size) random_char in
  H.(write a 0; write b 0; write c 0; write d 0);
  Bytes.(unsafe_to_string a,
         unsafe_to_string b,
         unsafe_to_string c,
         unsafe_to_string d)

let h_plus_incompleteb = String.sub h_plus_1b 0 90

let printer = function
| D.End -> "End"
| R _ -> "R"
| Error (Header_invalid hdr)-> Printf.sprintf "Header_invalid %S" hdr
| Error (Eof s) -> Printf.sprintf "Eof %S" s
| Await -> "Await"

let cmp a b = match a, b with
| D.End, D.End -> true
| R _, R _ -> true
| Error Header_invalid _, Error Header_invalid _ -> true
| Error (Eof _), Error (Eof _) -> true
| Await, Await -> true
| _ -> false

let decode_recode _ctx =
  let open EndianBytes.LittleEndian in
  let r' = Bytes.make R.size '\000' in
  for i = 0 to 1000 do
    let r = Bytes.init R.size random_char in
    set_int64 r 0 @@ Int64.bits_of_float (Random.float max_float);
    set_int32 r 8 @@ Int32.bits_of_float (Random.float max_float);
    set_int32 r 12 @@ Int32.bits_of_float (Random.float max_float);
    set_int32 r 16 @@ Int32.bits_of_float (Random.float max_float);
    set_int32 r 20 @@ Int32.bits_of_float (Random.float max_float);
    let r'' = R.read_bytes r 0 in
    assert_equal ~msg:"datetime" (get_int64 r 0 |> Int64.float_of_bits) r''.R.datetime;
    assert_equal ~msg:"o" (get_int32 r 8 |> Int32.float_of_bits) r''.R.o;
    assert_equal ~msg:"h" (get_int32 r 12 |> Int32.float_of_bits) r''.R.h;
    assert_equal ~msg:"l" (get_int32 r 16 |> Int32.float_of_bits) r''.R.l;
    assert_equal ~msg:"c" (get_int32 r 20 |> Int32.float_of_bits) r''.R.c;
    R.(write r'' r' 0);
    assert_equal ~msg:(string_of_int i) r r'
  done

let decode_recode2 _ctx =
  let r = random_r () in
  let b = Bytes.make R.size '\000' in
  R.write r b 0;
  let r' = R.read_bytes b 0 in
  assert_equal ~cmp:(fun a b -> R.compare a b = 0) r r'

let chk_hdr _ctx =
  let printer = function
  | Result.Ok () -> "Ok"
  | Result.Error s -> Printf.sprintf "Error: %s" s in
  assert_equal ~printer ~msg:"good_hdr" (Result.Ok ()) (H.check good_hdr 0);
  assert_equal ~printer ~msg:"bad_hdr"
    (Error (Printf.sprintf "Char at pos 0 should not be %C" '\000'))
    (H.check bad_hdr 0)

let empty _ctx =
  let d = D.of_string buf0 in
  assert_equal ~printer End (D.decode d)

let empty_manual _ctx =
  let d = D.make @@ Manual in
  assert_equal ~msg:"before_feed" ~printer Await (D.decode d);
  D.Manual.refill_string d buf0 0 0;
  assert_equal ~msg:"after_feed" ~printer End (D.decode d)

let incomplete_hdr _ctx =
  let d = D.of_string buf3 in
  assert_equal ~printer (Error (Eof buf3)) (D.decode d);
  assert_equal ~printer End (D.decode d)

let incomplete_hdr_ch _ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write_substring wr buf3 0 3 in
  Unix.close wr;
  assert_equal 3 nb_written;
  let d = D.make @@ Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (Error (Eof buf3)) (D.decode d);
  assert_equal ~printer End (D.decode d);
  Unix.close rd

let valid_hdr _ctx =
  let d = D.of_string good_hdr in
  assert_equal ~printer End (D.decode d)

let valid_hdr_ch _ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write_substring wr good_hdr 0 H.size in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = D.make @@ Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer End (D.decode d);
  Unix.close rd

let invalid_hdr _ctx =
  let d = D.of_string bad_hdr in
  assert_equal ~printer (Error (Header_invalid bad_hdr)) (D.decode d);
  assert_equal ~printer End (D.decode d)

let invalid_hdr_ch _ctx =
  let rd, wr = Unix.pipe () in
  let nb_written = Unix.write_substring wr bad_hdr 0 H.size in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = D.make @@ Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (Error (Header_invalid bad_hdr)) (D.decode d);
  assert_equal ~printer End (D.decode d);
  Unix.close rd

let incomplete_r _ctx =
  let d = D.of_string h_plus_0_5b in
  assert_equal ~printer ~cmp (Error (Eof buf3)) (D.decode d);
  assert_equal ~printer End (D.decode d)

let incomplete_hdr_fd_nb _ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write_substring wr buf3 0 3 in
  Unix.close wr;
  assert_equal 3 nb_written;
  let d = D.make @@ Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (Error (Eof buf3)) (D.decode d);
  assert_equal ~printer End (D.decode d);
  Unix.close rd

let invalid_hdr_fd_nb _ctx =
  let (rd, wr) = Unix.pipe () in
  let nb_written = Unix.write_substring wr bad_hdr 0 H.size in
  Unix.close wr;
  assert_equal H.size nb_written;
  let d = D.make @@ Channel (Unix.in_channel_of_descr rd) in
  assert_equal ~printer (Error (Header_invalid bad_hdr)) (D.decode d);
  assert_equal ~printer End (D.decode d);
  Unix.close rd

let incomplete_hdr_manual _ctx =
  let d = D.make Manual in
  assert_equal ~printer Await (D.decode d);
  D.Manual.refill_string d buf3 0 3;
  assert_equal ~msg:"first await" ~printer Await (D.decode d);
  assert_equal ~msg:"second await" ~printer Await (D.decode d)

let invalid_hdr_manual _ctx =
  let d = D.make Manual in
  D.Manual.refill_string d bad_hdr 0 H.size;
  assert_equal ~msg:"first" ~printer (Error (Header_invalid bad_hdr)) (D.decode d);
  assert_equal ~printer Await (D.decode d)

let incomplete_r_nb _ctx =
  let d = D.make Manual in
  assert_equal ~msg:"before_feed" ~printer Await (D.decode d);
  D.Manual.refill_string d h_plus_0_5b 0 57;
  assert_equal ~msg:"after_feed" ~printer Await (D.decode d)

let complete_2b_manual _ctx =
  let d = D.make Manual in
  D.Manual.refill_string d h_plus_2b 0 (String.length h_plus_2b);
  let r1 = R.read h_plus_2b 56 in
  let r2 = R.read h_plus_2b (56+40) in
  assert_equal ~msg:"first" ~printer (R r1) (D.decode d);
  assert_equal ~msg:"second" ~printer (R r2) (D.decode d)


let decode_3pages _ctx =
  let i = Bytes.make (3*io_buffer_size) '\000' in
  Bytes.blit_string good_hdr 0 i 0 H.size;
  fill_random_r i H.size;
  let d = D.make @@ Bytes i in
  let nb_decoded = ref 0 in
  begin try
    while true do
      match D.decode d with
      | R r ->
        assert_equal
          ~printer:(fun r ->
              let b = Bytes.make R.size '\000' in
              R.write r b 0;
              Printf.sprintf "%S" (Bytes.unsafe_to_string b))
          ~msg:(Printf.sprintf "record %d" !nb_decoded)
          (R.read_bytes i (H.size + !nb_decoded * R.size)) r;
        incr nb_decoded
      | _ -> failwith "break"
    done
  with Failure _ -> () end;
  assert_equal ~msg:"nb_decoded" (((3 * io_buffer_size) - H.size) / R.size ) !nb_decoded

let printer = function
| `Ok -> "Ok"
| `Partial -> "Partial"

let encode_manual _ctx =
  let len = H.size + R.size in
  let b = Bytes.create 0 in
  let e = E.make Manual in
  let r = random_r () in
  let dst = Bytes.create len in
  let correct_dst =
    let b = Bytes.create len in
    Bytes.blit_string good_hdr 0 b 0 H.size;
    R.write r b H.size;
    Bytes.unsafe_to_string b
  in
  assert_equal ~msg:"before adding bytes" `Partial (E.encode e @@ R r);
  E.Manual.add_bytes e b 0 0;
  assert_equal ~msg:"after adding empty buf" `Partial (E.encode e Await);
  assert_equal ~msg:"after adding empty buf, second await" `Partial (E.encode e Await);
  E.Manual.add_bytes e dst 0 len;
  assert_equal ~msg:"first rem" (H.size + R.size) (E.Manual.rem e);
  assert_equal ~msg:"after adding real buf" `Ok (E.encode e Await);
  assert_equal ~msg:"second rem" 0 (E.Manual.rem e);
  assert_equal ~msg:"after encoding End" `Partial (E.encode e End);
  assert_equal ~msg:"after final Await" `Ok (E.encode e Await);
  assert_equal ~msg:"dsts" ~printer:(Printf.sprintf "%S") correct_dst (Bytes.to_string dst);
  assert_equal ~msg:"decoding = encoding" ~cmp:(fun r r' -> R.compare r r' = 0)
    r (R.read_bytes dst H.size)

let decode_smallbuf _ctx =
  let len = H.size + R.size in
  let r = random_r () in
  let r_bytes = Bytes.create len in
  Bytes.blit_string good_hdr 0 r_bytes 0 H.size;
  R.write r r_bytes H.size;
  let d = D.make Manual in
  let b = Bytes.create 1 in
  for i = 0 to len - 1 do
    Bytes.(set b 0 @@ get r_bytes i);
    D.Manual.refill_bytes d b 0 1;
    if i < len - 1
    then assert_equal ~msg:"intermediate decode" D.Await (D.decode d)
    else begin
      let r' = D.decode d |> function R r -> r | _ -> assert false in
      assert_equal ~msg:"final decode"
        ~cmp:(fun r r' -> R.compare r r' = 0)
        ~printer:(fun r ->
            let b = Bytes.create R.size in R.write r b 0;
            Bytes.unsafe_to_string b) r r'
    end
  done

let encode_smallbuf _ctx =
  let len = H.size + R.size in
  let r = random_r () in
  let r' = Bytes.create len in
  let b = Bytes.create 1 in
  let e = E.make Manual in
  assert_equal ~msg:"before adding bytes" `Partial (E.encode e @@ R r);
  for i = 0 to len - 1 do
    E.Manual.add_bytes e b 0 1;
    if i < len - 1
    then assert_equal ~msg:(Printf.sprintf "loop %d" i) `Partial (E.encode e Await)
    else assert_equal ~msg:(Printf.sprintf "loop %d" i) `Ok (E.encode e Await);
    Bytes.(set r' i @@ get b 0)
  done;
  assert_equal ~msg:"loop final" `Ok (E.encode e Await);
  assert_equal ~msg:"loop final" `Ok (E.encode e Await);
  assert_equal ~msg:"header" good_hdr (Bytes.sub_string r' 0 H.size);
  assert_equal ~cmp:(fun r r' -> R.compare r r' = 0) ~msg:"record" r (R.read_bytes r' H.size)

let decode_recode_2b_manual _ctx =
  let b = Bytes.make (String.length h_plus_2b) '\000' in
  let d = D.make Manual in
  D.Manual.refill_string d h_plus_2b 0 (String.length h_plus_2b);
  let r1 = match D.decode d with R r -> r | _ -> assert false in
  let r2 = match D.decode d with R r -> r | _ -> assert false in
  let e = E.make Manual in
  E.Manual.add_bytes e b 0 (Bytes.length b);
  assert_equal ~msg:"encode r1" ~printer `Ok (E.encode e @@ R r1);
  assert_equal R.size (E.Manual.rem e);
  assert_equal ~msg:"encode r2" ~printer `Ok (E.encode e @@ R r2);
  assert_equal 0 (E.Manual.rem e);
  assert_equal (Bytes.unsafe_to_string b) h_plus_2b;
  assert_equal ~msg:"encode End" ~printer `Partial (E.encode e End);
  assert_equal ~msg:"flush" ~printer `Ok (E.encode e Await);
  assert_equal ~msg:"flush2" ~printer `Ok (E.encode e Await)

let decode_recode_2b_smallbuf _ctx =
  let len = H.size + 2 * R.size in
  let b = Bytes.create 1 in
  let h_plus_2b' = Bytes.create len in
  let d = D.make Manual in
  D.Manual.refill_string d h_plus_2b 0 len;
  let r1 = match D.decode d with R r -> r | _ -> assert false in
  let r2 = match D.decode d with R r -> r | _ -> assert false in
  let e = E.make Manual in

  assert_equal ~msg:"encode r1" ~printer `Partial (E.encode e @@ R r1);
  for i = 0 to H.size + R.size - 1 do
    E.Manual.add_bytes e b 0 1;
    if i = H.size + R.size - 1 then
      assert_equal ~msg:"encode r1" ~printer `Ok (E.encode e Await)
    else
      assert_equal ~msg:"encode r1" ~printer `Partial (E.encode e Await);
    Bytes.(set h_plus_2b' i @@ get b 0);
  done;

  assert_equal ~msg:"compare IO1"
    (String.sub h_plus_2b 0 @@ H.size + R.size)
    (Bytes.sub_string h_plus_2b' 0 @@ H.size + R.size);

  assert_equal ~msg:"encode r2" ~printer `Partial (E.encode e @@ R r2);
  for i = H.size + R.size to H.size + 2 * R.size - 1 do
    E.Manual.add_bytes e b 0 1;
    if i = H.size + 2 * R.size - 1 then
      assert_equal ~msg:"encode r2" ~printer `Ok (E.encode e Await)
    else
      assert_equal ~msg:"encode r2" ~printer `Partial (E.encode e Await);
    Bytes.(set h_plus_2b' i @@ get b 0);
  done;

  assert_equal ~msg:"rem" 0 (E.Manual.rem e);
  assert_equal ~msg:"compare IO" h_plus_2b (Bytes.unsafe_to_string h_plus_2b');
  assert_equal ~msg:"encode End" ~printer `Partial (E.encode e End);
  assert_equal ~msg:"flush" ~printer `Ok (E.encode e Await);
  assert_equal ~msg:"flush2" ~printer `Ok (E.encode e Await)

let decode_encode_3pages _ctx =
  let src = Bytes.make (3 * io_buffer_size) '\000' in
  let dst = Buffer.create (3 * io_buffer_size) in
  Bytes.blit_string good_hdr 0 src 0 H.size;
  fill_random_r src H.size;
  let d = D.of_bytes src in
  let e = E.of_buffer dst in
  let nb_encoded = ref 0 in
  begin try
    while true do
      match D.decode d with
      | R r ->
        (E.encode e @@ R r |> function
          | `Ok -> incr nb_encoded
          | `Partial -> assert false)
      | End -> failwith "End"
      | Error (Header_invalid s) -> failwith ("Header_invalid " ^ s)
      | Error (Eof b) -> failwith ("EOF " ^ b)
      | Await -> assert false
    done;
  with Failure s ->
    assert_equal "EOF" (String.sub s 0 3);
    assert_equal ~msg:"encoding `End" `Ok (E.encode e End);
    assert_equal ~msg:"encoding `Await" `Ok (E.encode e Await);
    while E.encode e Await <> `Ok do () done
  end;
  let dst = Buffer.contents dst in
  assert_equal ~msg:"nb_encoded" ((3 * io_buffer_size - H.size) / R.size) !nb_encoded;
  assert_equal ~printer:string_of_int
    ~msg:"buf size" (3 * io_buffer_size - ((3 * io_buffer_size - H.size) mod R.size)) (String.length dst);
  assert_equal ~printer:(Printf.sprintf "%S")
    Bytes.(sub src 0 (3 * io_buffer_size - 32) |> unsafe_to_string) dst

let decode_encode_3pages_manual _ctx =
  let src = Bytes.create (3 * io_buffer_size) in
  let dst = Bytes.create io_buffer_size in
  let buf = Buffer.create (3 * io_buffer_size) in
  let nb_records = (3 * io_buffer_size - H.size) / R.size in
  Bytes.blit_string good_hdr 0 src 0 H.size;
  fill_random_r src H.size;
  let d = D.make Manual in
  let e = E.make Manual in
  let nb_decoded = ref 0 in
  let nb_encoded = ref 0 in
  let refill_count = ref 0 in
  E.Manual.add_bytes e dst 0 io_buffer_size;
  begin try
    while true do
      match D.decode d with
      | R r ->
        incr nb_decoded;
        (E.encode e @@ R r |> function
          | `Ok -> incr nb_encoded
          | `Partial ->
            Buffer.add_subbytes buf dst 0 (io_buffer_size - E.Manual.rem e);
            E.Manual.add_bytes e dst 0 io_buffer_size;
            while E.encode e Await <> `Ok do () done;
            incr nb_encoded
        )
      | End -> failwith "End"
      | Error (Header_invalid s) -> failwith ("Header_invalid " ^ s)
      | Error (Eof b) -> failwith ("EOF " ^ b)
      | Await ->
        if !refill_count < 3 then begin
          D.Manual.refill_bytes d src (!refill_count*io_buffer_size) io_buffer_size;
          incr refill_count
        end else D.Manual.refill_bytes d src 0 0 (* Refill with an empty buf to signal EOF *)
    done;
  with Failure s ->
    assert_equal "EOF" (String.sub s 0 3);
    assert_equal ~msg:"encoding `End" `Partial (E.encode e End);
    assert_equal ~msg:"encoding `Await" `Ok (E.encode e Await);
    while E.encode e Await <> `Ok do () done;
  end;
  assert_equal ~printer:string_of_int ~msg:"nb_decoded" nb_records !nb_decoded;
  assert_equal ~printer:string_of_int ~msg:"nb_encoded" nb_records !nb_encoded;
  assert_equal ~printer:string_of_int ~msg:"encode buffer not used" 32 (E.Manual.rem e);
  let first_diff b b' =
    let ret = ref None in
    let len = Bytes.(min (length b) (length b')) in
    (try
      for i = 0 to len - 1 do
        if Bytes.get b i <> Bytes.get b' i
        then (ret := Some (i, Bytes.get b i, Bytes.get b' i) ; raise Exit)
      done
    with Exit -> ());
    !ret
  in
  let printer = function
  | None -> "equal"
  | Some (i, c, c') -> Printf.sprintf "pos %d, %C, %C" i c c' in
  assert_equal ~msg:"buffer equality" ~printer None
    (first_diff src (Buffer.contents buf |> Bytes.unsafe_of_string))

let suite =
  "scid" >:::

  [
    "decode_recode" >:: decode_recode;
    "decode_recode2" >:: decode_recode2;
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
    "decode_smallbuf" >:: decode_smallbuf;
    "decode_3pages" >:: decode_3pages;

    "encode_smallbuf" >:: encode_smallbuf;
    "encode_manual" >:: encode_manual;
    "decode_recode_2b_manual" >:: decode_recode_2b_manual;
    "decode_recode_2b_smallbuf" >:: decode_recode_2b_smallbuf;
    "decode_encode_3pages" >:: decode_encode_3pages;
    "decode_encode_3pages_manual" >:: decode_encode_3pages_manual;
  ]

let () = run_test_tt_main suite

