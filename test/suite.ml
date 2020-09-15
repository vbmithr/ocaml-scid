open Scid

let io_buffer_size = 4096
let max_uint32 = Int64.(pred (shift_left 1L 32))

let random_r () =
  let open Random in
  R.
    { datetime= float max_float;
      o= float max_float;
      h= float max_float;
      l= float max_float;
      c= float max_float;
      num_trades= int64 max_uint32;
      total_volume= int64 max_uint32;
      bid_volume= int64 max_uint32;
      ask_volume= int64 max_uint32 }

let fill_random_r ?len b pos =
  let len = match len with Some l -> l | None -> Bytes.length b - pos in
  if pos < 0 || len < 0 || pos + len > Bytes.length b then invalid_arg "bounds"
  else
    let nb_rec = len / R.size in
    for i = 0 to nb_rec - 1 do
      let r = random_r () in
      R.write r b (pos + (i * R.size))
    done ;
    if len mod R.size > 0 then (
      let b' = Bytes.make R.size '\000' in
      let r = random_r () in
      R.write r b' 0 ;
      Bytes.blit b' 0 b (pos + (nb_rec * R.size)) (len mod R.size) )

let random_char _ = Random.bits () mod 256 |> Char.chr
let r = String.init R.size random_char

type ret =
  [ `Await
  | `End
  | `Error of [`Eof of String.t | `Header_invalid of String.t]
  | `R of R.t ]

let buf0 = Bytes.(init 0 random_char |> unsafe_to_string)
let buf3 = Bytes.(init 3 random_char |> unsafe_to_string)

let bad_hdr =
  Bytes.init H.size (fun i -> if i = 0 then '\000' else random_char i)
  |> Bytes.unsafe_to_string

let good_hdr =
  let b = Bytes.make H.size '\000' in
  H.write b 0 ; Bytes.unsafe_to_string b

let h_plus_0_5b, h_plus_1b, h_plus_1_5b, h_plus_2b =
  let a = Bytes.init (H.size + 1) random_char in
  let b = Bytes.init (H.size + R.size) random_char in
  let c = Bytes.init (H.size + R.size + 1) random_char in
  let d = Bytes.init (H.size + (2 * R.size)) random_char in
  H.(write a 0 ; write b 0 ; write c 0 ; write d 0) ;
  Bytes.
    ( unsafe_to_string a,
      unsafe_to_string b,
      unsafe_to_string c,
      unsafe_to_string d )

let h_plus_incompleteb = String.sub h_plus_1b 0 90
let float_eps = float Float.epsilon

let decode_recode () =
  let open EndianBytes.LittleEndian in
  let r' = Bytes.make R.size '\000' in
  for i = 0 to 1000 do
    let r = Bytes.init R.size random_char in
    set_int64 r 0 @@ Int64.bits_of_float (Random.float 1.0) ;
    set_int32 r 8 @@ Int32.bits_of_float (Random.float 1.0) ;
    set_int32 r 12 @@ Int32.bits_of_float (Random.float 1.0) ;
    set_int32 r 16 @@ Int32.bits_of_float (Random.float 1.0) ;
    set_int32 r 20 @@ Int32.bits_of_float (Random.float 1.0) ;
    let r'' = R.read_bytes r 0 in
    check float_eps "datetime"
      (get_int64 r 0 |> Int64.float_of_bits)
      r''.R.datetime ;
    check float_eps "o" (get_int32 r 8 |> Int32.float_of_bits) r''.R.o ;
    check float_eps "h" (get_int32 r 12 |> Int32.float_of_bits) r''.R.h ;
    check float_eps "l" (get_int32 r 16 |> Int32.float_of_bits) r''.R.l ;
    check float_eps "c" (get_int32 r 20 |> Int32.float_of_bits) r''.R.c ;
    R.(write r'' r' 0) ;
    check string (string_of_int i) (Bytes.unsafe_to_string r)
      (Bytes.unsafe_to_string r')
  done

let r_testable = (module R : TESTABLE with type t = R.t)

let decode_recode2 () =
  let r = random_r () in
  let b = Bytes.make R.size '\000' in
  R.write r b 0 ;
  let r' = R.read_bytes b 0 in
  check r_testable "recode2" r r'

let chk_hdr () =
  check (result unit string) "good_hdr" (Ok ()) (H.check good_hdr 0) ;
  check (result unit string) "bad_hdr"
    (Error (Printf.sprintf "Char at pos 0 should not be %C" '\000'))
    (H.check bad_hdr 0)

module Decode_result_testable = struct
  type t = D.decode_result = R of R.t | Await | End | Error of D.error

  let equal a b =
    match (a, b) with
    | R a, R b -> R.equal a b
    | Await, Await | End, End -> true
    | Error (Header_invalid _a), Error (Header_invalid _b) -> true
    | Error (Eof _a), Error (Eof _b) -> true
    | _ -> false

  let pp ppf _ = Format.pp_print_string ppf "<D.decode_result>"
end

let decode_result =
  (module Decode_result_testable : TESTABLE with type t = D.decode_result)

let empty () =
  let d = D.of_string buf0 in
  check decode_result "" D.End (D.decode d)

let empty_manual () =
  let d = D.make @@ Manual in
  check decode_result "" D.Await (D.decode d) ;
  D.Manual.refill_string d buf0 0 0 ;
  check decode_result "" D.End (D.decode d)

let incomplete_hdr () =
  let d = D.of_string buf3 in
  check decode_result "" D.(Error (Eof buf3)) D.(decode d) ;
  check decode_result "" D.End D.(decode d)

let incomplete_hdr_ch () =
  let rd, wr = Unix.pipe () in
  let nb_written = Unix.write_substring wr buf3 0 3 in
  Unix.close wr ;
  check int "nb_written" 3 nb_written ;
  let d = D.make @@ Channel (Unix.in_channel_of_descr rd) in
  check decode_result "" D.(Error (Eof buf3)) D.(decode d) ;
  check decode_result "" D.End D.(decode d) ;
  Unix.close rd

let valid_hdr () =
  let d = D.of_string good_hdr in
  check decode_result "" D.End (D.decode d)

let valid_hdr_ch () =
  let rd, wr = Unix.pipe () in
  let nb_written = Unix.write_substring wr good_hdr 0 H.size in
  Unix.close wr ;
  check int "nb_written" H.size nb_written ;
  let d = D.make @@ Channel (Unix.in_channel_of_descr rd) in
  check decode_result "" D.End D.(decode d) ;
  Unix.close rd

let invalid_hdr () =
  let d = D.of_string bad_hdr in
  check decode_result "" D.(Error (Header_invalid bad_hdr)) D.(decode d) ;
  check decode_result "" D.End D.(decode d)

let invalid_hdr_ch () =
  let rd, wr = Unix.pipe () in
  let nb_written = Unix.write_substring wr bad_hdr 0 H.size in
  Unix.close wr ;
  check int "nb_written" H.size nb_written ;
  let d = D.make @@ Channel (Unix.in_channel_of_descr rd) in
  check decode_result "" D.(Error (Header_invalid bad_hdr)) D.(decode d) ;
  check decode_result "" D.End D.(decode d) ;
  Unix.close rd

let incomplete_r () =
  let d = D.of_string h_plus_0_5b in
  check decode_result "incomp_error" D.(Error (Eof buf3)) D.(decode d) ;
  check decode_result "incomp_end" D.End D.(decode d)

let incomplete_hdr_fd_nb () =
  let rd, wr = Unix.pipe () in
  let nb_written = Unix.write_substring wr buf3 0 3 in
  Unix.close wr ;
  check int "nb_written" 3 nb_written ;
  let d = D.make @@ Channel (Unix.in_channel_of_descr rd) in
  check decode_result "" D.(Error (Eof buf3)) D.(decode d) ;
  check decode_result "" D.End D.(decode d) ;
  Unix.close rd

let invalid_hdr_fd_nb () =
  let rd, wr = Unix.pipe () in
  let nb_written = Unix.write_substring wr bad_hdr 0 H.size in
  Unix.close wr ;
  check int "nb_written" H.size nb_written ;
  let d = D.make @@ Channel (Unix.in_channel_of_descr rd) in
  check decode_result "" D.(Error (Header_invalid bad_hdr)) D.(decode d) ;
  check decode_result "" D.End D.(decode d) ;
  Unix.close rd

let incomplete_hdr_manual () =
  let d = D.make Manual in
  check decode_result "" D.(Await) D.(decode d) ;
  D.Manual.refill_string d buf3 0 3 ;
  check decode_result "" D.(Await) D.(decode d) ;
  check decode_result "" D.Await D.(decode d)

let invalid_hdr_manual () =
  let d = D.make Manual in
  D.Manual.refill_string d bad_hdr 0 H.size ;
  check decode_result "" D.(Error (Header_invalid bad_hdr)) D.(decode d) ;
  check decode_result "" D.Await D.(decode d)

let incomplete_r_nb () =
  let d = D.make Manual in
  check decode_result "" D.Await D.(decode d) ;
  D.Manual.refill_string d h_plus_0_5b 0 57 ;
  check decode_result "" D.Await D.(decode d)

let complete_2b_manual () =
  let d = D.make Manual in
  D.Manual.refill_string d h_plus_2b 0 (String.length h_plus_2b) ;
  let r1 = R.read h_plus_2b 56 in
  let r2 = R.read h_plus_2b (56 + 40) in
  check decode_result "" D.(R r1) (D.decode d) ;
  check decode_result "" D.(R r2) (D.decode d)

let decode_3pages () =
  let i = Bytes.make (3 * io_buffer_size) '\000' in
  Bytes.blit_string good_hdr 0 i 0 H.size ;
  fill_random_r i H.size ;
  let d = D.make @@ Bytes i in
  let nb_decoded = ref 0 in
  ( try
      while true do
        match D.decode d with
        | R r ->
            check r_testable ""
              (R.read_bytes i (H.size + (!nb_decoded * R.size)))
              r ;
            incr nb_decoded
        | _ -> failwith "break"
      done
    with Failure _ -> () ) ;
  check int "nb_decoded" (((3 * io_buffer_size) - H.size) / R.size) !nb_decoded

module Partial_testable = struct
  type t = E.encode_result

  let equal = Stdlib.( = )

  let pp ppf = function
    | `Ok -> Format.pp_print_string ppf "Ok"
    | `Partial -> Format.pp_print_string ppf "Partial"
end

let partial = (module Partial_testable : TESTABLE with type t = E.encode_result)

let encode_manual () =
  let len = H.size + R.size in
  let b = Bytes.create 0 in
  let e = E.make Manual in
  let r = random_r () in
  let dst = Bytes.create len in
  let correct_dst =
    let b = Bytes.create len in
    Bytes.blit_string good_hdr 0 b 0 H.size ;
    R.write r b H.size ;
    Bytes.unsafe_to_string b in
  check partial "before adding bytes" `Partial (E.encode e @@ R r) ;
  E.Manual.add_bytes e b 0 0 ;
  check partial "after adding empty buf" `Partial (E.encode e Await) ;
  check partial "after adding empty buf, second await" `Partial
    (E.encode e Await) ;
  E.Manual.add_bytes e dst 0 len ;
  check int "first rem" (H.size + R.size) (E.Manual.rem e) ;
  check partial "after adding real buf" `Ok (E.encode e Await) ;
  check int "second rem" 0 (E.Manual.rem e) ;
  check partial "after encoding End" `Partial (E.encode e End) ;
  check partial "after final Await" `Ok (E.encode e Await) ;
  check string "dsts" correct_dst (Bytes.to_string dst) ;
  check r_testable "decoding = encoding" r (R.read_bytes dst H.size)

let decode_smallbuf () =
  let len = H.size + R.size in
  let r = random_r () in
  let r_bytes = Bytes.create len in
  Bytes.blit_string good_hdr 0 r_bytes 0 H.size ;
  R.write r r_bytes H.size ;
  let d = D.make Manual in
  let b = Bytes.create 1 in
  for i = 0 to len - 1 do
    Bytes.(set b 0 @@ get r_bytes i) ;
    D.Manual.refill_bytes d b 0 1 ;
    if i < len - 1 then
      check decode_result "intermediate decode" D.Await (D.decode d)
    else
      let r' = D.decode d |> function R r -> r | _ -> assert false in
      check r_testable "final decode" r r'
  done

let encode_smallbuf () =
  let len = H.size + R.size in
  let r = random_r () in
  let r' = Bytes.create len in
  let b = Bytes.create 1 in
  let e = E.make Manual in
  check partial "before adding bytes" `Partial (E.encode e @@ R r) ;
  for i = 0 to len - 1 do
    E.Manual.add_bytes e b 0 1 ;
    if i < len - 1 then
      check partial (Printf.sprintf "loop %d" i) `Partial (E.encode e Await)
    else check partial (Printf.sprintf "loop %d" i) `Ok (E.encode e Await) ;
    Bytes.(set r' i @@ get b 0)
  done ;
  check partial "loop final" `Ok (E.encode e Await) ;
  check partial "loop final" `Ok (E.encode e Await) ;
  check string "header" good_hdr (Bytes.sub_string r' 0 H.size) ;
  check r_testable "record" r (R.read_bytes r' H.size)

let decode_recode_2b_manual () =
  let b = Bytes.make (String.length h_plus_2b) '\000' in
  let d = D.make Manual in
  D.Manual.refill_string d h_plus_2b 0 (String.length h_plus_2b) ;
  let r1 = match D.decode d with R r -> r | _ -> assert false in
  let r2 = match D.decode d with R r -> r | _ -> assert false in
  let e = E.make Manual in
  E.Manual.add_bytes e b 0 (Bytes.length b) ;
  check partial "encode r1" `Ok (E.encode e @@ R r1) ;
  check int "size" R.size (E.Manual.rem e) ;
  check partial "encode r2" `Ok (E.encode e @@ R r2) ;
  check int "rem" 0 (E.Manual.rem e) ;
  check string "" (Bytes.unsafe_to_string b) h_plus_2b ;
  check partial "encode End" `Partial (E.encode e End) ;
  check partial "flush" `Ok (E.encode e Await) ;
  check partial "flush2" `Ok (E.encode e Await)

let decode_recode_2b_smallbuf () =
  let len = H.size + (2 * R.size) in
  let b = Bytes.create 1 in
  let h_plus_2b' = Bytes.create len in
  let d = D.make Manual in
  D.Manual.refill_string d h_plus_2b 0 len ;
  let r1 = match D.decode d with R r -> r | _ -> assert false in
  let r2 = match D.decode d with R r -> r | _ -> assert false in
  let e = E.make Manual in
  check partial "encode r1" `Partial (E.encode e @@ R r1) ;
  for i = 0 to H.size + R.size - 1 do
    E.Manual.add_bytes e b 0 1 ;
    if i = H.size + R.size - 1 then
      check partial "encode r1" `Ok (E.encode e Await)
    else check partial "encode r1" `Partial (E.encode e Await) ;
    Bytes.(set h_plus_2b' i @@ get b 0)
  done ;
  check string "compare IO1"
    (String.sub h_plus_2b 0 @@ (H.size + R.size))
    (Bytes.sub_string h_plus_2b' 0 @@ (H.size + R.size)) ;
  check partial "encode r2" `Partial (E.encode e @@ R r2) ;
  for i = H.size + R.size to H.size + (2 * R.size) - 1 do
    E.Manual.add_bytes e b 0 1 ;
    if i = H.size + (2 * R.size) - 1 then
      check partial "encode r2" `Ok (E.encode e Await)
    else check partial "encode r2" `Partial (E.encode e Await) ;
    Bytes.(set h_plus_2b' i @@ get b 0)
  done ;
  check int "rem" 0 (E.Manual.rem e) ;
  check string "compare IO" h_plus_2b (Bytes.unsafe_to_string h_plus_2b') ;
  check partial "encode End" `Partial (E.encode e End) ;
  check partial "flush" `Ok (E.encode e Await) ;
  check partial "flush2" `Ok (E.encode e Await)

let decode_encode_3pages () =
  let src = Bytes.make (3 * io_buffer_size) '\000' in
  let dst = Buffer.create (3 * io_buffer_size) in
  Bytes.blit_string good_hdr 0 src 0 H.size ;
  fill_random_r src H.size ;
  let d = D.of_bytes src in
  let e = E.of_buffer dst in
  let nb_encoded = ref 0 in
  ( try
      while true do
        match D.decode d with
        | R r -> (
            E.encode e @@ R r
            |> function `Ok -> incr nb_encoded | `Partial -> assert false )
        | End -> failwith "End"
        | Error (Header_invalid s) -> failwith ("Header_invalid " ^ s)
        | Error (Eof b) -> failwith ("EOF " ^ b)
        | Await -> assert false
      done
    with Failure s ->
      check string "eof" "EOF" (String.sub s 0 3) ;
      check partial "encoding `End" `Ok (E.encode e End) ;
      check partial "encoding `Await" `Ok (E.encode e Await) ;
      while E.encode e Await <> `Ok do
        ()
      done ) ;
  let dst = Buffer.contents dst in
  check int "nb_encoded" (((3 * io_buffer_size) - H.size) / R.size) !nb_encoded ;
  check int "buf size"
    ((3 * io_buffer_size) - (((3 * io_buffer_size) - H.size) mod R.size))
    (String.length dst) ;
  check string ""
    Bytes.(sub src 0 ((3 * io_buffer_size) - 32) |> unsafe_to_string)
    dst

let decode_encode_3pages_manual () =
  let src = Bytes.create (3 * io_buffer_size) in
  let dst = Bytes.create io_buffer_size in
  let buf = Buffer.create (3 * io_buffer_size) in
  let nb_records = ((3 * io_buffer_size) - H.size) / R.size in
  Bytes.blit_string good_hdr 0 src 0 H.size ;
  fill_random_r src H.size ;
  let d = D.make Manual in
  let e = E.make Manual in
  let nb_decoded = ref 0 in
  let nb_encoded = ref 0 in
  let refill_count = ref 0 in
  E.Manual.add_bytes e dst 0 io_buffer_size ;
  ( try
      while true do
        match D.decode d with
        | R r -> (
            incr nb_decoded ;
            E.encode e @@ R r
            |> function
            | `Ok -> incr nb_encoded
            | `Partial ->
                Buffer.add_subbytes buf dst 0 (io_buffer_size - E.Manual.rem e) ;
                E.Manual.add_bytes e dst 0 io_buffer_size ;
                while E.encode e Await <> `Ok do
                  ()
                done ;
                incr nb_encoded )
        | End -> failwith "End"
        | Error (Header_invalid s) -> failwith ("Header_invalid " ^ s)
        | Error (Eof b) -> failwith ("EOF " ^ b)
        | Await ->
            if !refill_count < 3 then (
              D.Manual.refill_bytes d src
                (!refill_count * io_buffer_size)
                io_buffer_size ;
              incr refill_count )
            else D.Manual.refill_bytes d src 0 0
        (* Refill with an empty buf to signal EOF *)
      done
    with Failure s ->
      check string "eof" "EOF" (String.sub s 0 3) ;
      check partial "encoding `End" `Partial (E.encode e End) ;
      check partial "encoding `Await" `Ok (E.encode e Await) ;
      while E.encode e Await <> `Ok do
        ()
      done ) ;
  check int "nb_decoded" nb_records !nb_decoded ;
  check int "nb_encoded" nb_records !nb_encoded ;
  check int "encode buffer not used" 32 (E.Manual.rem e) ;
  let first_diff b b' =
    let ret = ref None in
    let len = Bytes.(min (length b) (length b')) in
    ( try
        for i = 0 to len - 1 do
          if Bytes.get b i <> Bytes.get b' i then (
            ret := Some (i, Bytes.get b i, Bytes.get b' i) ;
            raise Exit )
        done
      with Exit -> () ) ;
    !ret in
  assert (None = first_diff src (Buffer.contents buf |> Bytes.unsafe_of_string))

let basic =
  [ test_case "decode_recode" `Quick decode_recode;
    test_case "decode_recode2" `Quick decode_recode2;
    test_case "chk_hdr" `Quick chk_hdr; test_case "empty" `Quick empty;
    test_case "empty_manual" `Quick empty_manual;
    test_case "incomplete_hdr" `Quick incomplete_hdr;
    test_case "incomplete_hdr_ch" `Quick incomplete_hdr_ch;
    test_case "invalid_hdr" `Quick invalid_hdr;
    test_case "invalid_hdr_ch" `Quick invalid_hdr_ch;
    test_case "valid_hdr" `Quick valid_hdr;
    test_case "valid_hdr_ch" `Quick valid_hdr_ch;
    test_case "incomplete_r" `Quick incomplete_r;
    test_case "incomplete_hdr_fd_nb" `Quick incomplete_hdr_fd_nb;
    test_case "invalid_hdr_fd_nb" `Quick invalid_hdr_fd_nb;
    test_case "incomplete_hdr_manual" `Quick incomplete_hdr_manual;
    test_case "invalid_hdr_manual" `Quick invalid_hdr_manual;
    test_case "incomplete_r_nb" `Quick incomplete_r_nb;
    test_case "complete_2b_manual" `Quick complete_2b_manual;
    test_case "decode_smallbuf" `Quick decode_smallbuf;
    test_case "decode_3pages" `Quick decode_3pages;
    test_case "encode_smallbuf" `Quick encode_smallbuf;
    test_case "encode_manual" `Quick encode_manual;
    test_case "decode_recode_2b_manual" `Quick decode_recode_2b_manual;
    test_case "decode_recode_2b_smallbuf" `Quick decode_recode_2b_smallbuf;
    test_case "decode_encode_3pages" `Quick decode_encode_3pages;
    test_case "decode_encode_3pages_manual" `Quick decode_encode_3pages_manual
  ]

let () = run "scid" [("basic", basic)]
