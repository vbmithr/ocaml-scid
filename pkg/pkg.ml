#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "scid" @@ fun c ->
  Ok [
    Pkg.mllib "src/scid.mllib" ;
    Pkg.test ~run:true "lib_test/suite" ;
    Pkg.test ~run:false "lib_test/browser" ;
    Pkg.test ~run:false "lib_test/copy" ;
    Pkg.test ~run:false "lib_test/test_decoder" ;
  ]
