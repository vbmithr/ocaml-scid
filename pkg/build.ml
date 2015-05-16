#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "scid" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "lib/scid";
    Pkg.bin ~auto:true "lib_test/suite";
    Pkg.bin ~auto:true "lib_test/browser";
    Pkg.bin ~auto:true "lib_test/copy";
    Pkg.bin ~auto:true "lib_test/test_decoder";
  ]
