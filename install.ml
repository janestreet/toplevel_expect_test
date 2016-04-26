#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"toplevel_expect_test"
  [ oasis_lib "toplevel_expect_test"
  ; oasis_lib "toplevel_expect_test_glue"
  ; oasis_lib "toplevel_expect_test_types"
  ; file "META" ~section:"lib"
  ; oasis_exe "ocaml-expect" ~dest:"ocaml-expect"
  ]
