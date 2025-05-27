(lang dune 3.7)
; use >3.7 otherwise 2.9 will generate error about bin broken symlink
(name tigerc)
(version 0.1.0)

(package
 (name tigerc)
 (synopsis "A simple example of an OCaml project")
 (description "A _really_ long description of a simple project")
 (source (github aryx/fork-tiger))
 (authors "Yoann Padioleau <yoann.padioleau@gmail.com>")
 (maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
 (depends
  ; DO NOT declare dune/odoc as dependencies, dune will automatically add them
  ;(ocaml (>= 4.14.0))
  ;atdgen
  ;(yojson (>= 2.0.0))
  ;ppxlib
  ;ppx_deriving
  ;ppx_inline_test
  ;(ocamlformat :with-test)
  ;; if we want to depend only for testing
  ;;(alcotest :with-test)
))
(generate_opam_files)
