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
   ppx_deriving
  (ocaml (>= 4.14.0))
))
(generate_opam_files)
