(lang dune 3.4)

(name CoBruh)

(generate_opam_files true)

(source
 (github Akash-Nayar/CoBruh))

(authors "Akash Nayar, Lance Wong, Joshua Zhou")

(package
 (name CoBruh)
 (synopsis "CoBruh compiler")
 (description "A CoBruh compiler written in OCaml.")
 (depends ocaml dune llvm llvm.analysis))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
