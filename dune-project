(lang dune 2.5)

(name mlang)

(version 1.1.0)

(generate_opam_files true)

(source
 (uri git+https://gitlab.inria.fr/verifisc/mlang.git))

(homepage https://gitlab.inria.fr/verifisc/mlang)

(bug_reports https://gitlab.inria.fr/verifisc/mlang/issues)

(authors "Denis Merigoux" "Rapha\195\171l Monat")

(maintainers "denis.merigoux@inria.fr")

(license GPLv3)

(using menhir 2.0)

(package
 (name mlang)
 (synopsis "Compiler for DGFiP's M language")
 (description
  "The Direction G\195\169n\195\169rale des Finances Publiques (DGFiP)\nuses the M language to specify how French taxes are computed\nfrom the characteristics of a fiscal household. This compiler\nparser the M code into a formalized compact intermediate language\nbefore translating it into various backend languages.\n")
 (depends
  (ocaml
   (and(>= "4.11.2")))
  (dune
   (and :build))
  (ANSITerminal
   (= 0.8.2))
  (cmdliner
    (= 1.0.4))
  (re
   (= 1.9.0))
  (ocamlgraph
   (= 1.8.8))
  (odoc
  (= 1.5.3))
  (dune-build-info
   (= 2.5.1))
  (num
   (>= 1.3))
  (mlgmpidl
   (>= 1.2.12))
  (ocamlformat
   (= 0.19.0))
  (parmap
    (= 1.2.3))))
