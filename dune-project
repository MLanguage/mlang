(lang dune 2.5)

(name mlang)

(version %%VERSION%%)

(generate_opam_files true)

(source
 (uri git+https://github.com/MLanguage/mlang.git))

(homepage https://github.com/MLanguage/mlang)

(bug_reports https://github.com/MLanguage/mlang/issues)

(authors "Denis Merigoux" "Rapha\195\171l Monat")

(maintainers "denis.merigoux@inria.fr")

(license GPL-3.0-or-later)

(using menhir 2.0)

(package
 (name mlang)
 (synopsis "Compiler for DGFiP's M language")
 (description
  "The Direction G\195\169n\195\169rale des Finances Publiques (DGFiP)\nuses the M language to specify how French taxes are computed\nfrom the characteristics of a fiscal household. This compiler\nparser the M code into a formalized compact intermediate language\nbefore translating it into various backend languages.\n")
 (depends
  (ocaml
   (and
    (>= "4.13.0")))
  (dune
   (and :build))
  (ANSITerminal
   (= 0.8.2))
  (cmdliner
   (= 1.3.0))
  (re
   (= 1.11.0))
  (ocamlgraph
   (= 1.8.8))
  (odoc
   (>= 1.5.3))
  (dune-build-info
   (= 2.9.3))
  (num
   (>= 1.3))
  (mlgmpidl
   (>= 1.2.12))
  (ocamlformat
   (= 0.24.1))
  (parmap
   (= 1.2.3))))

(package
 (name irj_checker)
 (synopsis "IRJ test validation tool")
 (description
  "This standalone module performs a syntactic validation of the DGFiP IRJ test format")
 (depends
  (ocaml
   (and
    (>= "4.11.2")))
  (dune
   (and :build))
  (odoc
   (>= 1.5.3))
  (ocamlformat
   (= 0.27.0))))
