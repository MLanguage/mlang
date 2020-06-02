(lang dune 2.2)
(name mlang)
(version 1.0.3)
(generate_opam_files true)

(source (uri git+https://gitlab.inria.fr/verifisc/mlang.git))
(homepage https://gitlab.inria.fr/verifisc/mlang)
(bug_reports https://gitlab.inria.fr/verifisc/mlang/issues)
(authors "Denis Merigoux" "Raphaël Monat")
(maintainers "denis.merigoux@inria.fr")
(license GPLv3)

(using menhir 2.0)
(package
 (name mlang)
 (synopsis "Compiler for DGFiP's M language")
 (description "\| The Direction Générale des Finances Publiques (DGFiP)
              "\| uses the M language to specify how French taxes are computed
              "\| from the characteristics of a fiscal household. This compiler
              "\| parser the M code into a formalized compact intermediate language
              "\| before translating it into various backend languages.
 )
 (depends
   (ocaml (>= 4.09.1))
   (dune (and :build ))
   (ANSITerminal (= 0.8.2))
   (re (= 1.9.0))
   (ppx_deriving (= 4.4))
   (visitors (>= 20200210))
   (ocamlgraph (= 1.8.8))
   (z3 (= 4.8.7))
   (dune-build-info (= 2.5.1))
   (num (>= 1.3))
 )
)
