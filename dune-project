(lang dune 1.10)
(name mlang)
(version 1.0.2)
(generate_opam_files true)

(source (uri git+https://gitlab.inria.fr/verifisc/verifisc-m.git))
(homepage https://gitlab.inria.fr/verifisc/verifisc-m)
(bug_reports https://gitlab.inria.fr/verifisc/verifisc-m/issues)
(authors "Denis Merigoux" "Raphaël Monat" )
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
   (ocaml (>= 4.07.0))
   (dune (and :build ))
   (ANSITerminal (= 0.8.1))
   (re (= 1.9.0))
   (ppx_deriving (= 4.4))
   (visitors (>= 20190711))
   (ocamlgraph (= 1.8.8))
   (z3 (= 4.8.4))
   (specifisc (= 1.0.2))
   (dune-build-info (= 1.11.0))
 )
)

(package
 (name verifisc)
 (synopsis "Low-level language for tax code specification")
 (description "\| The Verifisc language is designed to be a low-level target for
              "\| higher-level specification languages for fiscal legislation.
 )
 (depends
   (ocaml (>= 4.07.0))
   (dune (and :build ))
 )
)
