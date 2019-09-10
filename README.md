# The Mlang compiler

The M language has been invented by the French Direction Générale des Finances
Publiques, equivalent to the IRS, to transcribe the tax code into machine-readable
instructions. It is a small Domain Specific Language based on variable
declarations and arithmetic operations. This work is based on a retro-engineering
of the syntax and the semantics of M, from the [codebase](https://framagit.org/dgfip/ir-calcul)
released by the DGFiP.


## Installation

You will need an OCaml distribution with the following Opam packages:

        opam install ppx_deriving ANSITerminal ocamlgraph z3 re odoc odig

The Z3 Opam package takes a very long time to install (10 minutes). You also need
to install the [Verifisc](https://gitlab.inria.fr/verifisc/verifisc) intermediate
language library as an opam package (follow the instructions in the README of
this repo).

To install the compiler as an opam package, simply use:

    opam install ./

Otherwise, the project is managed using Dune, so you can use `dune build` or
`dune exec` commands. To use the M source code released by the DGFiP, you have
to initiate the `ir-calcul` submodule with

    git submodule update --init --recursive

## Usage

The `Makefile` contains several examples of invocation of the `mlang` executable.

For instance, if you want to
compile all the source code files released by the DGFiP for the year 2017,
and analyse the code that computes the income tax, then launch

        make case_basique_2018

## Documentation

The OCaml code is self-documented using `ocamldoc`. You can generate the HTML
documentation using

        make doc

The output will be in the `doc` folder, rooted at file `index.html`.

## License

The compiler is released under the GPL license (version 3).
