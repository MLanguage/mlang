# Verifisc M compiler

The M language has been invented by the French Direction Générale des Finances
Publiques, equivalent to the IRS, to transcribe the tax code into machine-readable
instructions. It is a small Domain Specific Language based on variable
declarations and arithmetic operations. This work is based on a retro-engineering
of the syntax and the semantics of M.


## Installation

You will need an OCaml distribution with the following Opam packages:

        ppx_deriving ANSITerminal ocamlgraph z3 odoc odig

The Z3 Opam package takes a very long time to install. To build the project, simply enter:

        make build

## Usage

The command

        make test

will launch the compiler of the test file `test.m`. However, if you want to
compile all the source code files released by the DGFiP for the year 2017,
and analyse the code that computes the income tax, then launch

        make iliad

To learn more about the available options, you can use

        make build
        ./main.native --help

## Documentation

The OCaml code is self-documented using `ocamldoc`. You can generate the HTML
documentation using

        make doc

The output will be in the `doc` folder, rooted at file `index.html` which should
display this very page. To access the modules' documentation, go to
 [the Verifisc package documentation](verifisc/index.html).
