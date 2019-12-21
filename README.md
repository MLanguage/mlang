# The Mlang compiler

The M language has been invented by the French Direction Générale des Finances
Publiques, equivalent to the IRS, to transcribe the tax code into machine-readable
instructions. It is a small Domain Specific Language based on variable
declarations and arithmetic operations. This work is based on a retro-engineering
of the syntax and the semantics of M, from the [codebase](https://framagit.org/dgfip/ir-calcul)
released by the DGFiP.

## Disclaimer

There is currently no guarantee of any kind about the correctness of the code
produced by the Mlang compiler, or by the results produced by the interpreter of
Mlang. However, authors are in contact with DGFiP to try and validate the
semantics of M implemented in Mlang.

## Installation

Mlang is implemented in OCaml. To manage dependencies,
[install opam](https://opam.ocaml.org/doc/Install.html) and switch to a version
of OCaml that is at least 4.0.7. Then, you can install Mlang's dependencies using

    make deps

Warning: the Z3 opam package takes a very long time to install (10 minutes).
Next, fetch the source code dependencies of Mlang with

    git submodule update --init --recursive

This will fetch the M source code released by the DGFiP.

You can then use `dune build` to build all source files, and other dune commands.

If you want to install the `mlang` executable and the opam packages, use

    chmod +x install.h && ./install.sh

## Usage

If the `mlang` executable is install, you can consult its man page with

    mlang --help

The `examples` folder  contains several examples of invocation of the `mlang` executable,
catagorized by the backend used. For instance, if you want to
compile all the source code files released by the DGFiP for the year 2017,
and produce a Python module equivalent to the official simplified simulator available
[published by the DGFiP](https://www3.impots.gouv.fr/simulateur/calcul_impot/2018/simplifie/index.htm),
go to the `examples/python` folder and use

        make simulateur_simplifie_2018

Mlang should generate a file named `ir_2018.py` containing the generated Python code.

## Documentation

The OCaml code is self-documented using `ocamldoc` style. You can generate the HTML
documentation using

        make doc

To browse the documentation, just open the file `doc.html` with your browser.

## Known Limitations

The code released by the DGFiP is not complete as of September 2019. Indeed,
in order to correctly compute the amount of taxes for a fiscal household, the DGFiP
executes the M program several times, each time changing the values of some variables
to enable or disable parts of the computation.

The DGFiP has not published the details of this iterative computation. Hence,
until they do, the amounts of taxes computed by Mlang-generated programs are usually
false (except on very simple situations).

## Contributors

The project accepts pull requests. There is currently no formalized contribution
guide or centralized discussion place about the project. Please email the authors
if you are interested.

Please note that the copyright of this code is owned by Inria; by contributing,
you disclaim all copyright interests in favor of Inria.

## Formal semantics

The `formal_semantics` folder contains two separate formalizations for the core of the
M language, that roughly corresponds to the `Mvg` internal representation in Mlang.
The reference formalization is the Coq one, in file `semantique.v`. The F* formalization
is a proof of concept.

## License

The compiler is released under the GPL license (version 3).
