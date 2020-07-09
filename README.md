# The Mlang compiler

[![CI](https://gitlab.inria.fr/verifisc/mlang/badges/master/pipeline.svg)](https://gitlab.inria.fr/verifisc/mlang/-/commits/master)

The M language has been invented by the French Direction Générale des Finances
Publiques (DGFiP), equivalent to the IRS, to transcribe the tax code into machine-readable
instructions. It is a small Domain Specific Language based on variable
declarations and arithmetic operations. This work is based on a retro-engineering
of the syntax and the semantics of M, from the [codebase](https://framagit.org/dgfip/ir-calcul)
released by the DGFiP.

## Disclaimer

The results obtained by compiling the DGFiP M sources with this compiler are not endorsed by the
DGFiP. Official simulators are available on the [DGFiP's website](https://www.impots.gouv.fr/portail/simulateurs). This project

## Installation

Mlang is implemented in OCaml. To manage dependencies,
[install opam](https://opam.ocaml.org/doc/Install.html) and switch to a version
of OCaml that is at least 4.09.1. Then, you can install Mlang's dependencies using

    make deps

Next, fetch the source code dependencies of Mlang with

    git submodule update --init --recursive

This will fetch the M source code released by the DGFiP.

You can then use `make build` to build the compiler.

## Usage

As of July 2020, the only reliable feature of the M compiler that consistently produces results similar to the DGFiP's computation
is the interpreter for the 2018 tax campaign sources. To use it, simply launch

```
make interpreter
```

The input variables that you want to use have to be declared beforehand in the `interpreter.m_spec`
file, in the `const` section. If you put a variable in the `saisie` section, you will then be
prompted to input it at interpretation time. You can also change which variables you want the
interpreter to output in the `sortie` section.

Please refer to the DGFiP's simulator for the meaning of the variables. Important variables are:

* `0AC` and `0AM`, which should be set to 1 for respectively single or married;
* `1AJ` and `1BJ`, salaried income for individuals number 1 and 2;
* `0CF`, the number of dependent persons (children);
* ...

After the evaluation, an interactive prompt let you examine the values of other variables.

## Documentation

The OCaml code is self-documented using `ocamldoc` style. You can generate the HTML
documentation using

        make doc

To browse the documentation, just open the file `doc.html` with your browser.

## Known Limitations

The code published by the DGFiP on their [Framagit repo](https://framagit.org/dgfip/ir-calcul)
is not complete as of July 2020. Indeed,
in order to correctly compute the amount of taxes for a fiscal household, the DGFiP
executes the M program several times, each time changing the values of some variables
to enable or disable parts of the computation.

However, the authors of Mlang have signed a convention with the DGFiP in order to come and

The DGFiP has not published the details of this iterative computation. Hence,
until they do, the amounts of taxes computed by Mlang-generated programs are usually
false (except on very simple situations).

## Contributors

The project accepts pull requests. There is currently no formalized contribution
guide or centralized discussion place about the project. Please email the authors
if you are interested.

Please note that the copyright of this code is owned by Inria; by contributing,
you disclaim all copyright interests in favor of Inria.

Don't format to use `make format` before you commit to ensure a uniform style.

## Formal semantics

The `formal_semantics` folder contains two separate formalizations for the core of the
M language, that roughly corresponds to the `Mvg` internal representation in Mlang.
The reference formalization is the Coq one, in file `semantique.v`. The F* formalization
is a proof of concept.

## License

The compiler is released under the GPL license (version 3).
