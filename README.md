# The Mlang compiler

![CI Tests](https://github.com/MLanguage/mlang/actions/workflows/check_correctness.yml/badge.svg)
[![Documentation deployed](https://img.shields.io/badge/Developper%20documentation-deployed-GREEN.svg)](https://mlanguage.github.io/mlang/mlang/index.html)

The M language has been invented by the French Direction Générale des Finances
Publiques (DGFiP), equivalent to the IRS, to transcribe the tax code into machine-readable
instructions. It is a small Domain Specific Language based on variable
declarations and arithmetic operations. This work is based on a retro-engineering
of the syntax and the semantics of M, from the codebase previously released by the DGFiP on
[Framagit forge](https://framagit.org) and now regularly published on
[Adullact forge](https://gitlab.adullact.net/dgfip/ir-calcul).

## Disclaimer

There is currently no legal guarantee of any kind about the correctness of the code
produced by the Mlang compiler, or by the results produced by the interpreter of
Mlang. However, authors have been in contact with DGFiP to validate Mlang, and
the system passes all the private DGFiP tests as of Sept. 2022 for the version
of the source files responsible for computing the tax on the 2018, 2019, 2020 and 2021 income.

## Installation

Mlang is implemented in OCaml. To manage dependencies,
[install opam](https://opam.ocaml.org/doc/Install.html) and switch to a version
of OCaml that is at least 4.0.9. In order to support multi-precision floating-point
computation, you will need also need to install the MPFR library.

For Debian-based distributions, simply invoke

    sudo apt install libgmp-dev libmpfr-dev git opam

For Red Hat-based distributions, first invoke

    sudo yum install gmp-devel mpfr-devel git

Opam is only packaged for Fedora. For other distributions using RPM, please refer to
the [official documentation](https://opam.ocaml.org/doc/Install.html). Note that to use
the Opam binary release and install Mlang dependencies, you will need a C compiler and
the following softwares as Opam dependencies: `patch`,`unzip`, `bubblewrap` and `bzip2`.

If you would like to generate tests using the fuzzer, you will need to install AFL:

    sudo apt install afl++ afl++-clang

If you have not used opam before, run:

    opam init
    opam update -y

Then, you can initialize your Mlang projet using

    make init

This command creates a local Opam switch (analogous to a virtual environment), installs
Mlang's OCaml dependencies and clones the M source code repository released by the DGFiP with

    git submodule update --init ir-calcul

You can then use

    make build

to build the compiler.

If needed,

    make deps

will reinstall OCaml dependencies and refetch the M source code.

The interpreter and the C backend in `examples/dgfip_c/` should be usable straight away,
as the C compiler was installed for Opam. Mlang results are tested on GCC and Clang, the latter
being preferred if available.

> You will probably need to do the following to get the repo to work correctly :
> 1. install `gmp` and `mpfr` C libraries
> 1. set the C compiler flags correctly :
> ```shell
> export LIBRARY_PATH=$HOMEBREW_PREFIX/lib:$LIBRARY_PATH
> export C_INCLUDE_PATH=$HOMEBREW_PREFIX/include:$C_INCLUDE_PATH
> ```
> you can replace `$HOMEBREW_PREFIX/include` with wherever your includes lives, and similarly for `$HOMEBREW_PREFIX/lib`
> 
> 2. `eval $(opam env)`
> 2. `opam install menhir`

## Usage

Mlang also need an M file to know how to run the "liquidations multiples"
mechanism that is necessary to compute the income tax correctly. For instance, the file
`ir_calcul/2022/cible.m` corresponds to the unpublished code of the DGFiP
for version of the 2022 M sources published in `ir-calcul`.

Some of the `Makefile` flags can be permanently configured by modifying the
file `makefiles/variables.mk`.

If you want to generate source files of the ML backend, run the command:

    make YEAR=<2020 or 2022> dgfip_c_backend

or

    make dgfip_c_backend

with default year 2022. The files are then generated in `example/dgfip_c/ml_primitif/calc`.

If you want to generate the executable of the ML backend, run the command:

    make YEAR=<2020 or 2022> compile_dgfip_c_backend

or

    make compile_dgfip_c_backend

with default year 2022.

## Testing

Mlang is tested using the `FIP` test file format used by the DGFiP to test
their internal tooling. The `--run_test` and `--run_all_tests` options ease
the testing process of the interpreter (with or without optimizations) and
report test errors in a convenient format.

Mlang backends are also tested using the same `FIP` format.

When running `--run_all_tests`, you can enable code coverage instrumentation
with the `--code_coverage` option. Another interesting option is `--precision`,
which lets you choose how numbers are represented for the tax computation.
The default is `--precision double`, that uses the 64-bits IEEE754 floating-point
representation and associated operations. This is what the DGFiP uses. The
`--precision mpfr` option lets you use 1024-bits floating point numbers for
virtually infinite precision. Finally, `--precision fixed<n>` uses
fixed-point arithmetic with GMP-provided big integers. The fixed-point numbers
are represented with the [Q number format](https://en.wikipedia.org/wiki/Q_(number_format))
and `<n>` is the number of fractional bits. The integer bits are unbounded.

The DGFiP does not publish its internal test base. However, randomized test
cases have been created for the 2018, 2019 and 2020 income versions of the software, in the
folder `tests`. The fact that Mlang passes these tests only means that
it faithfully reproduces the computation done by the DGFiP using unpublished
software. Notably, it does not mean that the M sources (published by the
DGFiP) and the M++ sources (recreated from unpublished sources) are faithful to
the way the law says taxes should be computed.

To check that Mlang passes all the randomized tests, simply invoke

    make tests

Some tests might fail using non-default precision settings, even if the error
message shows no difference between the expected value and the computed value.
This is because we control a difference of 0 between the computed and the
expected, but when doing computations with a higher precision, a difference
lower than the smallest representable float value might appear. To pass the test,
we have provided the command line option `--test_error_margin=0.0000001` to
let you define how much error margin you want to tolerate when running tests.

If you want to run the `mlang` interpreter on all tests of an income tax year,
run the command:

    make YEAR=<2020 or 2022> tests

or

    make tests

with default year 2022.

If you want to run the `mlang` interpreter on a specific test of an income tax year,
run the command:

    make YEAR=<2020 or 2022> TEST_ONE=<test file> test_one

or

    make TEST_ONE=<test file> test_one

with default year 2022. The tests files are stored in `tests/<year>/fuzzing/`.

If you want to test the output of the interpreter on a situation you made up,
edit your own `.m_test` and run it with the command:

    make YEAR=<2020 or 2022> TEST_FILE=<path to .m_test> make test_file

or

    make TEST_ONE=<test file> test_one

with default year 2022.

If you want to test the output of the ML backend on all tests of an income tax year,
run the command:

    make YEAR=<2020 or 2022> test_dgfip_c_backend

or

    make TEST_ONE=<test file> test_dgfip_c_backend

with default year 2022.

Please read the `tests/README.md` for a walk-through of what happens in input files.

## Documentation

The OCaml code is self-documented using `ocamldoc` style. You can generate the HTML
documentation using

    make doc

To browse the documentation, just open the file `doc.html` with your browser. Here
is a high-level picture describing the architecture of the compiler:

<center>
<img src="doc/architecture.png" alt="Architecture" height="300"/>
</center>

First, the code is parsed into AST (both for M and M++). Then, the AST are
desugared into M and M++ intermediate representations. BIR stands for Backend
IR, and collects the result of inlining the M code inside M++. OIR is the
Optimization IR, which is a CFG-form of BIR.

## Known Limitations

The code released by the DGFiP is not complete as of September 2020. Indeed,
in order to correctly compute the amount of taxes for a fiscal household, the DGFiP
executes the M program several times, each time changing the values of some variables
to enable or disable parts of the computation.

The DGFiP has not published the source code of this iterative computation. However,
the authors of Mlang have come up with a new DSL called M++, used for describing
the logic of this iterative computation. Currently, the authors have transcribed
the unpublished source code into the `mpp_specs/*2018_6_7*.mpp` file, which has been tested only
for the computation of taxes for the 2018, 2019 and 2020 income.

## Contributions

The project accepts pull requests. There is currently no formalized contribution
guide or centralized discussion place about the project. Please email the authors
if you are interested:

  denis DOT merigoux AT inria DOT fr
  raphael DOT monat AT lip6 DOT fr

Please note that the copyright of this code is owned by Inria; by contributing,
you disclaim all copyright interests in favor of Inria.

Don't forget format to use `make format` before you commit to ensure a uniform style.

## Formal semantics

The `formal_semantics` folder contains the formalization for the core of the
M language, that roughly corresponds to the `Mir` internal representation in Mlang.
The reference formalization is written in Coq, in file `semantique.v`.
See [the research paper](https://hal.inria.fr/hal-03002266) for
more details.

## License

The compiler is released under the GPL license (version 3).
