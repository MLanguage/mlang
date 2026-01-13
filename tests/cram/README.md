# Mlang cram tests

## What are cram tests?

Cram tests are a dune feature that mimics a bash and checks commands behave
as expected.
You can run them with:

```
$ dune build @runtest
```

If you want to run a specific test (for example `ok/arithmetique.t`), you can run:

```
$ dune build @ok/arithmetique
```

The whole documentation is available [here](https://dune.readthedocs.io/en/latest/reference/cram.html).


## How it is set up

There are two directories: `ok` and `ko`. 
The `ok` directory contains all the tests that actually behave as expected and
we want to preserve.
The `ko` directory contains the tests that are bugs in mlang and we would like
to solve. Our goal is to empty this directory if it is not. When such files
are solved, we move them to `ok`.
We never want to move a file from `ok` to `ko`. Ever.

## How to contribute

When you want to add a test, add a new directory in `ok` or `ko` (or in a 
subdirectory of `ok` or `ko`). This directory must have an explicit name with
a `.t` extension.
This directory must contain all the files required for testing. Usually, it has
at least three files:
* `run.t` contains the commands that will be run in sequence;
* `file.m` is the `M` program to test;
* `vars.irj` is the IRJ file to run `file.m`.

Once this is done, you may run it with

```
$ dune build @PATH/TO/YOUR/DIR/WITHOUT/EXTENSION --auto-promote
```

and check the new content of your `run.t` file.

NB: you may use the option `--no_nondet_display` in the commands in `run.t`
to avoid unpredictable displays.

## I made a change and the cram tests now fail, what should I do?

Run the following command:

```
$ dune build @runtest --auto-promote
```

This will update the `run.t` files with the new behavior. You can check the differences between the version without your change and with your change.
Once your solve everything, make sure to re-run the command to update the tests to their
final version.
