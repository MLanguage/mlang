# Semantics of M

The definition and proofs related to the Semantics of M (§2) are shown in `semantics.v`.

The expressions are defined by `Inductive expression`, commands by `Inductive command`.
The semantics of expressions is defined as function `eval`, and the one of commands and program as `exec` and `exec_program` respectively.

The well-formedness judgment over the expressions (respectively commands, program) is defined using the inductive rules `well_formed` (respectively `well_formed_cmd`, `well_formed_prog`).

The soundness theorems are `soundness_expr`, `soundness_cmd`, `soundness_prog`. The definitions `related_envs_*` matches the `\rhd` notation of the paper.

The file has been tested using Coq 8.12.0 and `coq-flocq` 3.3.1 (which is a library describing the IEEE 754 floats, although we do not use it much). Both can be installed using [opam](https://opam.ocaml.org/):
```shell
# add opam repository used by flocq
opam repo add coq-released https://coq.inria.fr/opam/released
opam install coq=8.12.0 coq-flocq=3.3.1
```
