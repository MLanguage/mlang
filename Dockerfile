FROM ubuntu:20.04

ENV APT_DEPS opam libgmp-dev libmpfr-dev m4 perl python3 clang git

ENV OPAM_DEPS ppx_deriving ANSITerminal re ocamlgraph dune menhir cmdliner dune-build-info visitors parmap num ocamlformat mlgmpidl

ENV TERM xterm-256color

RUN apt-get update
RUN apt-get install -y -qq $APT_DEPS
RUN adduser --disabled-password --gecos 'Mlang' mlang

USER mlang
WORKDIR /home/mlang
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

RUN opam init -y  --disable-sandboxing
RUN eval $(opam env)
RUN opam update -y
RUN opam switch create 4.11.1 -y
RUN opam install -y -j 8 $OPAM_DEPS
RUN echo $(opam env) >> /home/mlang/.bashrc