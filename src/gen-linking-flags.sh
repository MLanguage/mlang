#!/bin/sh
set -ue

# Copied on https://ocamlpro.com/fr/blog/2021_09_02_generating_static_and_portable_executables_with_ocaml/

LINKING_MODE="$1"
OS="$2"
FLAGS=
CCLIB=

case "$LINKING_MODE" in
    '')
        ;; # No extra flags needed
    static)
        case "$OS" in
            linux)
                FLAGS="-noautolink"
                CCLIB="-Wl,-Bstatic -lgmp_caml -lmpfr -lgmp -lnums \
                       -lthreadsnat -lparmap_stubs -lANSITerminal_stubs \
                       -Wl,-Bdynamic -lpthread -lunix"
                LIBS=""
                OCAML_LIBS="camlidl"
                for lib in $LIBS; do
                    CCLIB="$CCLIB $(pkg-config $lib --variable libdir)/$lib.a"
                done
                for lib in $OCAML_LIBS; do
                    CCLIB="$CCLIB $(ocamlfind query $lib)/lib$lib.a"
                done;;
            *)
                echo "No known static compilation flags for '$OS'" >&2
                exit 1
        esac;;
    *)
        echo "Invalid linking mode '$LINKING_MODE'" >&2
        exit 2
esac

echo '('
for f in $FLAGS; do echo "  $f"; done
for f in $CCLIB; do echo "  -cclib $f"; done
echo ')'
