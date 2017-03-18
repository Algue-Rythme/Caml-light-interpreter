#!/bin/sh
formula=out/Formula
robdd=out/ROBDD

make \
&& ./f2bdd "$@" \
&& dot $robdd.dot -Tpdf -o $robdd.pdf \
&& evince $robdd.pdf
