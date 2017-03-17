#!/bin/sh
formula=out/Formula
robdd=out/ROBDD

make \
&& ./f2bdd "$@" \
&& dot $formula.dot -Tpdf -o $formula.pdf \
&& dot $robdd.dot -Tpdf -o $robdd.pdf \
&& evince $formula.pdf $robdd.pdf
