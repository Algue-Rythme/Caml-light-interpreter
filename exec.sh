#!/bin/sh
formula=/tmp/Formula
robdd=/tmp/ROBDD

make \
&& ./f2bdd "$@" \
&& dot $robdd.dot -Tpdf -o $robdd.pdf \
&& evince $robdd.pdf
