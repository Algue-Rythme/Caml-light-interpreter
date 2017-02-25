#!/bin/sh
formula=/tmp/Formula
robdd=/tmp/ROBDD

make \
&& ./calc \
&& dot $formula.dot -Tpdf -o $formula.pdf \
&& dot $robdd.dot -Tpdf -o $robdd.pdf \
&& evince $formula.pdf $robdd.pdf
