#!/bin/sh

make && ./calc && dot /tmp/out.dot -Tpdf -o /tmp/out.pdf && evince /tmp/out.pdf
