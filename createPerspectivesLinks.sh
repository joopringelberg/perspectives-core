#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version of aff-sockets!

cd .psc-package/psc-0.11.7-perspectives-core3/aff-sockets

rm -R v1.1.1

ln -s ../../../../purescript-aff-sockets/ v1.1.1

cd ../../..
