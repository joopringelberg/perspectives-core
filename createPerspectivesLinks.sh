#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version of aff-sockets!

cd .psc-package/psc-12-perspectives/aff-sockets

rm -Rf v1.1.1

ln -s ../../../../purescript-aff-sockets/ v1.1.1

cd ../perpectives-apitypes

rm -Rf v1.0.0

ln -s ../../../../perspectives-apitypes v1.0.0

cd ../avar-monadask/

rm -Rf v1.0.0

ln -s ../../../../avar-monadask v1.0.0

cd ../../..
