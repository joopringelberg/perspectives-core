#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

cd .psc-package/pv0.3.0/aff-sockets

rm -Rf v2.2.0

ln -s ../../../../purescript-aff-sockets v2.2.0

cd ../perpectives-apitypes

rm -Rf v2.3.0

ln -s ../../../../perspectives-apitypes v2.3.0

cd ../avar-monadask/

rm -Rf v2.1.0

ln -s ../../../../purescript-avar-monadask v2.1.0

cd ../perspectives-couchdb/

rm -Rf v2.5.8

ln -s ../../../../perspectives-couchdb v2.5.8

cd ../../..
