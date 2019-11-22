#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

cd .psc-package/psc-12.5-perspectives/aff-sockets

rm -Rf v2.0.0

ln -s ../../../../purescript-aff-sockets/ v2.0.0

cd ../perpectives-apitypes

rm -Rf v2.0.0

ln -s ../../../../perspectives-apitypes v2.0.0

cd ../avar-monadask/

rm -Rf v2.0.0

ln -s ../../../../purescript-avar-monadask v2.0.0

cd ../perspectives-couchdb/

rm -Rf v2.5.0

ln -s ../../../../perspectives-couchdb v2.4.0

cd ../../..
