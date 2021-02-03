#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

cd .psc-package/pv0.9.0/aff-sockets

rm -Rf v2.2.0

ln -s ../../../../purescript-aff-sockets v2.2.0

cd ../affjax

rm -Rf pv9.1.0

ln -s ../../../../purescript-affjax pv9.1.0

cd ../perpectives-apitypes

rm -Rf v2.9.0

ln -s ../../../../perspectives-apitypes v2.9.0

cd ../avar-monadask/

rm -Rf v2.1.0

ln -s ../../../../purescript-avar-monadask v2.1.0

cd ../perspectives-couchdb/

rm -Rf v2.8.2

ln -s ../../../../perspectives-couchdb v2.8.2

cd ../../..

cd ./node_modules

rm -Rf perspectives-proxy

ln -s ../../perspectives-proxy
