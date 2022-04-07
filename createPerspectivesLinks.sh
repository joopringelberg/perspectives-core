#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

#cd .psc-package/pv0.15.0/aff-sockets

##### SPAGO #####
cd ./.spago

##### AFF-SOCKETS #####
# cd ./aff-sockets
#
# rm -Rf v2.2.1
#
# ln -s ../../../../purescript-aff-sockets v2.2.1
#
# cd ..

##### AFFJAX #####

cd ./affjax

rm -Rf v12.0.0-with-xhr-cookies

ln -s ../../../../purescript-affjax v12.0.0-with-xhr-cookies

cd ..

##### PERSPECTIVES-APITYPES #####
cd ./perpectives-apitypes

rm -Rf v2.13.2

ln -s ../../../../perspectives-apitypes v2.13.2

cd ..

##### AVAR-MONADASK #####
cd ./avar-monadask/

rm -Rf v2.1.0

ln -s ../../../../purescript-avar-monadask v2.1.0

cd ..


##### PERSPECTIVES-COUCHDB #####
cd ./perspectives-couchdb/

rm -Rf v2.8.5

ln -s ../../../../perspectives-couchdb v2.8.5

cd ..

##### NODE_MODULES #####
cd ..

cd ./node_modules

##### PERSPECTIVES-PROXY #####
rm -Rf perspectives-proxy

ln -s ../../perspectives-proxy
