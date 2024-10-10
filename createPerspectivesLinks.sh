#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

#cd .psc-package/pv0.15.0/aff-sockets

##### SPAGO #####
cd .spago

##### AFF-SOCKETS #####
# cd ./aff-sockets
#
# rm -Rf v2.2.1
#
# ln -s ../../../../purescript-aff-sockets v2.2.1
#
# cd ..

##### AVAR-MONADASK #####
cd avar-monadask/

rm -Rf v2.2.0

ln -s ../../../purescript-avar-monadask v2.2.0

cd ..

##### v1.1.0 #####
cd kishimen

rm -Rf v1.1.0

ln -s ../../../purescript-kishimen v1.1.0

cd ..

#### PERSPECTIVES-v2.16.0 #####
cd apitypes

rm -Rf v2.16.0

ln -s ../../../perspectives-apitypes v2.16.0

cd ..

##### PERSPECTIVES-v1.1.0
cd perspectives-utilities

rm -Rf v v1.1.0
ln -s ../../../perspectives-utilities v1.1.0

cd ..

##### SERIALISABLE-NONEMPTY-ARRAYS
cd serializablenonemptyarray

rm -Rf v v1.1.0
ln -s ../../../serialisable-nonempty-arrays v1.1.0

cd ..

##### PERSPECTIVES-v2.9.1 #####
cd perspectives-couchdb/

rm -Rf v2.9.1

ln -s ../../../perspectives-couchdb v2.9.1

cd ..

##### v12.0.0-with-xhr-cookies #####
cd affjax

rm -Rf v12.0.0-with-xhr-cookies

ln -s ../../../purescript-affjax v12.0.0-with-xhr-cookies

cd ..

##### v7.0.0-transformer-tagged #####
cd parsing

rm -Rf v7.0.0-transformer-tagged

ln -s ../../../purescript-parsing v7.0.0-transformer-tagged

cd ..

##### v1.1.0 #####
cd lrucache

rm -Rf v1.1.0

ln -s ../../../purescript-lru-cache v1.1.0

cd ..

##### NODE_MODULES #####
cd ..

cd node_modules

##### PERSPECTIVES-PROXY #####
rm -Rf perspectives-proxy

ln -s ../../perspectives-proxy
