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

rm -Rf v2.1.1

ln -s ../../../purescript-avar-monadask v2.1.1

cd ..

##### v1.1.0 #####
cd kishimen

rm -Rf v1.1.0

ln -s ../../../purescript-kishimen v1.1.0

cd ..

#### PERSPECTIVES-v2.13.2 #####
cd apitypes

rm -Rf v2.13.2

ln -s ../../../perspectives-apitypes v2.13.2

cd ..

##### PERSPECTIVES-v1.0.0
cd perspectives-utilities

rm -Rf v v1.0.0
ln -s ../../../perspectives-utilities v1.0.0

cd ..

##### SERIALISABLE-NONEMPTY-ARRAYS
cd serializablenonemptyarray

rm -Rf v v1.0.1
ln -s ../../../serialisable-nonempty-arrays v1.0.1

cd ..

##### PERSPECTIVES-v2.8.5 #####
cd perspectives-couchdb/

rm -Rf v2.8.5

ln -s ../../../perspectives-couchdb v2.8.5

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


##### NODE_MODULES #####
cd ..

cd node_modules

##### PERSPECTIVES-PROXY #####
rm -Rf perspectives-proxy

ln -s ../../perspectives-proxy
