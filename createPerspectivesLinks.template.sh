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

rm -Rf AVAR_MONADASK

ln -s ../../../purescript-avar-monadask AVAR_MONADASK

cd ..

##### KISHIMEN #####
cd kishimen

rm -Rf KISHIMEN

ln -s ../../../purescript-kishimen KISHIMEN

cd ..

#### PERSPECTIVES-APITYPES #####
cd apitypes

rm -Rf APITYPES

ln -s ../../../perspectives-apitypes APITYPES

cd ..

##### PERSPECTIVES-UTILITIES
cd perspectives-utilities

rm -Rf v UTILITIES
ln -s ../../../perspectives-utilities UTILITIES

cd ..

##### SERIALISABLE-NONEMPTY-ARRAYS
cd serializablenonemptyarray

rm -Rf v SERIALIZABLENONEMPTYARRAY
ln -s ../../../serialisable-nonempty-arrays SERIALIZABLENONEMPTYARRAY

cd ..

##### PERSPECTIVES-COUCHDB #####
cd perspectives-couchdb/

rm -Rf COUCHDB

ln -s ../../../perspectives-couchdb COUCHDB

cd ..

##### AFFJAX #####
cd affjax

rm -Rf AFFJAX

ln -s ../../../purescript-affjax AFFJAX

cd ..

##### PARSING #####
cd parsing

rm -Rf PARSING

ln -s ../../../purescript-parsing PARSING

cd ..


##### NODE_MODULES #####
cd ..

cd node_modules

##### PERSPECTIVES-PROXY #####
rm -Rf perspectives-proxy

ln -s ../../perspectives-proxy
