#!/usr/bin/env bash

# NOTE: this script should be adapted with each new tagged version!

#cd .psc-package/pv0.15.0/aff-sockets

cd node_modules

##### PERSPECTIVES-PROXY #####
rm -Rf perspectives-proxy

ln -s ../../perspectives-proxy
