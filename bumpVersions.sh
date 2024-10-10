#!/usr/bin/env bash

# Modify the version numbers of dependencies as needed. Then run ./bumpVersions.sh to create updated versions of
# * packages.dhall
# * createPerspectivesLinks.sh
# * package.json

AVAR_MONADASK=v2.2.0
KISHIMEN=v1.1.0
APITYPES=v2.16.0
UTILITIES=v1.1.0
SERIALIZABLENONEMPTYARRAY=v1.1.0
COUCHDB=v2.9.1
AFFJAX=v12.0.0-with-xhr-cookies
PARSING=v7.0.0-transformer-tagged
PERSPECTIVESPROXY=v1.21.0
LRUCACHE=v1.1.0

sed "s/AVAR_MONADASK/${AVAR_MONADASK}/g;\
s/KISHIMEN/${KISHIMEN}/g;\
s/APITYPES/${APITYPES}/g;\
s/UTILITIES/${UTILITIES}/g;\
s/SERIALIZABLENONEMPTYARRAY/${SERIALIZABLENONEMPTYARRAY}/g;\
s/COUCHDB/${COUCHDB}/g;\
s/AFFJAX/${AFFJAX}/g;\
s/PARSING/${PARSING}/g;\
s/LRUCACHE/${LRUCACHE}/g;\
" packages.template.dhall > packages.dhall

sed "s/AVAR_MONADASK/${AVAR_MONADASK}/g;\
s/KISHIMEN/${KISHIMEN}/g;\
s/APITYPES/${APITYPES}/g;\
s/UTILITIES/${UTILITIES}/g;\
s/SERIALIZABLENONEMPTYARRAY/${SERIALIZABLENONEMPTYARRAY}/g;\
s/COUCHDB/${COUCHDB}/g;\
s/AFFJAX/${AFFJAX}/g;\
s/PARSING/${PARSING}/g;\
s/LRUCACHE/${LRUCACHE}/g;\
" createPerspectivesLinks.template.sh > createPerspectivesLinks.sh

sed "s/PERSPECTIVESPROXY/${PERSPECTIVESPROXY}/g;" package.template.json > package.json