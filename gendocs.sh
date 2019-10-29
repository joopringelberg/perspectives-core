#!/usr/bin/env bash

# Github pages (https://help.github.com/en/github/working-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site) publishes only from the directory 'docs'.
# Pulp (https://github.com/purescript-contrib/pulp#building-documentation) only creates documentation in 'generated-docs'
# This script runs the documentation generator and moves the output of Pulp to 'docs'

pulp docs -- --format html

rm -R docs
mv generated-docs docs
