#!/bin/bash

## and excellent guide, for this setup!
## https://github.com/steveklabnik/automatically_update_github_pages_with_travis_example

set -o errexit -o nounset

rev=$(git rev-parse --short HEAD)



#rm -rf inst/staticdocs
git clone -b gh-pages "https://$GH_TOKEN@github.com/sahilseth/flowr.git" gh-pages


## make relevent changes
echo `pwd`
Rscript misc/build.R


#touch .
cd gh-pages

git config user.name "Sahil Seth"
git config user.email "me@sahilseth.com"

git add -A .
git commit -m "rebuild pages at ${rev}"
git push

#git push -q upstream HEAD:gh-pages
