#!/bin/bash

## and excellent guide, for this setup!
## https://github.com/steveklabnik/automatically_update_github_pages_with_travis_example

set -o errexit -o nounset

rev=$(git rev-parse --short HEAD)



#rm -rf inst/staticdocs
git clone "https://$GH_TOKEN@github.com/sahilseth/flowrdocs.git" flowrdocs


## make relevent changes
echo `pwd`
Rscript misc/deploy.R


#touch .
cd flowrdocs

git config user.name "Sahil Seth"
git config user.email "me@sahilseth.com"

git add -A .
git commit -m "rebuild pages at ${rev}"
git push

#git push -q upstream HEAD:gh-pages
