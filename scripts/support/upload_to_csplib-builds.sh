#!/bin/bash

if [[   "$TRAVIS_PULL_REQUEST" == "false" && $TRAVIS_PYTHON_VERSION == '3.4'  ]]; then
  echo -e "Starting to update gh-pages\n"

  #copy data we're interested in to other place
  cp -R _deploy $HOME/_deploy

  #go to home and setup git
  cd $HOME
  git config --global user.email "admin@csplib.org"
  git config --global user.name "csplib-robot"

  #using token clone gh-pages branch
  git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com/csplib/csplib-builds.git  gh-pages > /dev/null

  #go into diractory and copy data we're interested in to that directory
  cd gh-pages
  cp -Rf $HOME/_deploy/* .

  #add, commit and push files
  git add -f .
  git commit -m "Travis build $TRAVIS_BUILD_NUMBER Python $TRAVIS_PYTHON_VERSION commit $TRAVIS_COMMIT_RANGE pushed to gh-pages"
  git push -fq origin gh-pages > /dev/null

  echo -e "Done magic with coverage\n"
fi