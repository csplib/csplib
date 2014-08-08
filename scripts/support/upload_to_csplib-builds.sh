#!/bin/bash

set -o nounset
set -x

if [[   "$TRAVIS_PULL_REQUEST" == "false" && $TRAVIS_PYTHON_VERSION == '3.4' ]]; then
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

  ./create_index_page.sh

  #add, commit and push files
  git add -f .
  git commit -m "Travis build $TRAVIS_BUILD_NUMBER Commit csplib/csplib@$TRAVIS_COMMIT Python $TRAVIS_PYTHON_VERSION Commit Range $TRAVIS_COMMIT_RANGE branch $TRAVIS_BRANCH"
  git push -fq origin gh-pages > /dev/null

  echo -e "<<Finished>>\n"
fi


if [[ "$TRAVIS_PULL_REQUEST" != "false" && $TRAVIS_PYTHON_VERSION == '3.4' ]]; then

    # Only upload a PR which only edited the Problems or Languages pages
    if ( ! git diff --name-only --find-renames HEAD~1 | grep -qv 'Problems' | grep -qv 'Languages' ); then
        # Only upload a PR which uses no script or link tags
        if ( ! git diff --no-color HEAD~1 Problems/ Languages/ | egrep '<[ \t]*(script|link)' --color=no ); then

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

            ./create_index_page.sh

            #add, commit and push files
            git add -f .
            git commit -m "Travis build $TRAVIS_BUILD_NUMBER PR csplib/#$TRAVIS_PULL_REQUEST Commit csplib/csplib@$TRAVIS_COMMIT Python $TRAVIS_PYTHON_VERSION Commit Range $TRAVIS_COMMIT_RANGE branch $TRAVIS_BRANCH"
            # git push -fq origin gh-pages > /dev/null

            echo -e "<<Finished>>\n"

        fi
    fi


fi


set +x
