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
  set +x
  echo 'git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com/csplib/csplib-builds.git  gh-pages > /dev/null'
  git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com/csplib/csplib-builds.git  gh-pages > /dev/null
  set -x

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
    # This token can only push to csplib-PR-builds
    export CSPLIB_ROBOT_TOKEN=c924c9149eeed0c238ba3076b838e5b85daa5918

    # Only upload a PR which only edited the Problems or Languages pages
    if ( ! git diff --name-only --find-renames HEAD~1 | grep -qv 'Problems' | grep -qv 'Languages' ); then
        # Only upload a PR which uses no script or link tags
        if ( ! git diff --no-color HEAD~1 Problems/ Languages/ | egrep '<[ \t]*(script|link)' --color=no ); then

            #copy data we're interested in to other place
            cp -R _deploy $HOME/_deploy

            #go to home and setup git
            pushd $HOME
            git config --global user.email "admin@csplib.org"
            git config --global user.name "csplib-robot"

            #using token clone gh-pages branch
            set +x
            # echo 'git clone --quiet --branch=gh-pages https://${CSPLIB_ROBOT_TOKEN}@github.com/csplib/csplib-PR-builds.git  gh-pages > /dev/null'
            # git clone --quiet --branch=gh-pages https://${CSPLIB_ROBOT_TOKEN}@github.com/csplib/csplib-PR-builds.git  gh-pages > /dev/null
            ./internal/scripts/support/csplib-private clone
            set -x

            #go into diractory and copy data we're interested in to that directory
            pushd gh-pages
            cp -Rf $HOME/_deploy/* .

            ./create_index_page.sh

            #add, commit and push files
            git add -f .
            git commit -m "Travis build $TRAVIS_BUILD_NUMBER PR csplib/csplib#$TRAVIS_PULL_REQUEST Commit csplib/csplib@$TRAVIS_COMMIT Python $TRAVIS_PYTHON_VERSION Commit Range $TRAVIS_COMMIT_RANGE branch $TRAVIS_BRANCH"
            git push -fq origin gh-pages > /dev/null

            echo -e "<<Finished>>\n"
            
            popd
            popd
            if [[ " $(openssl sha1 ./internal/scripts/support/add_preview_link_to_pr.py) " == " SHA1(./internal/scripts/support/add_preview_link_to_pr.py)= b12a6be410a73a439b973578710ed341368ae922 " ]]; then
                  ./internal/scripts/support/add_preview_link_to_pr.py
            else
                echo "./internal/scripts/support/add_preview_link_to_pr.py has been edited" 
            fi

        fi
    fi


fi


set +x
