#!/bin/bash
set -e
. ./internal/scripts/env/bin/activate
set -x

if [[ -n "$TRAVIS_PULL_REQUEST" && "$TRAVIS_PULL_REQUEST" != "false" ]]; then
    prefix="PR-${TRAVIS_PULL_REQUEST}"
    prefix_path="/csplib-PR-builds/$prefix"
else
    prefix="${TRAVIS_BRANCH:-$(git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')}"
    prefix_path="/csplib-builds/$prefix"
fi

./internal/scripts/support/problem_creation_dates.sh "_deploy/${prefix}"
./internal/scripts/framework/generate_web_site.py \
	--prefix_path "${prefix_path}" \
	--output_suffix "${prefix}" \
	--no_index

set +x
