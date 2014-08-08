#!/bin/bash
set -e
. ./scripts/env/bin/activate
set -x

if [[ -n "$TRAVIS_PULL_REQUEST" && "$TRAVIS_PULL_REQUEST" != "false" ]]; then
    prefix_path="PR-${TRAVIS_PULL_REQUEST}"
else
    prefix_path="${TRAVIS_BRANCH:-$(git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')}"
fi

./scripts/support/problem_creation_dates.sh "_deploy/${prefix_path}"
./scripts/framework/generate_web_site.py \
	--prefix_path "/csplib-builds/${prefix_path}" \
	--output_suffix "${prefix_path}"

set +x