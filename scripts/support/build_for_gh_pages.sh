#!/bin/bash
set -e
. ./scripts/env/bin/activate
set -x

branch="$(git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')"
./scripts/support/problem_creation_dates.sh "_deploy/${branch}"
./scripts/framework/generate_web_site.py \
	--prefix_path "/csplib-builds/${branch}" \
	--output_suffix "${branch}"

set +x