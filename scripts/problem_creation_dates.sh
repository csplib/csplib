#!/bin/bash
# Create a file of creations dates m

set -o nounset

mkdir -p _deploy
rm -f _deploy/problems_creation_dates.txt
for prob in Problems/*; do

	date="`git log --date-order --reverse --pretty=format:'%ai' "${prob}/specification.md" \
		| head -n1`"

	echo "`basename ${prob}`,${date}" >>  _deploy/problems_creation_dates.txt
done