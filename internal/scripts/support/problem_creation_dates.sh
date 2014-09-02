#!/bin/bash
# Create a file of creations dates m

set -o nounset

mkdir -p ${1:-_deploy}
rm -f ${1:-_deploy}/problems_creation_dates.txt
for prob in Problems/*; do
	# python2 strptime does not support timezones(%z) even though the docs says it does
	date="`TZ=UTC git log --date-order  --reverse --pretty=format:'%at' "${prob}/specification.md" \
		| head -n1`"

	echo "`basename ${prob}`,${date}" >>  ${1:-_deploy}/problems_creation_dates.txt
done
