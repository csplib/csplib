#!/bin/bash
# Add the missing directories and files

set -o nounset
prob=$1
cd $prob

for dir in assets data models references results; do
	if [ ! -d "$dir" ]; then
		echo "Creating $prob/$dir"
		mkdir "$dir"
		touch "$dir/.gitkeep"
	fi
done

if [ ! -f "references/notes.inline.md" ]; then
	echo "Creating note references"
	touch "references/notes.inline.md"
fi