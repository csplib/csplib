#!/bin/bash
# Creates a template for a new problem
set -o nounset

if [ $# -eq 0 ]; then
	echo "Creates a template for a new problem."
	prev=0
	cur=0

	for cur in $(ls -1 Problems/ | egrep -o 'prob[0-9]+' | sort -n | sed -e 's/prob*//' | sed 's/^0*//'); do
		if ((  cur != prev + 1 )); then
			cur="${prev}"
			break
		fi
		prev="${cur}"
	done

	(( cur++ ))

	echo "Run the following to use the next available number:"
	printf "  $0 prob%03d " "${cur}"

	exit 0
fi

prob_dir="./Problems/$1"

if [ -d "$prob_dir" ]; then
	echo "$prob_dir exists!"
	exit 1
fi

mkdir "$prob_dir"
echo "Created $prob_dir"
mkdir "$prob_dir/data"
touch "$prob_dir/data/.gitkeep"
mkdir "$prob_dir/models"
touch "$prob_dir/models/.gitkeep"
mkdir "$prob_dir/results"
touch "$prob_dir/results/.gitkeep"
mkdir "$prob_dir/assets"
touch "$prob_dir/assets/.gitkeep"


touch "$prob_dir/specification.md"

echo "---" >> "$prob_dir/specification.md"
echo "Title:    Problem" >> "$prob_dir/specification.md"
echo "Proposer: Person" >> "$prob_dir/specification.md"
echo "Category: Stuff" >> "$prob_dir/specification.md"
echo "---" >> "$prob_dir/specification.md"

echo "" >> "$prob_dir/specification.md"
echo "" >> "$prob_dir/specification.md"

echo "Problem description" >> "$prob_dir/specification.md"

mkdir "$prob_dir/references"
touch "$prob_dir/references/.gitkeep"
echo "" > "$prob_dir/references/references.bib"
echo "" > "$prob_dir/references/notes.inline.md"


echo "Created $prob_dir/specification.md"
echo ""

echo "Optionally add references (BibTeX format) in $prob_dir/references.bib"
echo "See ./Problems/prob001 or ./Problems/prob002 for an example"
