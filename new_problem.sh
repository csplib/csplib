#!/bin/bash
# Creates a template for a new problem
set -o nounset

if [ $# -eq 0 ]; then
	echo "$0 new_problem_name"
	echo "Creates a template for a new problem"
	highest=`ls -1 Problems/ | egrep 'prob[0-9]+' | sort -n | tail -n 1 | sed -e 's/prob*//'`
	if [ $highest -lt 0 ]; then
		highest=0
	fi
	(( highest++ ))

	printf "e.g $0 prob%03d for the next free problem number\n" $highest
	exit 0
fi

prob_dir="./Problems/$1"

if [ -d "$prob_dir" ]; then
	echo "$prob_dir exists!"
	exit 1
fi

mkdir "$prob_dir"
echo "Created $prob_dir"

touch "$prob_dir/specification.md" 
echo "Title:    " >> "$prob_dir/specification.md" 
echo "Proposer: " >> "$prob_dir/specification.md" 
echo "Category: " >> "$prob_dir/specification.md" 
echo "" >> "$prob_dir/specification.md" 
echo "" >> "$prob_dir/specification.md" 

echo "Created $prob_dir/specification.md"
echo "" 
echo "Optionally add references in bibtex $prob_dir/references.bib"
echo "    or in html $prob_dir/references.html"
echo "See ./Problems/prob001 or ./Problems/prob002 for an example"

