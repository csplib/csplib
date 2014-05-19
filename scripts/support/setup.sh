#!/bin/bash
if [ ! -d "scripts/env" ]; then
	./scripts/support/pyvenvex.py scripts/env
fi

. ./scripts/env/bin/activate
printf "pip @ "
which pip

pip install -r  scripts/support/packages.txt

# guess-language-spirit is required by mdx_smartypants and hosted at https://bitbucket.org/spirit/guess_language/downloads 
# pip 1.5+ does not like this and requires extra flags
# We put this here, to avoid adding '--pre' to other packages
pip install --allow-external guess-language-spirit --allow-unverified guess-language-spirit mdx_smartypants==1.5.1 --pre  

echo "<<Finished>>"
