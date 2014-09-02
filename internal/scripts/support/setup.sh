#!/bin/bash

if ( python3 -c 'import sys; sys.exit((sys.version_info[0] == 3 and sys.version_info[1] >=3 ))' ); then
	echo "Python 3.3+ is required"
	exit 1
fi


if [ ! -d "internal/scripts/env" ]; then

	# if using python3.4 pip is already installed
	if ( python3 -c 'import sys; sys.exit((sys.version_info[0] == 3 and sys.version_info[1] ==3 ))' ); then
		python3 -m venv internal/scripts/env
	else
		./internal/scripts/support/pyvenvex.py internal/scripts/env
	fi
fi

. ./internal/scripts/env/bin/activate
printf "pip @ "
which pip

pip install -r  internal/scripts/support/packages.txt

# guess-language-spirit is required by mdx_smartypants and hosted at https://bitbucket.org/spirit/guess_language/downloads
# pip 1.5+ does not like this and requires extra flags
# We put this here, to avoid adding '--pre' to other packages
pip install --allow-external guess-language-spirit --allow-unverified guess-language-spirit mdx_smartypants==1.5.1 --pre

echo "<<Finished>>"
