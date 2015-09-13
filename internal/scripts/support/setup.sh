#!/bin/bash
if ( python3 -c 'import sys; sys.exit((sys.version_info[0] == 3 and sys.version_info[1] >=3 ))' ); then
	echo "Python 3.3+ is required"
	exit 1
fi


if [ ! -d "internal/scripts/env" ]; then

	# if using python3.4 pip is already installed
	if ( python3 -c 'import sys; sys.exit((sys.version_info[0] == 3 and sys.version_info[1] ==3 ))' ); then


		# Ubuntu 14.04, decided to disable ensurepip
		# https://github.com/csplib/csplib/issues/24
		if (python3 -Im ensurepip); then
			python3 -m venv internal/scripts/env
		else
			echo "ensurepip did not work, installing pip manually..."
			./internal/scripts/support/pyvenvex.py internal/scripts/env
		fi

	else
		./internal/scripts/support/pyvenvex.py internal/scripts/env
	fi
fi

. ./internal/scripts/env/bin/activate
printf "pip @ "
which pip

# Don't use pip new cache feature since at lest on OSX it requires sudo, when I tested Python 3.5
if ( pip --no-cache-dir &>/dev/null ); then
	pip install -r  internal/scripts/support/packages.txt --no-cache-dir
else
	pip install -r  internal/scripts/support/packages.txt
fi


# guess-language-spirit is required by mdx_smartypants and hosted at https://bitbucket.org/spirit/guess_language/downloads
# pip 1.5+ does not like this and requires extra flags
# We put this here, to avoid adding '--pre' to other packages
if ( pip --no-cache-dir &>/dev/null ); then
	pip install --allow-external guess-language-spirit --allow-unverified guess-language-spirit mdx_smartypants==1.5.1 --pre --no-cache-dir
else
	pip install --allow-external guess-language-spirit --allow-unverified guess-language-spirit mdx_smartypants==1.5.1 --pre
fi


echo "<<Finished>>"