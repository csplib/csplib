#!/bin/bash
if ( python3 -c 'import sys; sys.exit((sys.version_info[0] == 3 and sys.version_info[1] >=4 ))' ); then
	echo "Python 3.4+ is required"
	exit 1
fi


if [ ! -d "internal/scripts/env" ]; then
	python3 -m venv internal/scripts/env
fi

. ./internal/scripts/env/bin/activate
printf "pip @ "
which pip

# Don't use pip new cache feature since at lest on OSX it requires sudo, when I tested Python 3.5
if ( pip --no-cache-dir &>/dev/null ); then
	pip install wheel==0.37.0 --no-cache-dir
	pip install -r  internal/scripts/support/packages.txt --no-cache-dir
	pip install packages/*.gz --no-cache-dir
else
	pip install wheel==0.37.0
	pip install -r  internal/scripts/support/packages.txt
	pip install packages/*.gz
fi

echo "<<Finished>>"