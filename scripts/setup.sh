#!/bin/bash
if [ ! -d "scripts/env" ]; then
	./scripts/pyvenvex.py scripts/env
fi

. ./scripts/env/bin/activate
printf "pip @ "
which pip

pip install -r  scripts/packages.txt

echo "<<Finished>>"