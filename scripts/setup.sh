#!/bin/bash
if [ ! -d "scripts/env" ]; then
	./scripts/pyvenvex.py scripts/env
fi

. ./scripts/env/bin/activate
printf "pip @ "
which pip

pip install -r  scripts/packages.txt
# We put this here, to avoid adding '--pre' to other packages
pip install mdx_smartypants==1.5.0 --pre

echo "<<Finished>>"
