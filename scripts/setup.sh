#!/bin/bash
if [ ! -n "env" ]; then
	./scripts/pyvenvex.py env
fi

. ./env/bin/activate
echo 'run `deactivate` to stop'

pip install -r  scripts/packages.txt

