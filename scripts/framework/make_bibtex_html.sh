#!/bin/bash
set -x
set -o nounset
cd "$( dirname "$0" )"
./bib2xhtml -s paragraph "$1"
set +x