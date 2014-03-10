#!/bin/bash
# Assume pip is installed

pip install --user -r  scripts/packages.txt

# assume pip <1.5  (since if they could install the lastest version of pip they should just install python3)
pip install --user mdx_smartypants==1.5.1
pip install --user argparse

echo "<<Finished>>"
