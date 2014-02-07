Requries Python 3.3+, bibtex

To build
	make

To view locally:
	make
	make serve
	<open localhost:8000 in your browser>
	
To add a new problem use new_problem.sh to create a basic outline.

The minimum required to create a new problem is to complete 
specification.md.

References are added to references.bib or references.html.

Optionally add models in models/, data in data/ and results in results/.
Files in these directories are handled specially. See below for details.

The assets/ directory will be copied but not automatically linked to.
Use this directory for files, such as images, which are linked to from
specification.md
	
See prob001, prob002, and prob005 for examples


models, data and results directories
------------------------------------

Each of these directories are handled in the same way.

Data files can be added to these directories by simply placing the
file here. Human readable files are displayed in place, binary files
are given a download link. If any file is incorrectly identified
as binary or human readable, please submit a bug report!

You can describe a file 'file.txt' in a file 'file.txt.metadata'.
This file should contain a single line of the form:

Title: This describes the data file

You can finally use files with the extension .inline-html or
.inline-md which will be displayed inline directly in the 
results/references/model tab. See problem XXX for an example
of all these types of files!

