Requires Python 3.3+,  (Python 3.4+ on windows)

To build a local copy of csplib to view locally type:

    make        # Builds CSPLib to the _deploy directory
    make serve  # Starts a local web server to view the files
    <open localhost:8000 in your browser>

To add a new problem use `new_problem.sh` to create a basic outline.


# Problem Specification

Each problem is stored in the `Problems` directory. The best way
to get a feeling for how a problem is stored is to look at an existing
problem (Problem/prob001 is a good start).

Each problem is built from the following parts:

* [specification.md](#specificationmd) - The basic description of the problem.
This will contain a title, proposer, category, and an English description of the
problem. This is technically all that is required.

* [references](#references) - This directory contains the references, usually
in `references.bib` file as bibtex.

* [models, data and results](#models-data-and-results-directories) - The models,
data and results directories store models of the problem, data files and any known
results. The exact structure of these is described below.


The assets/ directory will be copied but not automatically linked to.
Use this directory for files, such as images, which are linked to from
specification.md

## specification.md

The specification needs to start with a `Title`, `Proposer` and `Category` in the following format:

    ---
    Title:    Magic Hexagon
    Proposer: Toby Walsh
    Category:
        - Design and configuration
        - Combinatorial mathematics
        - Games and puzzles
    ---

If the `Proposer` or `Category` have multiple values, they are put on
a new line as shown above.

The rest of the specification is written in [markdown](https://help.github.com/articles/github-flavored-markdown).
 References from the [references](#references) section can be cited using the notation `cite{bibref}`.


## References

References are stored in the `references` directory. The file `references.bib` should contain references
in standard BibTeX format. Any text in the file `notes.inline.md` will be listed at the top of the references page.

A problem can be referenced using its id, [prob058] will be shown hyperlinked as [prob058:Discrete Lot Sizing Problem].


## Models, data and results directories

Each of these directories are handled in the same way.

Files can be added to these directories by simply placing the file in the appropriate directory.
Human readable files are displayed in place, binary files are given a download link.
If any file is incorrectly identified as binary or human readable, please submit a bug report!

You can describe a file `file.txt` in a file `file.txt.metadata`. This can
contain a `Title`, and a `Type`. The file should be formatted as follows:

    ---
    Title: This describes the data file
    Type: The type
    ---

In general, `Type` is only useful for models and data.

You can also use files with the extension
`.inline.md` which will be displayed inline directly in the
results/references/model tab. See problem prob013 for an example
of all these types of files.

# Language Specification

Languages are stored in the `Languages` directory. Languages do not store the models,
data and results, these are stored with the problems and linked from the languages.

## specification.md for Languages

The specification of languages is similar to problems, with a simpler design. Languages
should contain a title, and (single or list of) extensions:

    ---
    Title: MiniZinc
    Extensions:
        - mzn
        - minizinc
    ---

The extensions will be matched against the file extensions, or `Type` contained in all
[models, data and results](#models-data-and-results-directories) of all problems.

# Website building scripts

An overview of the architecture used to build csplib can be found at [internal/readme.md](internal/readme.md).

# Development builds

Development builds of every branch and pull requests are located at http://csplib.github.io/csplib-builds/
