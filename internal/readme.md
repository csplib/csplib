This file provides a brief introduction to the architecture used to construct csplib.

A primary design decision what that no server-side code should have to be run on csplib.org, mainly
to avoid the need to worry about security issues in server side software.

The basic directory structure is:

- web/ - javascript and css files which are copied to the web server unchanged.
- templates/ - jinja2 templates used for page design
- scripts/framework - the python scripts which do the actual website building
- scripts/support - a selection of miscellaneous scripts

The most likely files you are likely to look at is templates and scripts/framework. If you have
any questions about any of these files, please ask!

