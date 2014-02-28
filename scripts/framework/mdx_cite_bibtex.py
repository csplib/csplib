#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import markdown

CITE_BIBTEX_RE = r'cite\{(\w+?)\}',


class CitePattern(markdown.inlinepatterns.Pattern):
	def handleMatch(self, m):
		from pprint import pprint
		pprint(locals())
		pprint(m.groups())
		pprint(m.expand('\\1  --- \\2'))
		pprint(m.string)
		ref = m.group(2)


		url = 'references/#' + ref

		el = markdown.util.etree.Element("a")
		el.set('href', url)
		el.text = markdown.util.AtomicString("[" + ref + "]")
		return el


class CiteBibtexExtension(markdown.Extension):
	""" cite_bibtex Extension for Python-Markdown. """

	def extendMarkdown(self, md, md_globals):
		""" Replace autolink with UrlizePattern """
		md.inlinePatterns['cite_bibtex'] = CitePattern(CITE_BIBTEX_RE, md)


def makeExtension(configs=None):
	return CiteBibtexExtension(configs=configs)

if __name__ == "__main__":
	import doctest
	doctest.testmod()