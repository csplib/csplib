# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import markdown
import re

PROB_LINK_RE = r'\[(prob\d+)\]',


class CitePattern(markdown.inlinepatterns.Pattern):
	numbered = {}

	def handleMatch(self, m):
		base = markdown.util.etree.Element('span')
		base.text=' '
		ref = m.group(2)
		url = '~~PREFIX_PATH~~/Problems/' + ref

		el = markdown.util.etree.Element("a")
		el.set('href', url)
		el.set('class', 'bibref')
		el.set('data-bibfragment', ref)
		el.text = markdown.util.AtomicString("[{0}]".format(ref))
		base.append(el)

		return base


class ProbLinkExtension(markdown.Extension):
	""" ProbLink Extension for Python-Markdown. """

	def extendMarkdown(self, md, md_globals):
		md.inlinePatterns['prob_link'] = CitePattern(PROB_LINK_RE, md)


def makeExtension(configs=None):
	return ProbLinkExtension(configs=configs)
