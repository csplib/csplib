# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import markdown
import re

PROB_LINK_RE = r'\[(prob\d+)\]',
# PROB_DATA is shared with generate_web_site
PROB_DATA = {}

class ProbLink(markdown.inlinepatterns.Pattern):
	def handleMatch(self, m):
		base = markdown.util.etree.Element('span')
		base.text=' '
		ref = m.group(2)
		url = '~~PREFIX_PATH~~/Problems/' + ref

		el = markdown.util.etree.Element("a")
		el.set('href', url)

		try:
			val =  "[{}:{}]".format(ref, PROB_DATA[ref]['title'])
		except KeyError:
			val =  "[{}]".format(ref)


		el.text= markdown.util.AtomicString(val)
		base.append(el)

		return base


class ProbLinkExtension(markdown.Extension):
	""" ProbLink Extension for Python-Markdown. """

	def extendMarkdown(self, md, md_globals):
		md.inlinePatterns['prob_link'] = ProbLink(PROB_LINK_RE, md)


def makeExtension(configs=None):
	return ProbLinkExtension(configs=configs)
