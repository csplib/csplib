# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import markdown
import re


import xml.etree.ElementTree as etree

PROB_LINK_RE = r'([\[{])(\w+)[}\]]',
# PROB_DATA is shared with generate_web_site
PROB_DATA = {}
# prefix_path is shared with generate_web_site
PREFIX_PATH = None

class ProbLink(markdown.inlinepatterns.Pattern):
	def handleMatch(self, m):
		base = etree.Element('span')
		base.text=' '
		ref = m.group(3)

		try:
			if m.group(2) == '{':
				val =  "{}".format(PROB_DATA[ref]['title'])
			else:
				val =  "[{}]".format(ref)
			if PROB_DATA[ref]['is_language'] == True:
				kind = "Languages"
			else:
				kind = "Problems"

		except KeyError:
			# val =  "[{}]".format(ref)
			# kind =  'Problems'
			return None


		url = '{}/{}/{}'.format(PREFIX_PATH,kind,ref)

		el = etree.Element("a")
		el.set('href', url)

		el.text= markdown.util.AtomicString(val)
		base.append(el)

		return base


class ProbLinkExtension(markdown.Extension):
	""" ProbLink Extension for Python-Markdown. """

	def extendMarkdown(self, md):
		md.inlinePatterns.register(ProbLink(PROB_LINK_RE, md), 'prob_link', 176)


def makeExtension(**kwargs):
	return ProbLinkExtension(**kwargs)
