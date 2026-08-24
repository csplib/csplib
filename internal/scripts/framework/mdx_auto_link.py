# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import markdown
import xml.etree.ElementTree as etree


# from http://daringfireball.net/2010/07/improved_regex_for_matching_urls
# Seem to slow doing building

LINK_RE = r'(?<=\s)(https?://[^\s<>"\[\]{}|\\^`]*[^\s<>"\[\]{}|\\^`.,;:!?\)])(?=\s|$|[<>"\[\]{}|\\^`.,;:!?\)])'

class AutoLink(markdown.inlinepatterns.Pattern):
	def handleMatch(self, m):
		a = etree.Element('a')
		a.set('href', self.unescape(m.group(2)))

		a.text = markdown.util.AtomicString(m.group(2))
		return a


class AutoLinkExtension(markdown.Extension):
	""" AutoLink Extension for Python-Markdown. """

	def extendMarkdown(self, md):
		md.inlinePatterns.register(AutoLink(LINK_RE, md), 'auto_link', 48)


def makeExtension(**kwargs):
	return AutoLinkExtension(**kwargs)
