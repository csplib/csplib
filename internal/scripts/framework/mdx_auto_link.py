# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import markdown
import re
import xml.etree.ElementTree as etree


# from http://daringfireball.net/2010/07/improved_regex_for_matching_urls
# Seem to slow doing building

LINK_RE = r'\b((?:https?://|(?:[a-z0-9.-]+\.(?:[a-z]{2,})))(?:[^\s<>"\[\]{}|\\^`]*[^\s<>"\[\]{}|\\^`.,;:!?])?)(?=\s|$|[<>"\[\]{}|\\^`.,;:!?])'

class AutoLink(markdown.inlinepatterns.Pattern):
	def handleMatch(self, m):
		a = etree.Element('a')

		href = m.group(2)
		if not re.match('^(ftp|https?)://', href, flags=re.IGNORECASE):
			href = 'http://%s' % href
		a.set('href', self.unescape(href))

		a.text = markdown.util.AtomicString(m.group(2))
		return a


class AutoLinkExtension(markdown.Extension):
	""" AutoLink Extension for Python-Markdown. """

	def extendMarkdown(self, md):
		md.inlinePatterns.register(AutoLink(LINK_RE, md), 'auto_link', 177)


def makeExtension(**kwargs):
	return AutoLinkExtension(**kwargs)
