# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import markdown
import re


# from http://daringfireball.net/2010/07/improved_regex_for_matching_urls
LINK_RE = r"""(?xi)
\b
(                                      # Capture 1: entire matched URL
  (?:
    https?://                          # http or https protocol
    |                                  # or
    www\d{0,3}[.]                      # "www.", "www1.", "www2." … "www999."
    |                                  # or
    [a-z0-9.\-]+[.][a-z]{2,4}.         # looks like domain name followed by a slash
  )
  (?:                                  # One or more:
    [^\s()<>]+                         # Run of non-space, non-()<>
    |                                  # or
    \(([^\s()<>]+|(\([^\s()<>]+\)))*\) # balanced parens, up to 2 levels
  )+
  (?:                                  # End with:
    \(([^\s()<>]+|(\([^\s()<>]+\)))*\) # balanced parens, up to 2 levels
    |                                  # or
    [^\s`!()\[\]{};:'".,<>?«»“”‘’]     # not a space or one of these punct chars
  )
)
"""

class AutoLink(markdown.inlinepatterns.Pattern):
	def handleMatch(self, m):
		a = markdown.util.etree.Element('a')

		href = m.group(2)
		if not re.match('^(ftp|https?)://', href, flags=re.IGNORECASE | re.VERBOSE):
			href = 'http://%s' % href
		a.set('href', self.unescape(href))

		a.text = markdown.util.AtomicString(m.group(2))
		return a


class AutoLinkExtension(markdown.Extension):
	""" AutoLink Extension for Python-Markdown. """

	def extendMarkdown(self, md, md_globals):
		md.inlinePatterns['auto_link'] = AutoLink(LINK_RE, md)


def makeExtension(configs=None):
	return AutoLinkExtension(configs=configs)
