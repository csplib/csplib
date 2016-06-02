# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import markdown
import re


REL_RE = r"""(?x)
 (/(Problems|Languages|authors.html|updates.html)[-0-9_a-z/A-Z.]*)([.,!?;: ]|$)
"""

# prefix_path is shared with generate_web_site
PREFIX_PATH = None

class RelLink(markdown.inlinepatterns.Pattern):
	def handleMatch(self, m):
		# base = markdown.util.etree.Element('span')
		# base.text=' '

		url = PREFIX_PATH + m.group(2)
		a = markdown.util.etree.Element('a')
		a.text = markdown.util.AtomicString(m.group(2) + m.group(4))
		a.set('href', url)
		print("m:{} groups:{} g2:'{}'".format(m, m.groups(), m.group(2) ))
		print("a href {} a text {}".format(a.get("href"),a.text	 ))
		# base.append(a)
		return a


class RelLinkExtension(markdown.Extension):
	""" RelLink Extension for Python-Markdown. """

	def extendMarkdown(self, md, md_globals):
		md.inlinePatterns['rel_link'] = RelLink(REL_RE, md)


def makeExtension(configs=None):
	return RelLinkExtension(configs=configs)
