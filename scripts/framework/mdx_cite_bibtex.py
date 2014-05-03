# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import markdown
import re

CITE_BIBTEX_RE = r' ?cite\{([\w:/-]+?)\}',


class CitePattern(markdown.inlinepatterns.Pattern):
	numbered = {}

	def handleMatch(self, m):
		ref = m.group(2)
		# to allow in a html Fragment
		ref = re.sub("[^\w]", "_", ref)
		ref = re.sub("^(\d+)", "_\\1", ref)
		url = '~~PROB_BASE~~/references/#' + ref

		if ref in self.numbered:
			num = self.numbered[ref]
		else:
			num = len(self.numbered) + 1
			self.numbered[ref] = num

		el = markdown.util.etree.Element("a")
		el.set('href', url)

		# number refs
		# el.text = markdown.util.AtomicString("[{0}]".format(num))
		# bibkey refs
		el.text = markdown.util.AtomicString(" [{0}]".format(ref))

		return el


class CiteBibtexExtension(markdown.Extension):
	""" cite_bibtex Extension for Python-Markdown. """

	def extendMarkdown(self, md, md_globals):
		md.inlinePatterns['cite_bibtex'] = CitePattern(CITE_BIBTEX_RE, md)


def makeExtension(configs=None):
	return CiteBibtexExtension(configs=configs)
