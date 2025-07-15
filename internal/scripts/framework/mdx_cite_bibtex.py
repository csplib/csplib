# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import markdown
import re
import xml.etree.ElementTree as etree

CITE_BIBTEX_RE = r' ?cite\{([,\ \w:/-]+?)\}'


class CitePattern(markdown.inlinepatterns.InlineProcessor):
	numbered = {}

	def handleMatch(self, m, data):
		base = etree.Element('span')
		base.text=' '
		refs = m.group(1).split(",")
		for (i,ref) in enumerate(refs):
			ref = ref.strip()
			# to allow in a html Fragment
			ref = re.sub(r"[^\w]", r"_", ref)
			ref = re.sub(r"^(\d+)", r"_\\1", ref)
			url = '~~PROB_BASE~~/references/#' + ref

			if ref in self.numbered:
				num = self.numbered[ref]
			else:
				num = len(self.numbered) + 1
				self.numbered[ref] = num

			el = etree.Element("a")
			el.set('href', url)
			el.set('class', 'bibref')
			el.set('data-bibfragment', ref)

			# format cite{a,b} as  [a, b]
			fmt = "[{0}]"
			if len(refs) > 1:
				if i == 0:
					fmt = "[{0}"
				elif i == len(refs) - 1:
					fmt = ", {0}]"
				else:
					fmt = ", {0}"

			el.text = markdown.util.AtomicString(fmt.format(ref))
			base.append(el)


		# number refs
		# el.text = markdown.util.AtomicString("[{0}]".format(num))
		# bibkey refs


		return base, m.start(0), m.end(0)


class CiteBibtexExtension(markdown.Extension):
	""" cite_bibtex Extension for Python-Markdown. """

	def extendMarkdown(self, md):
		md.inlinePatterns.register(CitePattern(CITE_BIBTEX_RE, md), 'cite_bibtex', 175)


def makeExtension(**kwargs):
	return CiteBibtexExtension(**kwargs)
