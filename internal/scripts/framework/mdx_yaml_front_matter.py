from __future__ import absolute_import
from __future__ import unicode_literals

from markdown import Extension
from markdown.preprocessors import Preprocessor

import re
import yaml


class YamlFrontMatterExtension (Extension):
	"""  Add github like `yaml front matter` to python markdown  """
	
	def extendMarkdown(self, md, md_globals):
		md.preprocessors.add("meta", YamlFrontMatterPreprocessor(md), "_begin")


class YamlFrontMatterPreprocessor(Preprocessor):

	# example
	"""
	---
	Title:    My title
	Proposer: 
		- Author one
		- Author two
	---
	"""

	# Parse markdown and store converted yaml in .Meta
	def run(self, lines):
		yaml_block = []		
		in_yaml = False
		line = lines.pop(0)
		
		if re.match(r'-{3}', line):
			in_yaml = True
		else:
			lines.insert(0, line)
		
		while in_yaml and lines:
			line = lines.pop(0)
			if re.match(r'(?:\.{3}|-{3})', line):
				break
			yaml_block.append(line)
			
		if yaml_block:
			yaml_block = "\n".join(yaml_block)
			yaml_block = yaml_block.expandtabs(4)
			meta       = yaml.load(yaml_block)
			
			def ensure_list(obj):
				if isinstance(obj,list):
					return obj
				else:
					return [obj]
			
			# make key case-insensitise and make each value a list
			meta = { key.lower(): ensure_list(val) for key, val in meta.items() }
			self.markdown.Meta = meta
			
		return lines


def makeExtension(configs=None):
	return YamlFrontMatterExtension(configs=configs)
