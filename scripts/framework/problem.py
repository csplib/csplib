# -*- coding: utf-8 -*-
# Bilal Syed Hussain
import logging
import os, os.path as path

import bibtex

from distutils import dir_util, file_util
from pprint import pformat

from util import convert_markdown, makedirs_exist_ok, get_content_and_metadata

logger = logging.getLogger(__name__)


class Problem(object):
	"""Hold all the problem's data"""
	def __init__(self, name, prefix):
		super(Problem, self).__init__()
		self.name = name
		self.prefix = prefix

		self.data = []
		self.models = []
		self.results = []
		self.specification = None
		self.metadata = {}
		self.base_path = path.join(self.prefix, self.name)

	def __lt__(self, other):
		return (self.prefix, self.name) < (other.prefix, other.name)

	def __repr__(self):
		return "%s(%s)" % (self.__class__, pformat(self.__dict__))

	def find_files(self):

		spec = path.join(self.base_path, "specification.md")
		if path.exists(spec):
			self.specification = spec

		self.bib = None
		self.ref_notes=None
		ref = path.join(self.base_path, "references", "references.bib")
		ref_notes = path.join(self.base_path, "references", "notes.inline.md")

		if path.exists(ref):
			self.bib = bibtex.Bib(ref)

		if path.exists(ref_notes):
			self.ref_notes = ref_notes

		self.models = self.get_directory("models")
		self.data = self.get_directory("data")
		self.results = self.get_directory("results")

	def get_directory(self, name):
		dirr = path.join(self.base_path, name)
		if path.exists(dirr):
			return [path.join(dirr, f) for f in os.listdir(dirr) if f[0] != '.' and path.splitext(f)[1] != ".metadata"]
		return []

	def is_vaild(self):
		return self.specification is not None


def process_problem(prob, apply_template, output_dir, base):
	"Creates the problem's html"

	(content, metadata) = convert_markdown(prob.specification)
	if not "category" in metadata:
		metadata['category'] = ['Unclassified']
	else:
		metadata['category'] = [ m for m in metadata['category'] if m ]
		if len(metadata['category']) == 0:
			metadata['category'] = ['Unclassified']

	metadata['id'] = [prob.name[4:7]]
	prob.metadata = metadata

	title = " ".join(metadata['id']) + ": " + " ".join(metadata['title'])
	prob_meta = {"title": title, "prob_base": "/Problems/" + prob.name, "prob_name": prob.name, "prob": prob}

	spec = apply_template("problem.html", problemContent=content, type="specification", rel_path='specification.md', **prob_meta)
	prob_dir = path.join(output_dir, "Problems/{0}".format(prob.name))
	makedirs_exist_ok(prob_dir)

	def write(data, name):
		with open(path.join(prob_dir, name), "w", encoding='utf-8') as f:
			f.write(data)

	write(spec, "index.html")

	def problem_part(part_name):
		part_metadata = []
		part_dir = prob_dir + "/" + part_name + "/"
		makedirs_exist_ok(part_dir)

		raw_htmls = []
		for part in getattr(prob, part_name):
			fp = path.join(prob_meta['prob_base'], part_name)

			if (path.splitext(part)[1] == '.inline.html'):
				with open(part, encoding='utf-8'    ) as f:
					raw_html = f.read()
				raw_htmls.append(raw_html.strip())
				continue
			elif (path.splitext(part)[1] == '.inline.md'):
				(html, _) = convert_markdown(part)
				raw_htmls.append(html.strip())
				continue


			(content, metadata, url) = get_content_and_metadata(part, fp)
			logger.debug( (part, metadata, content[0:5]))
			if not url:
				name = path.basename(part)
				filename = name + ".html"
				res = apply_template("file.html", problemContent=content,
					name=name, part=part_name, rel_path="{0}/{1}".format(part_name, name),
					**prob_meta)
				write(res, part_name + "/" + filename)
				file_util.copy_file(part, path.join(part_dir, name))
			else:
				filename = url
				name = path.basename(filename)
				file_util.copy_file(part, path.join(part_dir, name))

			part_metadata.append({"name": name, "filename": filename, "meta": metadata})

		template = apply_template(part_name + ".html", metadata=part_metadata, rel_path=part_name,
									raw_htmls=raw_htmls, **prob_meta)

		write(template, part_name + "/index.html")

	problem_part("results")
	problem_part("data")
	problem_part("models")


	# Copying assets bindly
	prob_dir_in = path.join(base, "Problems/{0}".format(prob.name))
	assets_in = path.join(prob_dir_in, "assets")
	assets_out = path.join(prob_dir, "assets")

	if path.exists(assets_in):
		logger.debug("Copying assets from %s to %s", assets_in, assets_out )
		dir_util.copy_tree(assets_in, assets_out)


	has_bibtex=None
	bib_html=""
	ref_notes_html=""
	if prob.bib:
		makedirs_exist_ok(path.join(prob_dir, "references"))
		file_util.copy_file(prob.bib.bibfile, path.join(prob_dir, "references",  prob.name +"-refs.bib"))
		has_bibtex = True
		bib_html = prob.bib.to_html(apply_template)

	if prob.ref_notes:
		(ref_notes_html, _) = convert_markdown(prob.ref_notes)

	refs = apply_template("references.html", references=bib_html, rel_path="references.html",
		has_bibtex=has_bibtex, notes=ref_notes_html, **prob_meta)
	makedirs_exist_ok(path.join(prob_dir, "references"))
	write(refs, "references/index.html")

	# Cite a problem
	# pprint(prob_meta)
	cite = apply_template("problem_cite.html", **prob_meta)
	makedirs_exist_ok(path.join(prob_dir, "cite"))
	write(cite, "cite/index.html")


	old_path = path.join(output_dir, "prob/{0}".format(prob.name))
	makedirs_exist_ok(old_path)
	with open(path.join(old_path, "index.html"), "w", encoding='utf-8') as f:
		f.write(apply_template("redirect.html", url="/Problems/%s" % prob.name))
