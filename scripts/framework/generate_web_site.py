#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

from __future__ import print_function
import sys

if sys.version_info[0] == 2:
	if sys.version_info[1] < 6:
		print('Only python 2.6+ supported', file=sys.stderr)
		sys.exit(1)

	# Hack to get unicode to work properly in python2
	reload(sys)
	sys.setdefaultencoding('utf-8')

from collections import defaultdict
from datetime import datetime, date
from distutils import dir_util, file_util

from jinja2 import Environment, FileSystemLoader
from jinja2_exts import urlize2

import argparse
import cgi  # for cgi.escape
import logging
import os, os.path as path
import zipfile

import bibtex
import markdown

from pprint import pprint, pformat

logger = logging.getLogger(__name__)



# Option parser
parser = argparse.ArgumentParser(description='Builds csplib')
parser.add_argument("only_probs", nargs='*', metavar='Problems', help='Optional, Build only the specifed problems')
parser.add_argument("--debug",  action='store_true', help='Print debug output')
args = parser.parse_args()

# set up logging
if args.debug:
	logger_format='%(name)s:%(lineno)d:%(funcName)s: %(message)s'
	logger_level = logging.DEBUG
else:
	logger_format='%(message)s'
	logger_level = logging.INFO

logging.basicConfig(format=logger_format, level=logger_level)
logger.info("args %s", args)

# Where we are
prog_name = path.dirname(sys.argv[0])
abs_prog_dir = path.abspath(prog_name)

# Paths
base = path.dirname(path.dirname(abs_prog_dir)) + "/"
templates_dir = path.join(base, "templates")
output_dir = path.join(base, "_deploy")

logger.info("Base:%s", base)
logger.info("Output:%s", output_dir)

# Problem paths
problems_path = path.join(base, "Problems")
probs_names = set(f for f in os.listdir(problems_path) if path.isdir(path.join(problems_path, f)))

# If args are given, only build the specifed problems
if args.only_probs:
	to_build = set(args.only_probs)
	probs_names = probs_names & to_build


markdown_exts = ['extra', 'meta', 'sane_lists', 'tables', 'smartypants(entities=named)', 'cite_bibtex']
template_env = Environment(loader=FileSystemLoader(templates_dir), trim_blocks=True, lstrip_blocks=True)
template_env.filters['urlize2'] = urlize2


def formatted_time_for_updates(year_month):
	d = date(*year_month, day=1)
	return d.strftime("%B %Y")


template_env.filters['formatted_time_for_updates'] = formatted_time_for_updates
bibtex.add_filters(template_env)


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

	def __repr__(self):
		return "%s(%s)" % (self.__class__, pformat(self.__dict__))

	def find_files(self):

		spec = path.join(self.base_path, "specification.md")
		if path.exists(spec):
			self.specification = spec

		self.bib = None
		self.ref_notes=None
		ref = path.join(self.base_path, "references.bib")
		ref_notes = path.join(self.base_path, "references-notes.inline-md")

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


def create_problem(name, path):
	prob = Problem(name, path)
	prob.find_files()
	return prob


def copy_web_resources(output_dir):
	dir_util.copy_tree(path.join(base, "web"), output_dir)

probs = [p for p in [create_problem(p, problems_path) for p in probs_names] if p.is_vaild()]

# Copy every file in web  to the output directory
copy_web_resources(output_dir)


def read_file(filepath):
	with open(filepath, "r") as f:
		return "".join(f.readlines() + ["\n"])


# since exist_ok is not in python2
def makedirs_exist_ok(path):
	try:
		os.makedirs(path)
	except OSError:
		if not os.path.isdir(path):
			raise


def convert_markdown(page_path):
	md = markdown.Markdown(extensions=markdown_exts)
	md_input = read_file(page_path)
	page = md.convert(md_input)
	metadata = md.Meta
	return (page, metadata)


def apply_template(template_name, **kwargs):
	template = template_env.get_template(template_name)
	return template.render(kwargs)


def process_problem(prob):
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

			if (path.splitext(part)[1] == '.inline-html'):
				with open(part, encoding='utf-8'    ) as f:
					raw_html = f.read()
				raw_htmls.append(raw_html.strip())
				continue
			elif (path.splitext(part)[1] == '.inline-md'):
				(html, _) = convert_markdown(part)
				raw_htmls.append(html.strip())
				continue


			(content, metadata, url) = get_content_and_metadata(part, fp)
			logger.debug( (part, metadata, content[0:5]))
			if not url:
				name = path.basename(part)
				filename = path.splitext(name)[0] + ".html"
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

	old_path = path.join(output_dir, "prob/{0}".format(prob.name))
	makedirs_exist_ok(old_path)
	with open(path.join(old_path, "index.html"), "w", encoding='utf-8') as f:
		f.write(apply_template("redirect.html", url="/Problems/%s" % prob.name))


source_types = set(['cc', 'c', 'java', 'cpp', 'cs', 'eprime', 'erl', 'essence',
				'groovy', 'h', 'hh', 'hpp', 'hrl', 'javascript', 'js',
				'param', 'php', 'pl', 'py', 'rb', 'scala', 'solution', 'xml'])

text_formats = set(['txt', 'minizinc', 'hs', 'lhs', 'lisp', 'cnf', 'ecl', 'egenet', 'chip'])
text_formats |= source_types


def get_content_and_metadata(filepath, store_dir):
	(_, ext) = path.splitext(filepath)
	if (ext == ".md"):
		(a, b) = convert_markdown(filepath)
		return (a, b, None)
	elif (ext == '.html'):
		return (read_file(filepath), None, None)

	meta_path = filepath + ".metadata"
	try:
		(_, meta) = convert_markdown(meta_path)
	except Exception:
		meta = None

	print
	if ext[1:] in text_formats:
		css_class = ""
		txt = read_file(filepath)
		if ext[1:] in source_types:
			css_class = "class ='brush: {0}'".format(ext[1:])
			txt = cgi.escape(txt)

		return ("<pre {0}>{1}</pre>".format(css_class, txt), meta, None)
	else:
		bname = path.basename(filepath)
		url = path.join(store_dir, bname)
		return ("<a href='{0}'> {1} </a>".format(url, bname), meta, url)


essences = []
categories_map = defaultdict(list)
authors_map = defaultdict(list)
months_map = defaultdict(list)

try:
	# get creation times from git
	with open(path.join(output_dir, "problems_creation_dates.txt"), "r", encoding='utf-8') as f:
		creations_times = dict(line.strip().split(',') for line in f.readlines())
	logger.debug(creations_times)
except IOError:
	logger.warning("no creation times, updates pages will be empty")
	creations_times={}

for prob in probs:
	try:
		logger.debug("")
		logger.debug("Processing problem %s", prob.name)
		logger.debug(prob)
		logger.debug("")
		process_problem(prob)
		for category in prob.metadata['category']:
			categories_map[category].append(prob)

		for author in prob.metadata['proposer']:
			authors_map[author].append(prob)

		def fix_path(f):
			"""filepath inside zip"""
			return f.replace(problems_path + "/", "").replace("/models", "")

		essences += [(f, fix_path(f)) for f in prob.models if path.splitext(f)[1] == '.essence' ]

		if prob.name in creations_times:
			if creations_times[prob.name].strip():
				creation = datetime.fromtimestamp(float(creations_times[prob.name]))
				months_map[(creation.year, creation.month)].append( (creation, prob) )
	except Exception as e:
		logger.info("Failure in problem %s", prob.name)
		logger.info("Error: %s", e)
		raise

logger.debug("authors %s", authors_map.keys())


def create_zip_file(create_path, files):
	""" creates a zip file with the specified (src,dst) """
	zf = zipfile.ZipFile(create_path, "w")
	for (src, dst) in files:
		zf.write(src, dst)
	zf.close()

create_zip_file(path.join(output_dir, "essences.zip"), essences)

# index page
index_path = path.join(output_dir, "index.html")
res = apply_template("index.html",
	num_problems=len(probs), num_categories=len(categories_map), num_authors=len(authors_map))
with open(index_path, "w", encoding='utf-8') as f:
	f.write(res)

probs_path = path.join(output_dir, "Problems/index.html")
res = apply_template("problems.html", problems=sorted(probs, key=lambda x: x.metadata["id"]))
with open(probs_path, "w", encoding='utf-8') as f:
	f.write(res)

probs_path = path.join(output_dir, "Problems/categories.html")
res = apply_template("categories.html", categories=categories_map)
with open(probs_path, "w", encoding='utf-8') as f:
	f.write(res)

probs_path = path.join(output_dir, "authors.html")
res = apply_template("authors.html", authors=authors_map)
with open(probs_path, "w", encoding='utf-8') as f:
	f.write(res)

probs_path = path.join(output_dir, "updates.html")
res = apply_template("updates.html", mapping=months_map)
with open(probs_path, "w", encoding='utf-8') as f:
	f.write(res)
