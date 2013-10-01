#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import os
import os.path as path

from jinja2 import Environment, FileSystemLoader

import markdown

from distutils import dir_util
import subprocess
import sys

import re

#Where we are
prog_name = path.dirname(sys.argv[0])
abs_prog_dir = path.abspath(prog_name)

base = path.dirname(path.dirname(abs_prog_dir)) + "/"
templates_dir = path.join(base, "templates")
#output_dir = "/Users/bilalh/Sites"
output_dir = path.join(base, "_deploy")

print("Base:{}", base)
print("Output:{}", output_dir)


class Problem(object):
	"""Hold all the problem data"""
	def __init__(self, name, prefix):
		super(Problem, self).__init__()
		self.name = name
		self.prefix = prefix

		self.data = []
		self.models = []
		self.results = []
		self.specification = None
		self.metadata = {}

	def __repr__(self):
		return "%s(%s)" % (self.__class__, self.__dict__)

	def find_files(self):
		base_path = path.join(self.prefix, self.name)

		spec = path.join(base_path, "specification.md")
		if path.exists(spec):
			self.specification = spec

		refs = path.join(base_path, "references.bib")
		if path.exists(refs):
			self.references = refs

		self.models = self.get_directory(base_path, "models")
		self.data = self.get_directory(base_path, "data")
		self.results = self.get_directory(base_path, "results")

	def get_directory(self, base_path, name):
		dirr = path.join(base_path, name)
		if path.exists(dirr):
			return [path.join(dirr, f) for f in os.listdir(dirr) if not f[0] == '.']
		return []

	def is_vaild(self):
		return self.specification is not None


def create_problem(name, path):
	prob = Problem(name, path)
	prob.find_files()
	return prob


def copy_web_resources(output_dir):
	dir_util.copy_tree(path.join(base, "web"), output_dir)

# Read the data
problems_path = path.join(base, "problems")

probs_names = [f for f in os.listdir(problems_path) if path.isdir(path.join(problems_path, f))]
probs = [p for p in [create_problem(p, problems_path) for p in probs_names] if p.is_vaild()]

copy_web_resources(output_dir)

markdown_exts = ['extra', 'meta', 'sane_lists']
template_env = Environment(loader=FileSystemLoader(templates_dir))


def read_file(filepath):
	with open(filepath, "r") as f:
		return "".join(f.readlines())


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
	prob.metadata = metadata

	title = " ".join(metadata['id']) + ": " + " ".join(metadata['title'])
	prob_meta = {"title": title, "prob_base": "/Problems/" + prob.name}

	spec = apply_template("problem.html", problemContent=content, type="specification", **prob_meta)
	prob_dir = path.join(output_dir, "Problems/{}".format(prob.name))
	os.makedirs(prob_dir, exist_ok=True)

	def write(data, name):
		with open(path.join(prob_dir, name), "w") as f:
			f.write(data)

	write(spec, "index.html")

	def problem_part(part_name):
		part_metadata = []
		os.makedirs(prob_dir + "/" + part_name + "/", exist_ok=True)
		for part in prob.__dict__[part_name]:
			(content, metadata) = get_content_and_metadata(part)
			name = path.basename(part)
			filename = path.splitext(name)[0] + ".html"
			res = apply_template("file.html", problemContent=content, name=name, part=part_name, **prob_meta)
			write(res, part_name + "/" + filename)
			part_metadata.append({"name": name, "filename": filename})

		write(apply_template(part_name + ".html", metadata=part_metadata, **prob_meta), part_name + "/index.html")

	problem_part("results")
	problem_part("data")
	problem_part("models")
	bib_html = get_bib_references(prob.references)
	refs = apply_template("references.html", references=bib_html, **prob_meta)
	os.makedirs(path.join(prob_dir, "references"), exist_ok=True)
	write(refs, "references/index.html")


def get_content_and_metadata(filepath):
	(_, ext) = path.splitext(filepath)
	if (ext == ".md"):
		return convert_markdown(filepath)
	else:
		return ("<pre>{}</pre>".format(read_file(filepath)), None)


def get_bib_references(filepath):
	bib_cmd = [path.join(abs_prog_dir, "make_bibtex_html.sh"), filepath]
	bib_html = subprocess.check_output(bib_cmd, universal_newlines=True)
	# easier then using a html parser
	regex = re.compile(r"<p>.*?</p>", re.DOTALL)
	return "\n".join(regex.findall(bib_html))


for prob in probs:
	print(prob)
	process_problem(prob)


# index page
index_path = path.join(output_dir, "index.html")
res = apply_template("index.html")
with open(index_path, "w") as f:
	f.write(res)

probs_path = path.join(output_dir, "Problems/index.html")
res = apply_template("problems.html", problems=probs)
with open(probs_path, "w") as f:
	f.write(res)
