#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import os
import os.path as path

from jinja2 import Environment, FileSystemLoader

import markdown

from distutils import dir_util

# config
base = "/Users/bilalh/CS/csplib/"
templates_dir = path.join(base, "templates")
output_dir = "/Users/bilalh/Sites"


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
	prob_meta = {"title": title, "prob_base": "/prob/" + prob.name}

	spec = apply_template("problem.html", problemContent=content, type="specification", **prob_meta)
	prob_dir = path.join(output_dir, "prob/{}".format(prob.name))
	os.makedirs(prob_dir, exist_ok=True)

	def write(data, name):
		with open(path.join(prob_dir, name), "w") as f:
			f.write(data)

	write(spec, "index.html")

	results_metadata = []
	os.makedirs(prob_dir + "/results/", exist_ok=True)
	for result in prob.results:
		(content, metadata) = convert_markdown(result)
		name = path.basename(result)
		filename = path.splitext(name)[0] + ".html"
		res = apply_template("problem.html", problemContent=content, **prob_meta)
		write(res, "results/" + filename)
		results_metadata.append({"name": name, "filename": filename})

	write(apply_template("results.html", results=results_metadata, **prob_meta), "results/index.html")


for prob in probs:
	print(prob)
	process_problem(prob)


# index page
index_path = path.join(output_dir, "index.html")
res = apply_template("index.html")
with open(index_path, "w") as f:
	f.write(res)

probs_path = path.join(output_dir, "problems.html")
res = apply_template("problems.html", problems=probs)
with open(probs_path, "w") as f:
	f.write(res)
