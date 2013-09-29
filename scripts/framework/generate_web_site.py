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


problems_path = path.join(base, "problems")

probs_names = [f for f in os.listdir(problems_path) if path.isdir(path.join(problems_path, f))]
probs = [p for p in [create_problem(p, problems_path) for p in probs_names] if p.is_vaild()]

print(probs)

copy_web_resources(output_dir)

markdown_exts = ['extra', 'meta', 'sane_lists']
template_env = Environment(loader=FileSystemLoader(templates_dir))


def convert_markdown(page_path):
	md = markdown.Markdown(extensions=markdown_exts)
	with open(page_path) as f:
		md_input = "".join(f.readlines())
	page = md.convert(md_input)
	metadata = md.Meta
	return (page, metadata)


def apply_template(template_name, **kwargs):
	template = template_env.get_template('problem.html')
	return template.render(kwargs)


def process_problem(prob):
	(content, metadata) = convert_markdown(prob.specification)
	title = " ".join(metadata['id']) + ": " + " ".join(metadata['title'])
	res = apply_template("problem.html", title=title, problemContent=content)

	prob_dir = path.join(output_dir, "prob/{}".format(prob.name))
	os.makedirs(prob_dir)
	with open(path.join(prob_dir, "index.html"), "w") as f:
		f.write(res)

for prob in probs:
	process_problem(prob)
