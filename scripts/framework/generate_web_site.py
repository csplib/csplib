#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

from __future__ import print_function
import sys

from copy import deepcopy

if sys.version_info[0] == 2:
	if sys.version_info[1] < 6:
		print('Only python 2.6+ supported', file=sys.stderr)
		sys.exit(1)

	# Hack to get unicode to work properly in python2
	reload(sys)
	sys.setdefaultencoding('utf-8')


import argparse
import logging
import os, os.path as path

import jinja2_exts
import bibtex
import problem

from collections import defaultdict
from datetime import datetime
from distutils import dir_util
from jinja2 import Environment, FileSystemLoader

from problem import Problem, PageType
from util import create_zip_file, makedirs_exist_ok

logger = logging.getLogger(__name__)


# Option parser
parser = argparse.ArgumentParser(description='Builds csplib')
parser.add_argument("only_probs", nargs='*', metavar='Problems', help='Optional, Build only the specifed problems')
parser.add_argument("--debug",  action='store_true', help='Print debug output')
parser.add_argument("--prefix_path", help='The prefix to prepend to all urls, useful for github pages')
parser.add_argument("--output_suffix", help='The suffix to append the output_dir, useful for github pages')
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

if args.prefix_path:
	prefix_path=args.prefix_path
else:
	prefix_path=""

if args.output_suffix:
	output_dir = path.join(base, "_deploy", args.output_suffix)
else:
	output_dir = path.join(base, "_deploy")

logger.info("Base:%s", base)
logger.info("Output:%s", output_dir)

# Problem paths
problems_path = path.join(base, "Problems")
probs_names = set(f for f in os.listdir(problems_path) if path.isdir(path.join(problems_path, f)))

languages_path = path.join(base, "Languages")
langs_names = set(f for f in os.listdir(languages_path) if path.isdir(path.join(languages_path, f)))

# If args are given, only build the specifed problems
if args.only_probs:
	to_build = set(args.only_probs)
	probs_names = probs_names & to_build

# Setup up templates
template_env = Environment(loader=FileSystemLoader(templates_dir), trim_blocks=True, lstrip_blocks=True)
jinja2_exts.init_exts(template_env)
bibtex.add_filters(template_env)


def apply_template(template_name, **kwargs):
	template = template_env.get_template(template_name)
	return template.render(kwargs, prefix_path=prefix_path)


# Copy every file in web to the output directory
dir_util.copy_tree(path.join(base, "web"), output_dir)


def create_problem(name, path, pagetype):
	prob = Problem(name, path, pagetype)
	prob.find_files()
	return prob

# Init problems, and peform some validation
probs = [p for p in [create_problem(p, problems_path, PageType.PROBLEM) for p in probs_names] if p.is_vaild()]
langs = [p for p in [create_problem(p, languages_path, PageType.LANGUAGE) for p in langs_names] if p.is_vaild()]

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


def generate_pages(pages):
	# Creates the output for the problems
	for page in sorted(pages):
		try:
			logger.debug("")
			logger.debug("Processing page %s", page.name)
			logger.debug(page)
			logger.debug("")
			problem.process_problem(page, apply_template, output_dir, base)

			for category in page.metadata.get('category', []):
				categories_map[category].append(page)

			for author in page.metadata.get('proposer', []):
				authors_map[author].append(page)

			def fix_path(fp):
				"""filepath inside zip"""
				return fp.replace(problems_path + "/", "").replace("/models", "")

			global essences
			essences += [(f, fix_path(f)) for f in page.models if path.splitext(f)[1] == '.essence' ]

			if page.name in creations_times:
				if creations_times[page.name].strip():
					creation = datetime.fromtimestamp(float(creations_times[page.name]))
					months_map[(creation.year, creation.month)].append( (creation, page) )

		except Exception as e:
			logger.info("Failure in page %s", page.name)
			logger.info("Error: %s", e)
			raise

def write_pages(pages):
	for page in sorted(pages):
		try:
			logger.debug("")
			logger.debug("Writing page %s", page.name)
			logger.debug(page)
			logger.debug("")
			problem.write_problem(page, apply_template, output_dir, base)

		except Exception as e:
			logger.info("Failure in page %s", page.name)
			logger.info("Error: %s", e)
			raise

# Detects if a particular model uses a given language (given as a Problem)
def model_uses_language(model, language):
	for type in model['meta']['type']:
		if type in language.metadata['title']:
			return True
		# Now try to match the file extension
		for ext in language.metadata['extensions']:
			if type == ext:
				model['meta']['type'] = language.metadata['title']
				return True
	return False


generate_pages(probs)
generate_pages(langs)

for p in probs:
	for pages in ['models','data']:
		for model in p.parts[pages]:
			for l in langs:
				if model_uses_language(model, l):
					model['meta']['type_link'] = "../../../"+l.prob_meta['prob_base']
					clone = deepcopy(model)
					clone['filename'] = "../../../" + p.prob_meta['prob_base'] + "/models/" + clone['filename']
					clone['meta']['type'] = [p.prob_meta['title']]
					clone['meta']['type_link'] = "../../../"+p.prob_meta['prob_base']
					l.parts[pages].append(clone)
					break

write_pages(probs)
write_pages(langs)

for prob in probs:
	old_path = path.join(output_dir, "prob/{0}".format(prob.name))
	makedirs_exist_ok(old_path)
	with open(path.join(old_path, "index.html"), "w", encoding='utf-8') as f:
		f.write(apply_template("redirect.html", url="/Problems/%s" % prob.name))


logger.debug("authors %s", authors_map.keys())



# Other standalone pages

index_path = path.join(output_dir, "index.html")
res = apply_template("index.html",
	num_problems=len(probs), num_categories=len(categories_map), num_authors=len(authors_map))
with open(index_path, "w", encoding='utf-8') as f:
	f.write(res)

probs_path = path.join(output_dir, "Problems/index.html")
res = apply_template("problems.html", problems=sorted(probs, key=lambda x: x.metadata["id"]))
with open(probs_path, "w", encoding='utf-8') as f:
	f.write(res)

probs_path = path.join(output_dir, "Languages/index.html")
res = apply_template("languages.html", problems=sorted(langs, key=lambda x: x.metadata["id"]))
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


# Other data
create_zip_file(path.join(output_dir, "essences.zip"), essences)
