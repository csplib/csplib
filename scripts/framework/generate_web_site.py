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


import argparse
import logging
import os, os.path as path

import jinja2_exts
import bibtex
import problem
import language

from collections import defaultdict
from datetime import datetime
from distutils import dir_util
from jinja2 import Environment, FileSystemLoader

from problem import Problem
from language import Language
from util import create_zip_file

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

# languages paths
language_path = path.join(base, "Programs_and_Languages")
lang_names = set(f for f in os.listdir(language_path) if path.isdir(path.join(language_path, f)))


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
	return template.render(kwargs)


# Copy every file in web to the output directory
dir_util.copy_tree(path.join(base, "web"), output_dir)


def create_problem(name, path):
	prob = Problem(name, path)
	prob.find_files()
	return prob

# Init problems, and peform some validation
probs = [p for p in [create_problem(p, problems_path) for p in probs_names] if p.is_vaild()]


def create_language(name, path):
	prob = Language(name, path)
	prob.find_files()
	return prob

# Init problems, and peform some validation
langs = [p for p in [create_language(p, language_path) for p in lang_names] if p.is_vaild()]


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


# Creates the output for the problems
for prob in sorted(probs):
	try:
		logger.debug("")
		logger.debug("Processing problem %s", prob.name)
		logger.debug(prob)
		logger.debug("")
		problem.process_problem(prob, apply_template, output_dir, base)
		for category in prob.metadata['category']:
			categories_map[category].append(prob)

		for author in prob.metadata['proposer']:
			authors_map[author].append(prob)

		def fix_path(fp):
			"""filepath inside zip"""
			return fp.replace(problems_path + "/", "").replace("/models", "")

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

# Creates the output for the problems
for prob in sorted(langs):
	try:
		logger.debug("")
		logger.debug("Processing lang %s", prob.name)
		logger.debug(prob)
		logger.debug("")
		language.process_language(prob, apply_template, output_dir, base)

	except Exception as e:
		logger.info("Failure in language %s", prob.name)
		logger.info("Error: %s", e)
		raise


# Other standalone pages



probs_path = path.join(output_dir, "Problems/index.html")
res = apply_template("problems.html", problems=sorted(probs, key=lambda x: x.metadata["id"]))
with open(probs_path, "w", encoding='utf-8') as f:
	f.write(res)

langs_path = path.join(output_dir, "Languages/index.html")
res = apply_template("problems.html", problems=sorted(langs, key=lambda x: x.metadata["id"]))
with open(langs_path, "w", encoding='utf-8') as f:
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

index_path = path.join(output_dir, "index.html")
res = apply_template("index.html",
	num_problems=len(probs), num_categories=len(categories_map), num_authors=len(authors_map))
with open(index_path, "w", encoding='utf-8') as f:
	f.write(res)


# Other data
create_zip_file(path.join(output_dir, "essences.zip"), essences)

