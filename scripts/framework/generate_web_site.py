#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import os
import os.path as path

from jinja2 import Environment, FileSystemLoader

base = "/Users/bilalh/CS/csplib/"
templates_dir = path.join(base, "web/templates")
ouput_dir = "/Users/bilalh/CS/csplib/web"


def process_prob(name):
	return "d"

problems = path.join(base, "problems")

probs_names = [f for f in os.listdir(problems) if path.isdir(path.join(problems, f))]
probs = [process_prob(p) for p in probs_names]

print(probs)


env = Environment(loader=FileSystemLoader(templates_dir))
template = env.get_template('problem.html')

res = template.render(problemContent="<p> Test  </p>", title="prob001")


with open(path.join(ouput_dir, "prob001.html"), "w") as f:
	f.write(res)
