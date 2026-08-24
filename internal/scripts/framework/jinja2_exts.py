# -*- coding: utf-8 -*-
# modfied  filter to handle local links

import re
import os.path as path

from datetime import date

from markupsafe import escape
from util import convert_markdown_fragment

def init_exts(template_env):
	template_env.filters['formatted_time_for_updates'] = formatted_time_for_updates
	template_env.filters['to_markdown'] = convert_markdown_fragment

def formatted_time_for_updates(year_month):
	d = date(*year_month, day=1)
	return d.strftime("%B %Y")
