# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import cgi	# for cgi.escape
import markdown
import os, os.path as path
import zipfile

import logging
logger = logging.getLogger(__name__)

markdown_exts = ['extra', 'yaml_front_matter', 'sane_lists', 'tables', 'smartypants(entities=named)', 'cite_bibtex']

source_types = set(['cc', 'c', 'java', 'cpp', 'cs', 'eprime', 'erl', 'essence',
				'groovy', 'h', 'hh', 'hpp', 'hrl', 'javascript', 'js', 'minizinc',
				'param', 'php', 'pl', 'py', 'rb', 'scala', 'solution', 'xml'])

text_formats = set(['txt', 'hs', 'lhs', 'lisp', 'cnf', 'ecl', 'egenet',
					'chip', 'mzn', 'pi', 'pl', 'co', 'comet'])
text_formats |= source_types

binary_formats = set(['ps', 'pdf'])
archive_formats = set(['zip', 'tar', 'tar.gz', 'rar', '7z', 'xz', 'sit', 'sitx',
	 					'iso', 'bz2', 'lz', 'gz', 'lzma', 'lzo', 'z', 'Z', 'ace',
						'jar', 'pea', 'tarz', 'tar.bz2', 'tbz2', 'tlz', 'xar',
						'zipx', 'zz', 'zpaq'])

# Mapping file type which are actually source files (all lowercase)
source_mapping = {
	"ilog solver": 'cpp'
}

def convert_markdown(page_path):
	md = markdown.Markdown(extensions=markdown_exts)
	md_input = read_file(page_path)
	page = md.convert(md_input)
	if hasattr(md, 'Meta'):
		return (page, md.Meta)
	else:
		return (page, dict())


def read_file(filepath):
	with open(filepath, "r") as f:
		return "".join(f.readlines() + ["\n"])


def get_content_and_metadata(filepath, store_dir):
	(_, ext) = path.splitext(filepath)
	if (ext == ".md"):
		(a, b) = convert_markdown(filepath)
		if not('type' in b):
			if 'Type' in b:
				b['type'] = b['Type']
				del b['Type']
			else:
				b['type'] = [ext[1:]]

		return (a, b, None)
	elif (ext == '.html'):
		return (read_file(filepath), None, None)

	meta = dict()

	meta_path = filepath + ".metadata"
	try:
		(_, meta) = convert_markdown(meta_path)
	except Exception:
		pass

	# Add the language
	if not('type' in meta):
		if 'Type' in meta:
			meta['type'] = meta['Type']
			del meta['Type']
		else:
			meta['type'] = [ext[1:]]

	stype = meta['type'][0].lower()
	if stype in source_mapping:
		stype = source_mapping[stype]
	logger.debug("stype:%s ext:%s filepath:%s", stype, ext, filepath)
	# This if is necessary because Essence files stored in a zip will have
	# stype=Essence, ext[1:]=zip
	if ext[1:] not in archive_formats and ext[1:] not in binary_formats:
		css_class = ""
		txt = read_file(filepath)
		if stype in source_types:
			css_class = "class ='brush: {0}'".format(stype)
			txt = cgi.escape(txt)

		return ("<pre {0}>{1}</pre>".format(css_class, txt), meta, None)
	else:
		bname = path.basename(filepath)
		url = path.join(store_dir, bname)
		return ("<a href='{0}'> {1} </a>".format(url, bname), meta, url)


def create_zip_file(create_path, files):
	""" creates a zip file with the specified (src,dst) """
	zf = zipfile.ZipFile(create_path, "w")
	for (src, dst) in files:
		zf.write(src, dst)
	zf.close()


# since exist_ok is not in python2
def makedirs_exist_ok(path):
	try:
		os.makedirs(path)
	except OSError:
		if not os.path.isdir(path):
			raise
