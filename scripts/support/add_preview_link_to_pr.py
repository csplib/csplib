#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Add build url to pull request

from github3 import login
import os
import sys

token=os.getenv('CSPLIB_ROBOT_TOKEN')
gh = login(token=token)
if not gh:
	print("error logging in")
	sys.exit(3)

issue_num=os.getenv('TRAVIS_PULL_REQUEST')
place="PR-{}".format(issue_num)
text="Build preview located at http://csplib.github.io/csplib-PR-builds"

pr = gh.issue('csplib', 'csplib', issue_num)
already_added = any( i for i in pr.iter_comments() 
		if text in i.to_json()['body'] )

if not already_added:
	pr.create_comment("%s/%s\n This will be automatic updated if more commits are added."
		% (text, place))
	print("Added url to pull request")
else:
	print("already added url to pull request")

