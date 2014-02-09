
build: ./scripts/env creation_dates
	# activate virtual python Environment
	. ./scripts/env/bin/activate && \
	./scripts/framework/generate_web_site.py
	# built website

buildpy2: py2setup creation_dates
	python ./scripts/framework/generate_web_site.py
	# built website

.PHONY: creation_dates
creation_dates:
	./scripts/problem_creation_dates.sh

./scripts/env:
	# create virtual python Environment
	./scripts/setup.sh

.PHONY: py2setup
py2setup:
	./scripts/setuppy2.sh

serve:
	# open localhost:8000 in your browser
	# Crtl-C to finish
	cd _deploy && python3 -m http.server

servepy2:
	# open localhost:8000 in your browser
	# Crtl-C to finish
	cd _deploy && python -m SimpleHTTPServer