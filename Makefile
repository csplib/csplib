
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


# Build specific problems and serve locally
.PHONY: only
only: ./scripts/env creation_dates only_build serve

# to use a comma in $(subst you need to put it in a varible
comma :=,
only_build:
	. ./scripts/env/bin/activate && \
	./scripts/framework/generate_web_site.py $(subst ${comma}, ,${probs})