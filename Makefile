
build: ./scripts/env creation_dates
	# activate virtual python Environment
	. ./scripts/env/bin/activate && \
	./scripts/framework/generate_web_site.py
	# built website

.PHONY: creation_dates
creation_dates:
	./scripts/support/problem_creation_dates.sh

./scripts/env:
	# create virtual python Environment
	./scripts/support/setup.sh
serve:
	# open localhost:8000 in your browser
	# Crtl-C to finish
	cd _deploy && python3 -m http.server

# Build specific problems and serve locally
.PHONY: only
only: ./scripts/env creation_dates only_build serve

# to use a comma in $(subst you need to put it in a varible
comma :=,
only_build:
	. ./scripts/env/bin/activate && \
	./scripts/framework/generate_web_site.py $(subst ${comma}, ,${probs})

clean:
	rm -rf _deploy

purge:
	rm -rf _deploy
	rm -rf scripts/env
