

build: ./internal/scripts/env creation_dates
	# activate virtual python environment
	. ./internal/scripts/env/bin/activate && \
	./internal/scripts/framework/generate_web_site.py
	# built website

.PHONY: creation_dates
creation_dates:
	./internal/scripts/support/problem_creation_dates.sh

./internal/scripts/env:
	# create virtual python Environment
	./internal/scripts/support/setup.sh

serve:
	# open http://localhost:8000 in your browser
	# Crtl-C to finish
	cd _deploy && python3 -m http.server


build_for_gh_pages: ./internal/scripts/env
	./internal/scripts/support/build_for_gh_pages.sh
	# built website




# Build specific problems and serve locally
.PHONY: only
only: ./internal/scripts/env creation_dates only_build serve

# to use a comma in $(subst you need to put it in a variable
comma :=,
only_build:
	. ./internal/scripts/env/bin/activate && \
	./internal/scripts/framework/generate_web_site.py $(subst ${comma}, ,${build})

debug:
	. ./internal/scripts/env/bin/activate && \
	ipython --pdb ./internal/scripts/framework/generate_web_site.py -- --debug $(subst ${comma}, ,${build})

clean:
	rm -rf _deploy
	rm -rf internal/scripts/env

docker:
	docker build . --tag csplib
	bash -c 'docker run --user `id -u`:`id -g` -v `pwd`:/dir csplib sh -c "cd /dir;make"'