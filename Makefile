
build: ./scripts/env
	# activate virtual python Environment
	. ./scripts/env/bin/activate && \
	./scripts/framework/generate_web_site.py
	# built website
	
./scripts/env: 
	# create virtual python Environment
	./scripts/setup.sh

serve: 
	# open localhost:8000 in your browser
	# Crtl-C to finish
	cd _deploy && python3 -m http.server
	