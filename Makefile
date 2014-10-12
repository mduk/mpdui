all:: build

static = ./priv/static
npm = $(shell which npm)
erl = $(shell which erl)
gulp = cd $(static) && ./node_modules/.bin/gulp

install: install_npm install_erlang install_rebar
deps: install deps_backend deps_frontend
build: deps build_backend build_frontend

install_npm:
	$(npm) -v || ( echo "NPM is required" && exit 1 )

install_erlang:
	which erl || ( echo "Erlang is required" && exit 1 )
	
install_rebar:
	wget https://github.com/rebar/rebar/wiki/rebar
	chmod +x rebar

deps_backend:
	./rebar get-deps

deps_frontend:
	cd $(static) && $(npm) install
	cd $(static) && ./node_modules/.bin/bower install

build_backend:
	./rebar compile

build_frontend:
	$(gulp)

watch: install_npm deps_frontend
	$(gulp) watch

clean:
	rm -rf ./rebar
	rm -rf ./deps
	rm -rf $(static)/node_modules
	rm -rf $(static)/bower_components
