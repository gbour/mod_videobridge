
.PHONY: clean build test
build: ejabberd
	./rebar get-deps
	./rebar compile

ejabberd:
	if [ ! -e deps/ejabberd ]; then \
		git clone git://github.com/processone/ejabberd.git deps/ejabberd; \
	fi
	cd deps/ejabberd; git checkout 13.09;

test:
	./rebar -C test.config get-deps compile
	./rebar -C test.config skip_deps=true ct

clean:
	./rebar clean
