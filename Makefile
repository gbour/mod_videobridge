
.PHONY: clean build
build: ejabberd
	./rebar get-deps
	./rebar compile

ejabberd:
	if [ ! -e deps/ejabberd ]; then \
		git clone git://github.com/processone/ejabberd.git deps/ejabberd; \
	fi
	cd deps/ejabberd; git checkout 13.09;

clean:
	./rebar clean
