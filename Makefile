
EJABBERD_PATH=/usr/lib/ejabberd/

all: src install

src: 
	erlc -I$(EJABBERD_PATH)/include -pa$(EJABBERD_PATH)/ebin mod_videobridge.erl

install:
	cp *.beam $(EJABBERD_PATH)/ebin/

