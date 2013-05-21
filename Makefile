


OBJS = $(patsubst %.erl,%.beam,$(wildcard *.erl))


EJABBERD_PATH=/usr/lib/ejabberd/

#all: src install
#
#src: 
#	erlc -I$(EJABBERD_PATH)/include -pa$(EJABBERD_PATH)/ebin mod_videobridge.erl

#all: udprelay.beam mod_videobridge.beam
all: $(OBJS) install

%.beam: %.erl
	erlc -I$(EJABBERD_PATH)/include -pa$(EJABBERD_PATH)/ebin $<

install:
	cp *.beam $(EJABBERD_PATH)/ebin/

