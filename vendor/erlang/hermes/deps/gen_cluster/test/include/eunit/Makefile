
APPNAME=eunit

SUB_DIRECTORIES = src

include vsn.mk

DOC_OPTS={def,{version,\"$(EUNIT_VSN)\"}}


all: subdirs

subdirs:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE)); \
	done

clean:
	@for d in $(SUB_DIRECTORIES); do \
	  	(cd $$d; $(MAKE) clean); \
	done

docs:
	erl -noshell -eval "edoc:application($(APPNAME), \".\", [$(DOC_OPTS)])" -s init stop

test: subdirs
	@echo Testing...
	@erl -noshell -pa ebin -eval 'eunit:test({application,eunit})' -s init stop
