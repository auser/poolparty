LIBDIR					= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
CC							= erlc
ERL							= erl
EBIN						= ebin
CFLAGS					= +debug_info -W0 -I include -I ../../../../lib/proto/gen-erl -pa $(EBIN)
COMPILE					= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS				= $(wildcard deps/*/ebin)
APP							= hermes

all: ebin compile
deps: thrift
	
compile:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

run: all
	@$(ERL) -pa ./ebin -pa $(EBIN_DIRS) -s -s thrift_example run -s init stop

ebin:
	@mkdir ebin

clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump ebin/*.boot ebin/*.rel ebin/*.script 

thrift:
	@(cd deps/thrift;$(MAKE))
