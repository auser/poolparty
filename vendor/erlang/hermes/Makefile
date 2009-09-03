LIBDIR					= `erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION					= $(shell cat VERSION | tr -d '\n')
CC							= erlc
ERL							= erl
EBIN						= ebin
CFLAGS					= +debug_info -W0 -I include -pa $(EBIN) -I gen-erl/
COMPILE					= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS				= $(wildcard deps/*/ebin) $(wildcard include/*/ebin)
DEP_EBIN_DIRS_DOTDOT    = -pa ../deps/gen_cluster/ebin -pa ../deps/mochiweb/ebin -pa ../deps/thrift/ebin -pz ../include/stoplight/ebin # todo, make dynamic
WEB_DIR					= web/
TEST_DIR				= test
TEST_EBIN_DIR		= $(TEST_DIR)/ebin
DEPS_DIR 				= deps
INCLUDE_DIR = include
STOPLIGHT_DIR	= $(INCLUDE_DIR)/stoplight
STOPLIGHT_VERSION = $(shell cat $(STOPLIGHT_DIR)/VERSION | tr -d '\n')
APP							= hermes
RELFILE = $(EBIN)/hermes-$(VERSION).rel

all: $(TEST_EBIN_DIR) ebin compile
all_boot: all boot
wonderland_boot: wonderland all_boot
start: all start_all
rstakeout: wonderland compile

deps: mochi build_thrift gen_cluster stoplight cp_dep_beams

mochi:
	(cd deps/mochiweb;$(MAKE))
gen_cluster:
	(cd deps/gen_cluster;$(MAKE))
build_thrift:
	(cd deps/thrift;$(MAKE))
	thrift --gen erl thrift/hermes.thrift
	mv gen-erl/hermes*.hrl include
	mv gen-erl/hermes*.erl src/ambassador/ambassadors/thrift
	rmdir gen-erl

stoplight:
	[ -d $(STOPLIGHT_DIR) ] || (mkdir -p $(INCLUDE_DIR) && cd $(INCLUDE_DIR) && git clone git://github.com/jashmenn/stoplight.git)
	cd $(STOPLIGHT_DIR) && git pull origin master
	cd $(STOPLIGHT_DIR) && rake
cp_dep_beams:
	cp $(DEPS_DIR)/*/ebin/*.beam $(EBIN)

compile: deps
	@$(ERL) -pa $(EBIN_DIRS) -pa $(EBIN) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

edoc:
	@echo Generating $(APP) documentation from srcs
	@$(ERL) -noinput -eval 'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' -s erlang halt

eunit:
	cd test/include/eunit && make

shell: compile
	$(ERL) 	-sname shell -setcookie test \
					-pa $(EBIN) \
					-pa deps/*/ebin

test: $(TEST_EBIN_DIR) compile
	$(ERL) 	-noshell -pa $(EBIN) \
					-pa deps/*/ebin \
					-pa $(TEST_EBIN_DIR) \
					-pa test/include/gen_server_mock/ebin \
					-s test_suite test \
					-s init stop
	
boot: compile
	(cd $(EBIN); erl -pa ../$(EBIN) $(DEP_EBIN_DIRS_DOTDOT) -pz ../$(STOPLIGHT_DIR)/ebin -noshell -run make_boot write_scripts hermes $(VERSION) stoplight $(STOPLIGHT_VERSION))

release:
	(cd $(EBIN); erl -pa ../$(EBIN) $(DEP_EBIN_DIRS_DOTDOT) -pz ../$(STOPLIGHT_DIR)/ebin -noshell -run make_boot write_release_scripts hermes $(VERSION) stoplight $(STOPLIGHT_VERSION))

#relup:
#	(cd $(EBIN); erl -pa ../$(EBIN) $(DEP_EBIN_DIRS_DOTDOT) -pz ../$(STOPLIGHT_DIR)/ebin -noshell -run systools make_relup "hermes-0.0.2", ["hermes-0.0.1"], ["hermes-0.0.1"])

# (cd $(EBIN); erl -pa ../$(EBIN) -pa ../$(EBIN_DIRS) -pz ../$(STOPLIGHT_DIR)/ebin -noshell -run target_system create "hermes-$(VERSION)" -s init stop)

target_system: $(RELFILE)
	escript scripts/target_system create "ebin/hermes-$(VERSION)"

inspect_target_system:
	exec tar tf ebin/hermes-$(VERSION).tar.gz

start_all:
	(cd $(EBIN); erl -pa $(EBIN) -noshell -sname hermes -boot hermes)

$(EBIN):
	@mkdir $(EBIN)

clean:
	echo $(TEST_EBIN_DIR)
	rm -rf $(EBIN)/*.beam $(EBIN)/erl_crash.dump erl_crash.dump $(EBIN)/*.boot $(EBIN)/*.rel $(EBIN)/*.script $(TEST_EBIN_DIR)/*.beam $(EBIN)/hermes-*.tar.gz

clean_mochiweb:
	rm -rf deps/mochiweb/ebin/*.beam

$(TEST_EBIN_DIR):
	@mkdir $(TEST_EBIN_DIR)

$(RELFILE): boot

# tmp, testing for nate
nates_delivery:
	scp scripts/target_system vm:~/hermes
	scp ebin/hermes-0.0.1.tar.gz vm:~/hermes 
	# remotely:
	# rm -rf /usr/local/hermes && ./target_system install hermes-0.0.1 /usr/local/hermes