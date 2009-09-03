
include ../vsn.mk

EMULATOR=beam
EBIN=../ebin
INCLUDE=../include

APP_TARGET=$(EBIN)/eunit.app
APPUP_TARGET=$(EBIN)/eunit.appup

ERL_COMPILE_FLAGS += -I$(INCLUDE) +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

SOURCES= \
	eunit_autoexport.erl \
	eunit_striptests.erl \
	eunit.erl \
	eunit_serial.erl \
	eunit_listener.erl \
	eunit_proc.erl \
	eunit_test.erl \
	eunit_lib.erl \
	eunit_data.erl \
	eunit_server.erl \
	eunit_tty.erl \
	eunit_surefire.erl \
	eunit_tests.erl \
	code_monitor.erl \
	file_monitor.erl \
	file_monitor_tests.erl \
	autoload.erl

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) $(APPUP_TARGET)

all: $(OBJECTS)

$(OBJECTS): eunit_internal.hrl \
	$(INCLUDE)/eunit.hrl 

clean:
	rm -f $(OBJECTS)

$(EBIN)/%.$(EMULATOR):%.erl
	erlc -pa $(EBIN) -W $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

$(APP_TARGET): eunit.app.src ../vsn.mk
	sed -e 's;%VSN%;$(EUNIT_VSN);' $< > $@

$(APPUP_TARGET): eunit.appup.src ../vsn.mk
	sed -e 's;%VSN%;$(EUNIT_VSN);' $< > $@
