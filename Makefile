RUBY							= `which ruby`
RUBYGEMS					= `which gem`
GEMENV						= `gem env`

all: check_install run_tests

check_install:
	$(RUBY) -v
	$(RUBYGEMS) -v
	
show_env:
	echo $(GEMENV)
	
test: run_tests

run_tests:
	rake test