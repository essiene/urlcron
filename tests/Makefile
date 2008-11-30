EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERL_FLAGS=-pa $(EBIN_DIR) -I $(INCLUDE_DIR) -sname urlcronunittest@localhost
ERLC_FLAGS=-W -o $(EBIN_DIR) -pa $(EBIN_DIR) -I$(INCLUDE_DIR)

%.beam: %.erl
	erlc $(ERLC_FLAGS) $<


OBJECTS= test_util \
		 test_schedule_store \
		 test_schedule \
		 test_scheduler \
		 test_webservice


test: compile
	erl -noshell $(ERL_FLAGS) -s test_util test -s init stop
	erl -noshell $(ERL_FLAGS) -s test_schedule_store test -s init stop
	erl -noshell $(ERL_FLAGS) -s test_schedule test -s init stop
	erl -noshell $(ERL_FLAGS) -s test_scheduler test -s init stop
	erl -noshell $(ERL_FLAGS) -s test_webservice test -s init stop
	@echo all done


compile: ${OBJECTS:%=%.beam}
	@echo  All tests compiled

clean:
	@rm -f *.beam
	@rm -f erl_crash.dump
