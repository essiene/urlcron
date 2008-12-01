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
	erl -noshell $(ERL_FLAGS) $(foreach obj, $(OBJECTS), -s $(obj) test) -s init stop


compile: ${OBJECTS:%=%.beam}
	@echo  All tests compiled

clean:
	@rm -f *.beam
	@rm -f erl_crash.dump
