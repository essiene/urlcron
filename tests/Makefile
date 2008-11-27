EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERL_FLAGS=-pa $(EBIN_DIR) -I $(INCLUDE_DIR)
ERLC_FLAGS=-W -o $(EBIN_DIR)

%.beam: %.erl
	erlc $(ERL_FLAGS) $(ERLC_FLAGS) $<


OBJECTS=


test: compile
	@echo all done


compile: ${OBJECTS:%=%.beam}
	@echo  All tests compiled

clean:
	@rm -f *.beam
	@rm -f erl_crash.dump
