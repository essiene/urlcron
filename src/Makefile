EBIN_DIR= ../ebin
INCLUDE_DIR= ../include

ERL_FLAGS= -pa $(EBIN_DIR) -I $(INCLUDE_DIR)
ERLC_FLAGS=-W -o $(EBIN_DIR)

$(EBIN_DIR)/%.beam:  %.erl
	@mkdir -p $(EBIN_DIR)
	erlc $(ERL_FLAGS) $(ERLC_FLAGS) $<

$(EBIN_DIR)/%.app: %.app
	@mkdir -p $(EBIN_DIR)
	@cp $< $(EBIN_DIR)


OBJECTS= urlcron_util \
		 schedule_store \
		 urlcron_jsonutil \
		 schedule_data urlcron_schedule \
		 urlcron_scheduler \
		webservice urlcron_mochiweb \
		urlcron_app urlcron_sup \
		urlcron urlcronctl

APP=urlcron


all: ${OBJECTS:%=$(EBIN_DIR)/%.beam} ${APP:%=$(EBIN_DIR)/%.app}
	@echo Source built

run:
	erl $(ERL_FLAGS) -noshell -s urlcron_mochiweb start

clean:
	@rm -f erl_crash.dump
