BASE_DIR=$(shell pwd)
REBAR=$(BASE_DIR)/rebar3


compile: rebar.config relx.config
	$(REBAR) compile


release:
	$(REBAR) new release etheatr
	rm -rf ./etheatr
	$(REBAR) release

clean:
	rm -rf ./etheatr
	rm -rf ./_build

rebuild:
	rm -rf ./etheatr
	rm -rf ./_build/default/rel
	$(REBAR) new release etheatr
	$(REBAR) release

all: clean compile release


console:
	$(BASE_DIR)/_build/default/rel/etheatr/bin/etheatr console
