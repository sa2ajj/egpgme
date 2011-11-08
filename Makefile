REBAR := $(shell which rebar || echo $(PWD)/tools/rebar)

all: compile test

compile:
	@$(REBAR) compile

test:
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
