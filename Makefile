.PHONY: test

REBAR := $(shell which rebar || echo $(PWD)/tools/rebar)

all: compile test

compile:
	@$(REBAR) compile

test: compile
	@$(REBAR) ct

clean:
	@$(REBAR) clean
