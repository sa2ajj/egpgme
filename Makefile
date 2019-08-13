.PHONY: test clean

REBAR := rebar3

all: compile test

compile:
	@$(REBAR) compile

test: compile
	@$(REBAR) ct

clean:
	@$(REBAR) clean
