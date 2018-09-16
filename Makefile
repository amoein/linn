.PHONY: all test clean
REBAR=./resource/rebar3

all: compile

compile:
	${REBAR} compile

test:
	${REBAR} ct -v

clean:
	${REBAR} clean ;rm -rf _build/default/rel/*


