.PHONY: clean ct format format-check dialyzer test

format:	 
	 rebar3 fmt

format-check:	 	 
	 rebar3 fmt --check

ct:
	rebar3 ct -v 100 --cover

clean:
	rebar3 clean

dialyzer:
	rebar3 dialyzer

test:
	ct_run -logdir logs	