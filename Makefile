REBAR = ./rebar3
ELVIS = ./elvis

all: compile

compile:
	@$(REBAR) compile

rel:
	@$(REBAR) release

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyzer

eunit:
	@$(REBAR) eunit

ct:
	@$(REBAR) ct

update:
	@$(REBAR) update

console: rel
	_build/rel/celo/bin/celo console

elvis:
	@$(ELVIS) -c elvis.config rock

travis: update compile rel eunit ct elvis

.PHONY: compile rel clean dialyzer eunit ct console update elvis travis
