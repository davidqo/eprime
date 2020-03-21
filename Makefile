REBAR=./rebar3
ERLANG=erl
APP_NAME=eprime
EXECUTABLE_NAME=$(APP_NAME)
BIN_DIR=_build/default/rel/$(APP_NAME)/bin/

all: compile release

compile:
	$(REBAR) compile
	
release:
	$(REBAR) release

run:
	$(BIN_DIR)/$(EXECUTABLE_NAME) console

clean:
	$(REBAR) clean

test:
	$(REBAR) clean

.PHONY: all compile release run clean test