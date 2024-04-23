BUILDTOOL = rebar3
FORMATTER = efmt

.PHONY: build test doc

build:
	  $(BUILDTOOL) compile

format:
	  $(FORMATTER) -w

check:
	  $(FORMATTER) -c
	  $(BUILDTOOL) do xref, dialyzer

test:
	  $(BUILDTOOL) eunit

cover:
	  $(BUILDTOOL) cover

doc:
	  $(BUILDTOOL) edoc

clean:
	  $(BUILDTOOL) clean

start:
	  $(BUILDTOOL) shell
