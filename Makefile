.PHONY: all

all: reverb.html

%.html: %.md
	nix-shell -p pandoc --run "pandoc -s -t html $< -o $@"

%.md: %.fut
	futhark literate $<
