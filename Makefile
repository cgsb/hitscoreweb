all: build

build:
	ocaml setup.ml -build

static: install
	ocamlfind ocamlopt -linkall \
	    -package ocsigenserver,ocsigenserver.ext.ocsipersist-sqlite  \
	    -package eliom.server,ocsigenserver.ext.staticmod  \
	    -package hitscoreweb \
            server_main.cmx -o hitscoreserver -linkpkg -thread

install: build
	ocaml setup.ml -reinstall

uninstall:
	ocaml setup.ml -uninstall

doc:
	ocaml setup.ml -doc

clean:
	ocaml setup.ml -clean


# clean everything and uninstall
fresh: clean uninstall

.PHONY: doc install uninstall clean fresh
