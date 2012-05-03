.PHONY: mount_hitscoreweb build all install static
all: build



_build/hitscoreweb:
	mkdir -p _build/hitscoreweb
_build/hitscoreweb/%.eliom: src/webapp/%.eliom
	CC=$$PWD ; cd _build/hitscoreweb ; ln -s ../../$< ; cd $$CC 
_build/hitscoreweb/Makefile: src/webapp/Makefile
	CC=$$PWD ; cd _build/hitscoreweb ; ln -s ../../$< ; cd $$CC 

TO_MOUNT=$(patsubst src/webapp/%,_build/hitscoreweb/%,${wildcard src/webapp/*})
mount_hitscoreweb:: _build/hitscoreweb $(TO_MOUNT)

build: mount_hitscoreweb
	ocaml setup.ml -build hitscorewebpam.mllib
	make -C _build/hitscoreweb byte js css
	ocaml setup.ml -build

build_opt: mount_hitscoreweb
	ocaml setup.ml -build hitscorewebpam.mllib
	make -C _build/hitscoreweb byte opt js css
	ocaml setup.ml -build

static: build_opt install
	ocamlfind ocamlopt -linkall \
	    -package ocsigenserver,ocsigenserver.ext.ocsipersist-sqlite  \
	    -package eliom.server,ocsigenserver.ext.staticmod  \
	    -package hitscore,hitscorewebpam,core_extended \
	     _build/hitscoreweb/hitscoreweb.cmxa \
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
