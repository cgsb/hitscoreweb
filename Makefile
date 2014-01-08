.PHONY: mount_hitscoreweb build all install static
all: build



_build/hitscoreweb:
	mkdir -p _build/hitscoreweb
_build/hitscoreweb/%.eliom: src/webapp/%.eliom
	CC=$$PWD ; cd _build/hitscoreweb ; ln -s ../../$< ; cd $$CC
_build/hitscoreweb/%.ml: src/webapp/%.ml
	CC=$$PWD ; cd _build/hitscoreweb ; ln -s ../../$< ; cd $$CC
_build/hitscoreweb/Makefile: src/webapp/Makefile
	CC=$$PWD ; cd _build/hitscoreweb ; ln -s ../../$< ; cd $$CC
_build/hsw_manager.ml: src/app/hsw_manager.ml
	CC=$$PWD ; cd _build/ ; ln -s ../$< ; cd $$CC

TO_MOUNT=$(patsubst src/webapp/%,_build/hitscoreweb/%,${wildcard src/webapp/*})
mount_hitscoreweb:: _build/hitscoreweb $(TO_MOUNT)

hsw_manager: _build/hsw_manager.ml
	ocamlfind ocamlc -package core,hitscore -thread -linkpkg $< -o $@

build: mount_hitscoreweb hsw_manager
	make -C _build/hitscoreweb byte js css

# ocaml setup.ml -build

build_opt: mount_hitscoreweb hsw_manager
	make -C _build/hitscoreweb  opt js css

#	ocaml setup.ml -build

_build/hitscoreweb/hitscoreweb.cmxa: build_opt

hitscoreserver: _build/hitscoreweb/hitscoreweb.cmxa
	ocamlfind ocamlopt -linkall \
	    -package ocsigenserver,ocsigenserver.ext.ocsipersist-sqlite  \
	    -package eliom.server,ocsigenserver.ext.staticmod  \
	    -package re.posix,hitscore \
	     _build/hitscoreweb/hitscoreweb.cmxa \
	     server_main.cmx -o hitscoreserver -linkpkg -thread

static:  hitscoreserver hsw_manager

install: build
	ocaml setup.ml -reinstall

uninstall:
	ocaml setup.ml -uninstall

doc:
	ocaml setup.ml -doc

clean:
	rm -fr _build

distclean: clean
	rm -f setup.data setup.log


# clean everything and uninstall
fresh: clean uninstall

.PHONY: doc install uninstall clean fresh
