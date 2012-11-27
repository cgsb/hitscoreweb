## Sample Makefile for eliom application.


APP_NAME := hitscoreweb

## Packages required to build the server part of the application
SERVER_PACKAGES := hitscoregen threads core hitscore core_extended 

## Packages to be linked in the client part

CLIENT_PACKAGES := oclosure

## Source files for the server part
COMMON_FILES := \
  hitscoreweb_std.eliom	    \
  hitscoreweb_messages.eliom	    \
  hitscoreweb_data_access.eliom	    \
  hitscoreweb_state.eliom	    \
  hitscoreweb_queries.eliom	    \
  hitscoreweb_services.eliom	    \
  hitscoreweb_authentication.eliom \
  hitscoreweb_template.eliom	   \
  hitscoreweb_meta_form.eliom	    \
  hitscoreweb_persons.eliom	   \
  hitscoreweb_hiseq_runs.eliom	   \
  hitscoreweb_layout_service.eliom \
  hitscoreweb_one_person_service.eliom \
  hitscoreweb_libraries.eliom \
  hitscoreweb_facility_stats.eliom \
  hitscoreweb.eliom
SERVER_FILES := $(COMMON_FILES)
CLIENT_FILES := $(COMMON_FILES)

CLOSURE_CSS_NAMES= common dialog datepicker

JQUERY_MIN_JS=../../src/ext/highcharts/jquery.min.js
HIGHCHARTS_JS=../../src/ext/highcharts/highcharts.js
HIGHCHARTS_LIB=${JQUERY_MIN_JS} ${HIGHCHARTS_JS}

## Required binaries
#CAMLP4= 'camlp4o str.cma \
#   -I`ocamlfind query extlib` extLib.cma \
#   -I`ocamlfind query pcre` pcre.cma \
#   -I`ocamlfind query csv` csv.cma \
#   -I`ocamlfind query calendar` calendarLib.cma \
#   -I`ocamlfind query pgocaml.syntax` pgocaml.cma pa_pgsql.cmo'

#ELIOMC      := eliomc -thread -pp $(CAMLP4) -I . 
ELIOMC      := eliomc 
ELIOMOPT    := eliomopt

#ELIOMDEPSERVER := eliomdep -server -pp $(CAMLP4) -I .
ELIOMDEPSERVER := eliomdep -server 
ELIOMDEPCLIENT := eliomdep -client

JS_OF_ELIOM := js_of_eliom -jsopt -pretty -I _client/ -I .

## Where to put intermediate object files.
## - ELIOM_{SERVER,CLIENT}_DIR must be distinct
## - ELIOM_CLIENT_DIR mustn't be the local dir.
## - ELIOM_SERVER_DIR could be ".", but you need to
##   remove it from the "clean" rules...

export ELIOM_SERVER_DIR := _server
export ELIOM_CLIENT_DIR := _client
export ELIOM_TYPE_DIR   := .

#####################################

all: byte opt js css
js::  ${APP_NAME}.js
css::  ${APP_NAME}.css
byte:: ${APP_NAME}.cma 
opt:: ${APP_NAME}.cmxs 

#### Server side compilation #######

SERVER_INC  := ${addprefix -package ,${SERVER_PACKAGES}}

SERVER_OBJS := $(patsubst %.eliom,${ELIOM_SERVER_DIR}/%.cmo, ${SERVER_FILES})
#SERVER_OBJS := $(patsubst %.ml,${ELIOM_SERVER_DIR}/%.cmo, ${SERVER_OBJS})

${APP_NAME}.cma: ${SERVER_OBJS}
	${ELIOMC} -a -o $@ $^
${APP_NAME}.cmxa: ${SERVER_OBJS:.cmo=.cmx}
	${ELIOMOPT} -a -o $@ $^

${ELIOM_TYPE_DIR}/%.type_mli: %.eliom
	${ELIOMC} -infer ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmi: %.mli
	${ELIOMC} -c ${SERVER_INC} $<

${ELIOM_SERVER_DIR}/%.cmo: %.ml
	${ELIOMC} -c ${SERVER_INC} -o $@ $<
${ELIOM_SERVER_DIR}/%.cmo: %.eliom
	${ELIOMC} -c -noinfer ${SERVER_INC} -o $@ $<

${ELIOM_SERVER_DIR}/%.cmx: %.ml
	${ELIOMOPT} -c ${SERVER_INC} -o $@ $<
${ELIOM_SERVER_DIR}/%.cmx: %.eliom
	${ELIOMOPT} -c -noinfer ${SERVER_INC} -o $@ $<

%.cmxs: %.cmxa
	$(ELIOMOPT) -shared -linkall -o $@ $<

OCLOSURE_CSS_DIR=$(shell ocamlfind query oclosure)/closure/goog/css
CLOSURE_CSS_FILES := $(patsubst %, ${OCLOSURE_CSS_DIR}/%.css,${CLOSURE_CSS_NAMES})
${APP_NAME}.css: ${CLOSURE_CSS_FILES}
	cat ${CLOSURE_CSS_FILES} > $@

##### Client side compilation ####

CLIENT_LIBS := ${addprefix -package ,${CLIENT_PACKAGES}}
CLIENT_INC  := ${addprefix -package ,${CLIENT_PACKAGES}}

CLIENT_OBJS := $(patsubst %.eliom,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_FILES})
CLIENT_OBJS := $(patsubst %.ml,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_OBJS})

${APP_NAME}_tmp.js: ${CLIENT_OBJS}
	${JS_OF_ELIOM} -o $@ ${CLIENT_LIBS} $^

${APP_NAME}_tmp_oclosure.js: ${APP_NAME}_tmp.js
	oclosure_req $<

${APP_NAME}.js: ${APP_NAME}_tmp_oclosure.js ${APP_NAME}_tmp.js ${HIGHCHARTS_LIB}
	cat  ${APP_NAME}_tmp_oclosure.js ${HIGHCHARTS_LIB}  ${APP_NAME}_tmp.js > $@

${ELIOM_CLIENT_DIR}/%.cmi: %.eliom
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

${ELIOM_CLIENT_DIR}/%.cmo: %.eliom %.type_mli
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<
${ELIOM_CLIENT_DIR}/%.cmo: %.ml
	${JS_OF_ELIOM} -c ${CLIENT_INC} $<

############

## Clean up

clean:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f *.type_mli
	-rm -f ${APP_NAME}.js
	-rm -rf ${ELIOM_CLIENT_DIR} ${ELIOM_SERVER_DIR}

distclean: clean.local
	-rm -f *~ \#* .\#*

## Dependencies

depend: .depend
.depend:
	$(ELIOMDEPSERVER) ${SERVER_FILES} > .depend
	$(ELIOMDEPCLIENT) ${CLIENT_FILES} >> .depend
	ocaml ../../bin/fake_eliomdep.ml $(patsubst %.eliom,%,${COMMON_FILES}) >> .depend
## Warning: Dependencies towards *.eliom are not handled by eliomdep yet.

include .depend

## installation #########

STATICDIR      := /tmp/static

$(STATICDIR):
	mkdir -p $@

install: all $(STATICDIR)
	cp $(APP_NAME).js $(STATICDIR)/$(APP_NAME).js
