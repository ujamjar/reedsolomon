########################################
# hardcaml - hardware design in OCaml
#
#   (c) 2014 MicroJamJar Ltd
#
# Author(s): andy.ray@ujamjar.com
# Description: 
#
########################################

.PHONY: clean all install uninstall rswebdemo

BUILD_OPTS=

WEBDEMO ?= $(shell if ocamlfind query hardcaml js_of_ocaml >/dev/null 2>&1; then echo --enable-webdemo; fi)

all: 
	jbuilder build @install

install: all
	jbuilder install

uninstall: 
	jbuilder uninstall

rswebdemo: all
	jbuilder build webdemo/rswebdemo.bc.js

clean:
	rm -fr _build
	find . -name "*~" | xargs rm

