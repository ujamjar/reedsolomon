########################################
# hardcaml - hardware design in OCaml
#
#   (c) 2014 MicroJamJar Ltd
#
# Author(s): andy.ray@ujamjar.com
# Description: 
#
########################################

.PHONY: clean all install uninstall 

BUILD_OPTS=

all: setup.data
	ocaml setup.ml -build

setup.ml:
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

install: all
	ocaml setup.ml -install

uninstall: 
	ocamlfind remove hardcaml

rswebdemo: all
	js_of_ocaml +nat.js rswebdemo.byte

clean:
	ocaml setup.ml -clean
	- rm -f rswebdemo.js
	- find . -name "*~" | xargs rm

