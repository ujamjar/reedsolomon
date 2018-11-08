.PHONY: clean all install uninstall rswebdemo

all:
	jbuilder build @install

install: all
	jbuilder install

uninstall:
	jbuilder uninstall

rswebdemo: all
	jbuilder build webdemo/rswebdemo.bc.js

test: all
	jbuilder build test/test_iter.exe
	./_build/default/test/test_iter.exe

clean:
	rm -fr _build
	find . -name "*~" | xargs rm

