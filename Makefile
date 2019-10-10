all: install

install: \
	packages/snippets \
	packages/straight \
	packages/use-package

packages/snippets:
	[ -d ~/Labs/YASR/snippets ] && ln -s ~/Labs/YASR/snippets $@ \
	|| git clone git@github.com:Alhadis/YASR.git $@

packages/straight:
	git clone git@github.com:raxod502/straight.el.git $@

packages/use-package:
	git clone git@github.com:jwiegley/use-package.git $@
