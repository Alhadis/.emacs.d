all: snippets

snippets:
	[ -d ~/Labs/YASR/snippets ] && ln -s ~/Labs/YASR/snippets $@ \
	|| git clone git@github.com:Alhadis/YASR.git $@

clean:
	find . -type f -name '*.elc' -delete
	rm -f custom.el
	rm -rf elpa
	rm -rfP auto-save-list || true

.PHONY: clean
