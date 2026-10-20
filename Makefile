# -*- mode: makefile; -*-

# Shims for BSD make
> ?= $(.ALLSRC)
^ ?= $>

all: snippets

# Checkout snippets repository if we haven't already
snippets:
	[ -d ~/Labs/YASR/snippets ] && ln -s ~/Labs/YASR/snippets $@ \
	|| git clone git@github.com:Alhadis/YASR.git $@



# Section 1: Elisp compilation
eval-elisp = emacs -Q --batch -L git-modes --eval

compile: dynvars compile-native
compile-native:;   $(eval-elisp) '(native-compile-directory ".")'
compile-bytecode:; $(eval-elisp) '(byte-recompile-directory "." 0)'

# Generate and check list of “special” variable declarations
dynvars:
	EMACS_GENERATE_DYNVARS=1 $(MAKE) compile-bytecode
	echo ';; -*- lisp-data -*-' > $@
	find . -type f -name '*.dynvars' -depth -exec cat {} + >> $@
	EMACS_DYNVARS_FILE=$@ $(MAKE) compile-clean compile-bytecode



# Section 2: Resetting worktree

# Delete generated and untracked files
clean: compile-clean
	rm -f custom.el dynvars tramp
	rm -rf elpa eln-cache transient
	rm -rfP auto-save-list || true

# Delete byte-compiled Elisp files
compile-clean:
	find . -type f \( -name '*.elc' -or -name '*.dynvars' \) -print -delete

.PHONY: clean compile-clean
