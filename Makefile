#
# Makefile for automation
#

emacs ?= emacs

all: test

# Use LC_ALL=C to avoid locale dependencies in the dates!
test: clean
	LC_ALL=C $(emacs) -l dired-rsync-ert.el -l dired-rsync.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -Q -batch -f batch-byte-compile dired-rsync.el

clean:
	rm -f f.elc

.PHONY:	all test
