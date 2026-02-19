EMACS ?= emacs

.PHONY: test
test:
	$(EMACS) -Q --batch -L . -L test -l test/org-task-test.el -f ert-run-tests-batch-and-exit
