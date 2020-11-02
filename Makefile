# Makefile to install components of curry2go compiler

CPM=cypm

GOWORKSPACE=$(HOME)/go/src

.PHONY: install
install: runtime
	$(CPM) install

.PHONY: uninstall
uninstall: runtime
	/bin/rm -rf $(GOWORKSPACE)
	$(CPM) uninstall

# install run-time libraries:
.PHONY: runtime
runtime:
	/bin/rm -rf $(GOWORKSPACE)
	mkdir -p $(GOWORKSPACE)
	cp -r gocurry $(GOWORKSPACE)/gocurry
