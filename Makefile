# Makefile to install components of curry2go compiler

# CPM executable
CPM=cypm

# Standard location of Curry2Go run-time auxiliaries
GOWORKSPACE=$(HOME)/go/src

# The generated compiler executable
COMPILER=$(HOME)/.cpm/bin/curry2go

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

# clean compilation targets
.PHONY: clean
clean:
	$(CPM) clean

# clean all installed components
.PHONY: cleanall
cleanall: clean
	/bin/rm -rf $(GOWORKSPACE) $(COMPILER)

