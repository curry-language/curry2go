# Makefile to install components of curry2go compiler

# The root directory of the installation
export ROOT = $(CURDIR)
# The binary directory
export BINDIR = $(ROOT)/bin
# Directory where the actual libraries are located
export LIBDIR = $(ROOT)/lib
# The CPM executable
CPM=cypm

# Standard location of Curry2Go run-time auxiliaries
GOWORKSPACE=$(HOME)/go/src

# The generated compiler executable
COMPILER=$(HOME)/.cpm/bin/curry2go

.PHONY: install
install: scripts runtime
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

# install the scripts in the bin directory:
.PHONY: scripts
scripts:
	cd scripts && $(MAKE) all

# remove the scripts in the bin directory:
.PHONY: cleanscripts
cleanscripts:
	cd scripts && $(MAKE) clean

# clean compilation targets
.PHONY: clean
clean:
	$(CPM) clean

# clean all installed components
.PHONY: cleanall
cleanall: clean cleanscripts
	/bin/rm -rf $(GOWORKSPACE) $(COMPILER)

