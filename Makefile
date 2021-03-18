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

# Remove command
RM=/bin/rm

# The Go implementation of the base module Curry.Compiler.Distribution
COMPDISTGO=lib/Curry/Compiler/Distribution_external.go

.PHONY: install
install: scripts runtime
	$(CPM) install
	$(MAKE) lib/Curry/Compiler/Distribution_external.go

$(COMPDISTGO): src/Install.curry src/Curry2Go/Config.curry
	$(CPM) curry :load Install :eval main :quit

# install base libraries from package `base`:
.PHONY: baselibs
baselibs:
	$(RM) -rf base
	$(CPM) checkout base
	$(RM) -rf $(LIBDIR)
	/bin/cp -r base/src $(LIBDIR)
	/bin/cp base/VERSION $(LIBDIR)/VERSION

.PHONY: uninstall
uninstall: runtime
	$(RM) -rf $(GOWORKSPACE)
	$(CPM) uninstall

# install run-time libraries:
.PHONY: runtime
runtime:
	$(RM) -rf $(GOWORKSPACE)
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
	$(RM) -f $(COMPDISTGO)

# clean all installed components
.PHONY: cleanall
cleanall: clean cleanscripts
	$(RM) -rf $(GOWORKSPACE) $(COMPILER)

