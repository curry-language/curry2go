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
COMPILER=$(BINDIR)/curry2goc

# The generated executable of the REPL
REPL=$(BINDIR)/curry2goi

# Remove command
RM=/bin/rm

# The Go implementation of the base module Curry.Compiler.Distribution
COMPDISTGO=lib/Curry/Compiler/Distribution_external.go

# The root of the Curry system used to install this package
INSTALLROOT := $(shell $(CPM) -v quiet curry :set v0 :l Curry.Compiler.Distribution :eval installDir :q)

.PHONY: install
install: scripts runtime
	$(CPM) install
	$(MAKE) $(COMPILER)
	$(MAKE) $(REPL)
	$(MAKE) $(COMPDISTGO)

$(COMPILER): src/*.curry src/Curry2Go/*.curry
	$(CPM) -d BININSTALLPATH=$(BINDIR) install -x curry2goc

$(REPL): src/*.curry src/Curry2Go/*.curry
	$(CPM) -d BININSTALLPATH=$(BINDIR) install -x curry2goi

$(COMPDISTGO): src/Install.curry src/Curry2Go/PkgConfig.curry
	$(CPM) curry :load Install :eval main :quit

# install base libraries from package `base`:
.PHONY: baselibs
baselibs:
	$(RM) -rf base
	$(CPM) checkout base
	$(RM) -rf $(LIBDIR)
	/bin/cp -r base/src $(LIBDIR)
	/bin/cp base/VERSION $(LIBDIR)/VERSION
	$(RM) -rf base

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

# install scripts in the bin directory:
.PHONY: scripts
scripts:
	cd scripts && $(MAKE) all
	cd $(BINDIR) && $(RM) -f curry curry2go-frontend
	# add alias `curry`:
	cd $(BINDIR) && ln -s curry2go curry
	# add alias for frontend:
	ln -s $(INSTALLROOT)/bin/*-frontend bin/curry2go-frontend

# remove scripts in the bin directory:
.PHONY: cleanscripts
cleanscripts:
	cd scripts && $(MAKE) clean

# clean compilation targets
.PHONY: clean
clean:
	$(CPM) clean
	$(RM) -f $(COMPILER) $(REPL) $(COMPDISTGO)

# clean all installed components
.PHONY: cleanall
cleanall: clean cleanscripts
	$(RM) -rf $(GOWORKSPACE)

