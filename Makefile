# Makefile to install all components of the Curry2Go compiler

# The name of a Curry system used for generating the initial compiler
export CURRYSYSTEM := $(shell which pakcs)

# The root directory of the installation
export ROOT = $(CURDIR)
# The binary directory
export BINDIR = $(ROOT)/bin
# Directory where local executables are stored
export LOCALBIN = $(BINDIR)/.local
# Directory where the actual libraries are located
export LIBDIR = $(ROOT)/lib
# The CPM executable
CPMBIN=cypm

# CPM with compiler specified by CURRYSYSTEM
CPM = $(CPMBIN) -d CURRYBIN=$(CURRYSYSTEM)
# CPM with Curry2Go compiler
CPMC2G = $(CPMBIN) -d CURRYBIN=$(BINDIR)/curry2go

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

# Install the Curry2Go system (compiler and REPL) with CURRYSYSTEM
install: checkcurrysystem runtime
	$(CPM) install --noexec
	$(MAKE) scripts
	$(MAKE) $(COMPILER)
	$(MAKE) $(REPL)
	$(MAKE) $(COMPDISTGO)

# Check validity of variable CURRYSYSTEM
.PHONY: checkcurrysystem
checkcurrysystem:
ifneq ($(shell test -x "$(CURRYSYSTEM)" ; echo $$?),0)
	@echo "Executable defined by CURRYSYSTEM not found!"
	@echo "Please redefine variable CURRYSYSTEM in Makefile!"
	@exit 1
endif

.PHONY: compiler
compiler: checkcurrysystem $(COMPILER)

$(COMPILER): src/CompilerStructure.curry src/Curry2Go/Compiler.curry \
             src/Curry2Go/Main.curry src/Curry2Go/*Config.curry
	$(CPM) -d BININSTALLPATH=$(BINDIR) install -x curry2goc

.PHONY: repl
repl: checkcurrysystem $(REPL)

$(REPL): src/Curry2Go/REPL.curry src/Curry2Go/*Config.curry
	$(CPM) -d BININSTALLPATH=$(BINDIR) install -x curry2goi

# Generate the implementation of externals of Curry.Compiler.Distribution
$(COMPDISTGO): checkcurrysystem src/Install.curry src/Curry2Go/PkgConfig.curry
	$(CPM) curry :load Install :eval main :quit

# Bootstrap, i.e., compile the compiler and REPL with existing Curry2Go compiler
# Saves existing executables in $(LOCALBIN)
.PHONY: bootstrap
bootstrap: $(COMPILER) $(REPL)
	mkdir -p $(LOCALBIN)
	cp -p $(COMPILER) $(LOCALBIN)/curry2goc
	$(CPMC2G) -d BININSTALLPATH=$(BINDIR) install -x curry2goc
	cp -p $(REPL) $(LOCALBIN)/curry2goi
	$(CPMC2G) -d BININSTALLPATH=$(BINDIR) install -x curry2goi

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

# The root of the Curry system used to install this package
CURRYSYSTEMROOT = $(shell $(CPM) -v quiet curry :set v0 :l Curry.Compiler.Distribution :eval installDir :q)

# install scripts in the bin directory:
.PHONY: scripts
scripts:
	cd scripts && $(MAKE) all
	cd $(BINDIR) && $(RM) -f curry curry2go-frontend
	# add alias `curry`:
	cd $(BINDIR) && ln -s curry2go curry
	# add alias for frontend:
	ln -s $(CURRYSYSTEMROOT)/bin/*-frontend bin/curry2go-frontend

# remove scripts in the bin directory:
.PHONY: cleanscripts
cleanscripts:
	cd scripts && $(MAKE) clean
	cd $(BINDIR) && $(RM) -f curry curry2go-frontend

# clean compilation targets
.PHONY: clean
clean:
	$(CPM) clean
	$(RM) -rf $(LOCALBIN) $(COMPILER) $(REPL) $(COMPDISTGO)

# clean all installed components
.PHONY: cleanall
cleanall: clean cleanscripts
	$(RM) -rf $(BINDIR) $(GOWORKSPACE)

