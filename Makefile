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
GOCURRYWORKSPACE=$(HOME)/go/src/gocurry

# The generated compiler executable
COMPILER=$(BINDIR)/curry2goc

# The generated executable of the REPL
REPL=$(BINDIR)/curry2goi

# Remove command
RM=/bin/rm

# The Go implementation of the base module Curry.Compiler.Distribution
COMPDISTGO=lib/Curry/Compiler/Distribution_external.go

###############################################################################
# Installation

# Install the Curry2Go system (compiler and REPL) with CURRYSYSTEM
.PHONY: install
install: checkcurrysystem
	$(CPM) install --noexec
	$(MAKE) kernel

# Install the kernel of Curry2Go (compiler and REPL) with CURRYSYSTEM
# without installing all packages
.PHONY: kernel
kernel: checkcurrysystem
	$(MAKE) scripts
	$(MAKE) $(COMPILER)
	$(MAKE) $(REPL)
	$(MAKE) $(COMPDISTGO)
	$(MAKE) runtime # late creation: avoid conflicts with previous versions

# Check validity of variable CURRYSYSTEM
.PHONY: checkcurrysystem
checkcurrysystem:
ifneq ($(shell test -f "$(CURRYSYSTEM)" -a -x "$(CURRYSYSTEM)" ; echo $$?),0)
	@echo "'$(CURRYSYSTEM)' is not an executable!"
	@echo "Please redefine variable CURRYSYSTEM in Makefile!"
	@exit 1
endif

# Build the compiler
.PHONY: compiler
compiler: checkcurrysystem $(COMPILER)

$(COMPILER): src/CompilerStructure.curry src/Curry2Go/Compiler.curry \
             src/Curry2Go/Main.curry src/Curry2Go/*Config.curry
	$(CPM) -d BININSTALLPATH=$(BINDIR) install -x curry2goc

# Build the REPL
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
uninstall:
	$(CPM) uninstall
	$(RM) -rf $(GOCURRYWORKSPACE)

# install run-time libraries:
.PHONY: runtime
runtime:
	mkdir -p $(GOCURRYWORKSPACE)
	$(RM) -rf $(GOCURRYWORKSPACE)
	cp -r gocurry $(GOCURRYWORKSPACE)

# install scripts in the bin directory:
.PHONY: scripts
scripts:
	cd scripts && $(MAKE) all
	cd $(BINDIR) && $(RM) -f curry curry2go-frontend
	# add alias `curry`:
	cd $(BINDIR) && ln -s curry2go curry
	# copy the frontend executable of the Curry system
	# used to install this package:
	cp -p $(shell $(CPM) -v quiet curry :set v0 :l Curry.Compiler.Distribution :eval installDir :q)/bin/*-frontend bin/curry2go-frontend

##############################################################################
# testing

.PHONY: runtest
runtest:
	cd examples && ./test.sh

##############################################################################
# cleaning

# remove scripts in the bin directory:
.PHONY: cleanscripts
cleanscripts:
	cd scripts && $(MAKE) clean
	cd $(BINDIR) && $(RM) -f curry curry2go-frontend

# clean compilation targets
.PHONY: cleantargets
cleantargets:
	$(CPM) clean
	$(CPMC2G) clean
	$(RM) -rf $(LOCALBIN) $(COMPILER) $(REPL) $(COMPDISTGO)

# clean all installed components
.PHONY: clean
clean: cleantargets cleanscripts
	$(RM) -rf $(BINDIR) $(GOCURRYWORKSPACE)

##############################################################################
# distributing

TARFILE=$(ROOT)/tmpcurry2go.tgz
# put the distribution in a /tmp directory available on all machines:
TMPC2GDIR=/tmp/Curry2Go
# URL of the Curry2Go repository:
GITURL=https://git.ps.informatik.uni-kiel.de/curry/curry2go.git
# CPM with /tmp/Curry2Go compiler
CPMTMPC2G = $(CPMBIN) -d CURRYBIN=$(TMPC2GDIR)/bin/curry2go

$(TARFILE):
	$(RM) -rf $(TMPC2GDIR) $(TARFILE)
	git clone $(GITURL) $(TMPC2GDIR)
	cd $(TMPC2GDIR) && $(MAKE) && $(MAKE) bootstrap
	cd $(TMPC2GDIR) && $(CPMTMPC2G) checkout cpm
	cd $(TMPC2GDIR)/cpm && $(CPMTMPC2G) -d BININSTALLPATH=$(TMPC2GDIR)/bin install
	cd $(TMPC2GDIR)  && $(MAKE) distclean
	cd $(TMPC2GDIR)/.. && tar cfvz $(TARFILE) Curry2Go

.PHONY: distclean
distclean:
	$(RM) -rf .git $(LOCALBIN) $(COMPILER) $(REPL) bin/cypm
	cd benchmarks && $(RM) -f bench.sh *.curry BENCHRESULTS.csv

# publish a current distribution
# the local HTML directory containing the distribution:
LOCALURL=$(HOME)/public_html/curry2go

.PHONY: dist
dist: $(TARFILE)
	cp $(TARFILE) $(LOCALURL)/
	cp goinstall/download.sh $(LOCALURL)/download
	chmod -R go+rX $(LOCALURL)


# install from the tar file of the distribution
.PHONY: installdist
installdist: runtime
	# Compile the Curry2Go compiler
	cp goinstall/Compiler.go .curry/curry2go-*/
	go build .curry/curry2go-*/Compiler.go
	mv Compiler bin/curry2goc
	# Compile the Curry2Go REPL
	cp goinstall/REPL.go .curry/curry2go-*/
	go build .curry/curry2go-*/REPL.go
	mv REPL bin/curry2goi
	# install CPM
	cp goinstall/CPM.go cpm/.curry/curry2go-*/
	cd cpm && go build .curry/curry2go-*/CPM.go
	mv cpm/CPM bin/cypm
	@echo "Add '$(ROOT)/bin' to your path to use the Curry2Go system!"

##############################################################################
