# Makefile to install all components of the Curry2Go compiler

# The name of a Curry system used for generating the initial compiler
export CURRYSYSTEM := $(shell which pakcs)

# Should the front end be built from the front-end repository sources?
export BUILDFRONTEND = no

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

# set GOPATH to directory containing run-time auxiliaries
export GOPATH=$(ROOT)/go
# required for Go version 1.16 or higher:
export GO111MODULE=auto

# CPM with compiler specified by CURRYSYSTEM
CPM = $(CPMBIN) -d CURRYBIN=$(CURRYSYSTEM)
# CPM with Curry2Go compiler
CPMC2G = $(CPMBIN) -d CURRYBIN=$(BINDIR)/curry2go

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
	$(MAKE) frontend
	$(MAKE) $(COMPILER)
	$(MAKE) $(REPL)
	$(MAKE) $(COMPDISTGO)

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

# Bootstrap the system, i.e., first install the Curry2Go system (compiler and REPL)
# with CURRYSYSTEM and then compile the compiler and REPL with installed Curry2Go compiler.
# Saves existing executables in $(LOCALBIN).
.PHONY: bootstrap
bootstrap:
	$(MAKE) install
	mkdir -p $(LOCALBIN)
	cp -p $(COMPILER) $(LOCALBIN)/curry2goc
	touch lib/Curry/Compiler/Distribution.curry # enforce recompilation
	$(CPMC2G) -d BININSTALLPATH=$(BINDIR) install -x curry2goc
	cp -p $(REPL) $(LOCALBIN)/curry2goi
	$(CPMC2G) -d BININSTALLPATH=$(BINDIR) install -x curry2goi

# install base libraries from package `base`:
.PHONY: baselibs
baselibs: require-jq
	$(RM) -rf base
	$(CPM) checkout base
	@if [ "$(shell $(JQ) -r '.version' base/package.json)" != "$(shell cat base/VERSION)" ] ; then \
	   echo "Corrupted base package: version different from file VERSION!" ; \
	   exit 1 ; fi
	$(RM) -rf $(LIBDIR)
	/bin/cp -r base/src $(LIBDIR)
	/bin/cp base/VERSION $(LIBDIR)/VERSION
	$(RM) -rf base

.PHONY: uninstall
uninstall:
	$(CPM) uninstall

# install scripts in the bin directory:
.PHONY: scripts
scripts:
	$(MAKE) -C scripts all
	cd $(BINDIR) && $(RM) -f curry curry2go-frontend
	# add alias `curry`:
	cd $(BINDIR) && ln -s curry2go curry

# install the front end:
.PHONY: frontend
frontend:
ifeq ($(BUILDFRONTEND),yes)
	$(MAKE) buildfrontend
else
	$(MAKE) copyfrontend
endif

# Copy the front end from the Curry system used to install this package:
FRONTENDREPO=https://git.ps.informatik.uni-kiel.de/curry/curry-frontend.git
.PHONY: copyfrontend
copyfrontend:
	$(RM) -f bin/curry2go-frontend
	cp -p $(shell $(CPM) -v quiet curry :set v0 :l Curry.Compiler.Distribution :eval installDir :q)/bin/*-frontend bin/curry2go-frontend

# Build and install the Curry front end from the repository:
FRONTENDREPO=https://git.ps.informatik.uni-kiel.de/curry/curry-frontend.git
.PHONY: buildfrontend
buildfrontend:
	$(RM) -rf frontend bin/curry2go-frontend
	git clone $(FRONTENDREPO) frontend
	$(MAKE) -C frontend
	cd bin && ln -s ../frontend/bin/curry-frontend curry2go-frontend

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
	$(MAKE) -C scripts clean
	$(RM) -f $(BINDIR)/curry

# clean compilation targets
.PHONY: cleantargets
cleantargets:
	$(CPM) clean
	$(CPMC2G) clean
	$(RM) -rf $(LIBDIR)/.curry
	$(RM) -rf $(LOCALBIN) $(COMPILER) $(REPL) $(COMPDISTGO)

# clean all installed components
.PHONY: clean
clean: cleantargets cleanscripts
	$(RM) -rf $(BINDIR)/curry2go-frontend frontend

##############################################################################
# distributing

# Executable of JSON command-line processor:
JQ := $(shell which jq)

.PHONY: require-jq
require-jq:
	@if [ ! -x "$(JQ)" ] ; then \
		echo "Tool 'jq' not found!" ; \
		echo "Install it, e.g.,  by 'sudo apt install jq'" ; \
		exit 1 ; fi

# ...in order to get the version number from package specification:
C2GVERSION = $(shell $(JQ) -r '.version' package.json)

# tar file to store the distribution
TARFILE=curry2go.tgz # curry2go-$(C2GVERSION).tgz

# URL of the Curry2Go repository:
GITURL=https://git.ps.informatik.uni-kiel.de/curry/curry2go.git

# CPM with distribution compiler
CPMDISTC2G = $(CPMBIN) -d CURRYBIN=$(C2GDISTDIR)/bin/curry2go

# the location where the distribution is built:
C2GDISTDIR=/tmp/Curry2Go

$(TARFILE):
	mkdir -p $(C2GDISTDIR)
	$(RM) -rf $(C2GDISTDIR) $(TARFILE)
	git clone $(GITURL) $(C2GDISTDIR)
	$(MAKE) -C $(C2GDISTDIR) bootstrap
	cd $(C2GDISTDIR) && $(CPMDISTC2G) checkout cpm
	cd $(C2GDISTDIR)/cpm && $(CPMDISTC2G) -d BININSTALLPATH=$(C2GDISTDIR)/bin install
	cd $(C2GDISTDIR)/lib && ../scripts/compile-all-libs.sh
	$(MAKE) -C $(C2GDISTDIR) cleandist
	cd $(C2GDISTDIR)/.. && tar cfvz $(ROOT)/$(TARFILE) Curry2Go

# Clean all files that should not be included in a distribution
.PHONY: cleandist
cleandist:
	$(RM) -rf .git .gitignore
	$(RM) -rf $(LOCALBIN) $(COMPILER) $(REPL) bin/cypm
	cd benchmarks && $(RM) -f bench.sh *.curry BENCHRESULTS.csv

# publish a current distribution
# the local HTML directory containing the distribution:
LOCALURL=$(HOME)/public_html/curry2go

.PHONY: dist
dist: require-jq
	$(MAKE) C2GDISTDIR=/opt/Curry2Go/Curry2Go $(TARFILE) && mv $(TARFILE) opt-$(TARFILE)
	$(MAKE) C2GDISTDIR=/tmp/Curry2Go          $(TARFILE) && mv $(TARFILE) tmp-$(TARFILE)
	cd $(LOCALURL) && $(RM) -f tmp-$(TARFILE) opt-$(TARFILE)
	cp tmp-$(TARFILE) opt-$(TARFILE) $(LOCALURL)/
	cp goinstall/download.sh $(LOCALURL)/
	$(MAKE) savedist
	chmod -R go+rX $(LOCALURL)

# save the current distribution files:
SAVEDISTPREFIX=ALLDISTS/`date -I`-$(C2GVERSION)
.PHONY: savedist
savedist:
	cd $(LOCALURL) && cp opt-$(TARFILE) $(SAVEDISTPREFIX)-opt-$(TARFILE)
	cd $(LOCALURL) && cp tmp-$(TARFILE) $(SAVEDISTPREFIX)-tmp-$(TARFILE)
	cd $(LOCALURL) && cp download.sh    $(SAVEDISTPREFIX)-download.sh

# install Curry2Go from the tar file of the distribution
.PHONY: installdist
installdist:
	# Compile the Curry2Go compiler
	cp goinstall/Compiler.go .curry/curry2go-*/
	cd .curry/curry2go-*/ && go build Compiler.go
	mv .curry/curry2go-*/Compiler bin/curry2goc
	# Compile the Curry2Go REPL
	cp goinstall/REPL.go .curry/curry2go-*/
	cd .curry/curry2go-*/ && go build .curry/curry2go-*/REPL.go
	mv .curry/curry2go-*/REPL bin/curry2goi
	# install CPM
	cp goinstall/CPM.go cpm/.curry/curry2go-*/
	cd cpm/.curry/curry2go-* && go build CPM.go
	mv .curry/curry2go-*/cpm/CPM bin/cypm
ifeq ($(BUILDFRONTEND),yes)
	$(MAKE) buildfrontend
endif

##############################################################################
