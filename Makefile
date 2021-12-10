# Makefile to install all components of the Curry2Go compiler

# The name of a Curry system used for generating the initial compiler
# (we try PAKCS if this parameter is not explicitly set)
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

# The file containing the compiler date
COMPDATEFILE=COMPDATE

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
	$(MAKE) $(COMPDATEFILE)
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
             src/Curry2Go/Main.curry src/Curry2Go/InstallPath.curry \
	     src/Curry2Go/*Config.curry
	$(CPM) -d BININSTALLPATH=$(BINDIR) install -x curry2goc

# Build the REPL
.PHONY: repl
repl: checkcurrysystem $(REPL)

$(REPL): src/Curry2Go/REPL.curry src/Curry2Go/InstallPath.curry \
	 src/Curry2Go/*Config.curry
	$(CPM) -d BININSTALLPATH=$(BINDIR) install -x curry2goi

# Initializing compiler date file with repository date
$(COMPDATEFILE):
	git log -1 --format="%ci" | cut -c-10 > $@

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
	$(RM) -rf $(LIBDIR)
	/bin/cp -r base/src $(LIBDIR)
	$(JQ) -r '.version' base/package.json > $(LIBDIR)/VERSION
	$(RM) -rf base

# pre-compile all libraries:
.PHONY: compilelibs
compilelibs:
	cd $(LIBDIR) && ../scripts/compile-all-libs.sh

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
	$(RM) -rf $(LOCALBIN) $(COMPILER) $(REPL) $(COMPDISTGO) $(COMPDATEFILE)

# clean all installed components
.PHONY: clean
clean: cleantargets cleanscripts
	$(RM) -rf $(BINDIR)/curry2go-frontend frontend

##############################################################################
# targets for generating a distribution

# get the version number from package specification:
C2GVERSION = $(shell $(JQ) -r '.version' package.json)

# tar file to store the distribution
TARFILE=curry2go.tgz

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
	$(MAKE) -C $(C2GDISTDIR) compilelibs
	$(MAKE) -C $(C2GDISTDIR) cleandist
	cd $(C2GDISTDIR) && tar cfvz $(ROOT)/$(TARFILE) .
	$(RM) -rf $(C2GDISTDIR)

# Clean all files that should not be included in a distribution
.PHONY: cleandist
cleandist:
	$(RM) -rf .git .gitignore .cpm src/.curry
	$(RM) -rf $(LOCALBIN) $(COMPILER) $(REPL) bin/cypm
	cd benchmarks && $(RM) -f bench.sh *.curry BENCHRESULTS.csv

##############################################################################
# publish a current distribution

# the local HTML directory containing the distribution
HTMLDIR=$(HOME)/public_html/curry2go

# the distribution prefix (current date and package version)
DISTDATE=$(shell date -I)
DISTPREFIX=$(DISTDATE)-$(C2GVERSION)

.PHONY: dist
dist: require-jq
	$(MAKE) $(TARFILE)
	cp $(TARFILE) $(HTMLDIR)/download/$(DISTPREFIX)-$(TARFILE)
	cat goinstall/download.sh | \
	   sed "s|^VERSION=.*$$|VERSION=$(DISTPREFIX)|" \
	     > $(HTMLDIR)/download.sh
	cd $(HTMLDIR) && cp download.sh download/$(DISTPREFIX)-download.sh
	$(MAKE) adddownloadref
	chmod -R go+rX $(HTMLDIR)

# The URL for downloads
DOWNLOADURL=https://www-ps.informatik.uni-kiel.de/curry2go/download
# The URL of the current download script
DOWNLOADSCRIPTURL=$(DOWNLOADURL)/$(DISTPREFIX)-download.sh
# The download HTML page
DOWNLOADPAGE=$(HTMLDIR)/download.html

# insert a new row containing a reference to this version in the download page
.PHONY: adddownloadref
adddownloadref: require-jq
	mv $(DOWNLOADPAGE) $(DOWNLOADPAGE).bak
	sed '/NEWESTDIST/a <tr><td>Version $(C2GVERSION) ($(DISTDATE))</td><td><kbd>curl -sSL $(DOWNLOADSCRIPTURL) | sh</kdb></td></tr>' < $(DOWNLOADPAGE).bak > $(DOWNLOADPAGE)

##############################################################################
# install Curry2Go from the tar file of the distribution

# command to adapt installation directory in a file
ADAPTINSTALLDIR=goinstall/adapt-installdir.sh "$(C2GDISTDIR)"

.PHONY: installdist
installdist:
	# Adapt installation directory in generated files
	@$(ADAPTINSTALLDIR) $(COMPDISTGO)
	@$(ADAPTINSTALLDIR) .curry/curry2go-*/Curry/Compiler/Distribution/Distribution_external.go
	@$(ADAPTINSTALLDIR) .curry/curry2go-*/Curry2Go/PkgConfig/PkgConfig.go
	@$(ADAPTINSTALLDIR) .curry/curry2go-*/REPL/PkgConfig/PkgConfig.go
	@$(ADAPTINSTALLDIR) .curry/curry2go-*/go.mod
	@$(ADAPTINSTALLDIR) cpm/.curry/curry2go-*/CPM/ConfigPackage/ConfigPackage.go
	@$(ADAPTINSTALLDIR) cpm/.curry/curry2go-*/go.mod
	# Compile the Curry2Go compiler
	cp goinstall/CompilerMain.go .curry/curry2go-*/
	cd .curry/curry2go-*/ && go build CompilerMain.go
	mv .curry/curry2go-*/CompilerMain bin/curry2goc
	# Compile the Curry2Go REPL
	cp goinstall/REPLMain.go .curry/curry2go-*/
	cd .curry/curry2go-*/ && go build REPLMain.go
	mv .curry/curry2go-*/REPLMain bin/curry2goi
	# install CPM
	cp goinstall/CPMMain.go cpm/.curry/curry2go-*/
	cd cpm/.curry/curry2go-* && go build CPMMain.go
	mv cpm/.curry/curry2go-*/CPMMain bin/cypm.bin
	cp goinstall/cypm bin/cypm
	chmod 755 bin/cypm
	cd $(BINDIR) && ln -s cypm curry2go-cypm
ifeq ($(BUILDFRONTEND),yes)
	$(MAKE) buildfrontend
endif
	# Make everything readable:
	chmod -R go+rX .

##############################################################################
# Required tools:

# Executable of JSON command-line processor:
JQ := $(shell which jq)

.PHONY: require-jq
require-jq:
	@if [ ! -x "$(JQ)" ] ; then \
		echo "Tool 'jq' not found!" ; \
		echo "Install it, e.g.,  by 'sudo apt install jq'" ; \
		exit 1 ; fi

##############################################################################
