##############################################################################
# Installation of PAKCS shell scripts
##############################################################################

# the scripts to be installed
BINS = $(BINDIR)/curry2go


.PHONY: all
all:
	$(MAKE) $(BINS)
	cd $(BINDIR) && ln -s curry2go curry

# install some script of PAKCS in the bin directory:
$(BINDIR)/%: %.sh
	mkdir -p $(@D)
	cat $< | \
	 sed "s|^CURRY2GOHOME=.*$$|CURRY2GOHOME=$(ROOT)|"  > $@
	chmod 755 $@

.PHONY: clean
clean:
	rm -f $(BINS)
