VERSION = 0.9.2

RELEASE_FILES = \
	fsvn*.el \
	mw32cmp.el mw32cmp-test.el mw32el-ntemacs.patch \
	Makefile MAKE-CFG.el MAKE-TARGETS.mk \
	BUG TODO ChangeLog

RELEASE_SAMPLES = \
	Samples/Makefile* Samples/MAKE-CFG.el*

ARCHIVE_DIR_PREFIX = ..

GOMI	= *.elc *~

default: elc

check: clean
	$(EMACS) $(CHECKFLAGS) -f check-fsvn $(CONFIG)

elc:
	$(EMACS) $(FLAGS) -f compile-fsvn $(CONFIG)

what-where:
	$(EMACS) $(FLAGS) -f what-where-fsvn $(CONFIG)

install: elc
	$(EMACS) $(FLAGS) -f install-fsvn $(CONFIG)

clean:
	-$(RM) $(GOMI)

release: archive single-file
	$(RM) -f $(ARCHIVE_DIR_PREFIX)/fsvn-$(VERSION).tar.bz2 $(ARCHIVE_DIR_PREFIX)/fsvn-$(VERSION).tar.gz
	$(RM) -f $(ARCHIVE_DIR_PREFIX)/fsvn.el
	$(RM) -f $(ARCHIVE_DIR_PREFIX)/fsvn.el.bz2
	$(RM) -f $(ARCHIVE_DIR_PREFIX)/fsvn.el.gz
	mv /tmp/fsvn-$(VERSION).tar.bz2 /tmp/fsvn-$(VERSION).tar.gz $(ARCHIVE_DIR_PREFIX)/
	mv fsvn.el.tmp $(ARCHIVE_DIR_PREFIX)/fsvn.el
	bzip2 --keep $(ARCHIVE_DIR_PREFIX)/fsvn.el
	gzip $(ARCHIVE_DIR_PREFIX)/fsvn.el

archive:
	rm -rf /tmp/fsvn-$(VERSION)
	mkdir /tmp/fsvn-$(VERSION)
	cp -pr $(RELEASE_FILES) /tmp/fsvn-$(VERSION)
	chmod 644 /tmp/fsvn-$(VERSION)/*
	mkdir /tmp/fsvn-$(VERSION)/Samples
	cp -pr $(RELEASE_SAMPLES) /tmp/fsvn-$(VERSION)/Samples
	chmod 644 /tmp/fsvn-$(VERSION)/Samples/*
	cd /tmp ; tar cjf fsvn-$(VERSION).tar.bz2 fsvn-$(VERSION)
	cd /tmp ; tar czf fsvn-$(VERSION).tar.gz fsvn-$(VERSION)

single-file:
	$(EMACS) $(FLAGS) -f make-fsvn $(CONFIG)
