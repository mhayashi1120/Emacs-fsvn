VERSION = 0.9.12

RELEASE_FILES = \
	fsvn-admin.el fsvn-blame.el fsvn-browse.el fsvn-cmd.el fsvn-config.el \
	fsvn-data.el fsvn-debug.el fsvn-deps.el fsvn-dev.el fsvn-diff.el \
	fsvn-dired.el fsvn-env.el fsvn-fs.el fsvn-logview.el fsvn-magic.el \
	fsvn-make.el fsvn-minibuf.el fsvn-mode.el fsvn-msgedit.el \
	fsvn-parasite.el fsvn-pkg.el fsvn-popup.el fsvn-proc.el \
	fsvn-proclist.el fsvn-propview.el fsvn-pub.el fsvn-select.el \
	fsvn-svk.el fsvn-test.el fsvn-tortoise.el fsvn-ui.el fsvn-url.el \
	fsvn-win.el fsvn-xml.el fsvn.el \
	mw32cmp.el mw32cmp-test.el Makefile \
	MAKE-CFG.el MAKE-TARGETS.mk BUG INSTALL README TODO ChangeLog


RELEASE_SAMPLES = \
	Samples/Makefile* Samples/MAKE-CFG.el*

RELEASE_IMAGES = \
	images/*.xpm

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

uninstall:
	$(EMACS) $(FLAGS) -f uninstall-fsvn $(CONFIG)

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
	cp -p $(RELEASE_SAMPLES) /tmp/fsvn-$(VERSION)/Samples
	mkdir /tmp/fsvn-$(VERSION)/images
	cp -p $(RELEASE_IMAGES) /tmp/fsvn-$(VERSION)/images
	chmod 744 /tmp/fsvn-$(VERSION)/Samples
	cd /tmp ; tar cjf fsvn-$(VERSION).tar.bz2 fsvn-$(VERSION)
	cd /tmp ; tar czf fsvn-$(VERSION).tar.gz fsvn-$(VERSION)

single-file:
	$(EMACS) $(FLAGS) -f make-fsvn $(CONFIG)
