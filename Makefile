# -*- mode: makefile; -*-
#
# Makefile for fsvn
#

include VERSION

TAR	= tar
RM	= /bin/rm -f

EMACS	= emacs

CONFIG  = MAKE-CFG.el

FLAGS   = -batch -q -no-site-file -l fsvn-make.el

GOMI	= *.elc *~

ARCHIVE_DIR_PREFIX = ..

include MAKE-TARGETS

