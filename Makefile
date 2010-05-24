# -*- mode: makefile; -*-
#
# Makefile for fsvn
#

TAR	= tar
RM	= /bin/rm -f

EMACS	= emacs

CONFIG  = MAKE-CFG.el

FLAGS   = -batch -q -no-site-file -l fsvn-make.el

include MAKE-TARGETS

