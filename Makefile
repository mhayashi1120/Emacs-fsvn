# -*- mode: makefile; -*-
#
# Makefile for fsvn
#

TAR	= tar
RM	= /bin/rm -f

EMACS	= emacs

CONFIG  = MAKE-CFG.el

FLAGS   = -batch -q -no-site-file -l fsvn-make.el

# For windows emacs
CHECKFLAGS = $(FLAGS)

include MAKE-TARGETS.mk

