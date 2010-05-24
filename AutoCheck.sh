#!/bin/sh

makefile=Makefile.tmp
cfgfile=MAKE-CFG.el.tmp

function check ()
{
	bin_dir=${1%/}
	emacs_command=${2}

	if [ -z "${emacs_command}" ] ; then
		return;
	fi

	if ! which "${emacs_command}" > /dev/null 2>&1 ; then
		echo "Skipping ${emacs_command}"
		return
	fi

	if [ ! -d ${bin_dir} ] ; then
		echo "Skipping ${bin_dir}"
		return
	fi

	svn_command=${bin_dir}/svn
	svnadmin_command=${bin_dir}/svnadmin

	cat > ${cfgfile} <<EOF
(setq fsvn-svn-command "${svn_command}")
(setq fsvn-svnadmin-command "${svnadmin_command}")
EOF

	cat > ${makefile} <<EOF
TAR	= tar
RM	= /bin/rm -f
EMACS	= ${emacs_command}
CONFIG  = ${cfgfile}
FLAGS   = -batch -q -no-site-file -l fsvn-make.el
include MAKE-TARGETS
EOF

	make -f ${makefile} check || exit 1
}

# for Unix (Linux)
UNIX_SVN_1_4_BIN=~/src/subversion-old/inst-1.4.x/bin
UNIX_SVN_1_5_BIN=~/src/subversion-old/inst-1.5.x/bin
UNIX_SVN_1_6_BIN=~/src/subversion-old/inst-1.6.x/bin
EMACS_BIN=emacs
EMACS_CURRENT_BIN=emacs-current

# for Windows
# WIN_SVN_1_4_BIN="c:/usr/local/subversion-old/svn-win32-1.4.6/bin"
# WIN_SVN_1_5_BIN="c:/usr/local/subversion-old/svn-win32-1.5.6/bin"
WIN_SVN_1_6_BIN="c:/usr/local/subversion-old/svn-win32-1.6.6/bin"
MEADOW3_BIN="/cygdrive/c/usr/local/Meadow-dev/bin/Meadow.exe"
# TODO NTEmacs not works
# NTEMACS22_BIN="/cygdrive/c/usr/local/NTEmacs/22.2/bin/emacs.exe"
# NTEMACS23_BIN="/cygdrive/c/usr/local/NTEmacs/23.1/bin/emacs.exe"

check /usr/bin ${EMACS_BIN}
check ${UNIX_SVN_1_4_BIN} ${EMACS_BIN}
check ${UNIX_SVN_1_4_BIN} ${EMACS_CURRENT_BIN}
check ${UNIX_SVN_1_5_BIN} ${EMACS_BIN}
check ${UNIX_SVN_1_5_BIN} ${EMACS_CURRENT_BIN}
check ${UNIX_SVN_1_6_BIN} ${EMACS_BIN}
check ${UNIX_SVN_1_6_BIN} ${EMACS_CURRENT_BIN}

check ${WIN_SVN_1_4_BIN} ${MEADOW3_BIN}
check ${WIN_SVN_1_5_BIN} ${MEADOW3_BIN}
check ${WIN_SVN_1_6_BIN} ${MEADOW3_BIN}
check ${WIN_SVN_1_4_BIN} ${NTEMACS22_BIN}
check ${WIN_SVN_1_5_BIN} ${NTEMACS22_BIN}
check ${WIN_SVN_1_6_BIN} ${NTEMACS22_BIN}
check ${WIN_SVN_1_4_BIN} ${NTEMACS23_BIN}
check ${WIN_SVN_1_5_BIN} ${NTEMACS23_BIN}
check ${WIN_SVN_1_6_BIN} ${NTEMACS23_BIN}

rm -f ${makefile} ${cfgfile}
