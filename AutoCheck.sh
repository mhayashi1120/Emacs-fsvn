#!/bin/sh
#
# Test tool
#

makefile=Makefile.tmp
cfgfile=MAKE-CFG.el.tmp

function check ()
{
	bin_dir=${1%/}
	emacs_command=${2}
	sample=${3}

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

	rm -f ${makefile}

	while read line ; do
		if echo "${line}" | grep -q -e "^ *EMACS[ 	]*=" ; then
			echo "EMACS	= ${emacs_command}" >> ${makefile}
		elif echo "${line}" | grep -q -e "^ *CONFIG[ 	]*=" ; then
			echo "CONFIG  = ${cfgfile}" >> ${makefile}
		else
			echo "${line}" >> ${makefile}
		fi
	done < ${sample}

	make -f ${makefile} check || exit 1
}

# for Unix (Linux)
UNIX_SVN_1_4_BIN=~/src/subversion-dev/inst-1.4.x/bin
UNIX_SVN_1_5_BIN=~/src/subversion-dev/inst-1.5.x/bin
UNIX_SVN_1_6_BIN=~/src/subversion-dev/inst-1.6.x/bin
EMACS_BIN=emacs
EMACS_CURRENT_BIN=emacs-current

# for Windows
# TODO not works
# WIN_SVN_1_4_BIN="c:/usr/local/subversion-dev/svn-win32-1.4.6/bin"
# WIN_SVN_1_5_BIN="c:/usr/local/subversion-dev/svn-win32-1.5.6/bin"
WIN_SVN_1_6_BIN="c:/usr/local/subversion-dev/svn-win32-1.6.6/bin"
MEADOW3_BIN="/cygdrive/c/usr/local/Meadow-dev/bin/Meadow.exe"
NTEMACS22_BIN="/cygdrive/c/usr/local/NTEmacs/22.2/bin/emacs.exe"
NTEMACS23_BIN="/cygdrive/c/usr/local/NTEmacs/23.1/bin/emacs.exe"

check /usr/bin ${EMACS_BIN} Makefile
check ${UNIX_SVN_1_4_BIN} ${EMACS_BIN} Makefile
check ${UNIX_SVN_1_4_BIN} ${EMACS_CURRENT_BIN} Makefile
check ${UNIX_SVN_1_5_BIN} ${EMACS_BIN} Makefile
check ${UNIX_SVN_1_5_BIN} ${EMACS_CURRENT_BIN} Makefile
check ${UNIX_SVN_1_6_BIN} ${EMACS_BIN} Makefile
check ${UNIX_SVN_1_6_BIN} ${EMACS_CURRENT_BIN} Makefile

check ${WIN_SVN_1_4_BIN} ${MEADOW3_BIN} Samples/Makefile.mw32
check ${WIN_SVN_1_5_BIN} ${MEADOW3_BIN} Samples/Makefile.mw32
check ${WIN_SVN_1_6_BIN} ${MEADOW3_BIN} Samples/Makefile.mw32
check ${WIN_SVN_1_4_BIN} ${NTEMACS22_BIN} Samples/Makefile.nt
check ${WIN_SVN_1_5_BIN} ${NTEMACS22_BIN} Samples/Makefile.nt
check ${WIN_SVN_1_6_BIN} ${NTEMACS22_BIN} Samples/Makefile.nt
check ${WIN_SVN_1_4_BIN} ${NTEMACS23_BIN} Samples/Makefile.nt23
check ${WIN_SVN_1_5_BIN} ${NTEMACS23_BIN} Samples/Makefile.nt23
check ${WIN_SVN_1_6_BIN} ${NTEMACS23_BIN} Samples/Makefile.nt23

make clean -f ${makefile}

rm -f ${makefile} ${cfgfile}

