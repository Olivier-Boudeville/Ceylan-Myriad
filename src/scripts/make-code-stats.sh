#!/bin/sh

# Copyright (C) 2010-2016 Olivier Boudeville (olivier.boudeville@esperide.com)

#
# This file is part of the Ceylan Erlang library.


USAGE="Usage: $(basename $0) SOURCE_DIRECTORY
Evaluates various simple metrics of the Erlang code found from specified root directory.
"

BC=$(which bc 2>/dev/null)

if [ ! -x "${BC}" ] ; then

	echo "  Error, 'bc' command not found. Hint: on Arch Linux, use 'pacman -S bc' as root." 1>&2

	exit 5

fi

root_dir=$1

if [ -z "$root_dir" ] ; then

	echo "  Error, no root directory specified.
$USAGE" 1>&2
	exit 10

fi


if [ ! -d "$root_dir" ] ; then

	echo "  Error, specified root directory ($root_dir) does not exist.
$USAGE" 1>&2
	exit 15

fi


if [ -n "$2" ] ; then

	echo "  Error, only one parameter needed.
$USAGE" 1>&2
	exit 20

fi


cd $root_dir

# Sometimes in /bin, sometimes in /usr/bin, etc.:
FIND=$(which find 2>/dev/null)
WC=$(which wc 2>/dev/null)


# -L: follow symlinks.

hrl_files=$(${FIND} -L . -name '*.hrl')
erl_files=$(${FIND} -L . -name '*.erl')


hrl_count=$(echo ${hrl_files} | ${WC} -w)
erl_count=$(echo ${erl_files} | ${WC} -w)

tmp_file=".tmp-code-stats.txt"

if [ -f "${tmp_file}" ] ; then

	/bin/rm -f "${tmp_file}"

fi


target_files="${hrl_files} ${erl_files}"
#echo "target_files = $target_files"


if [ "$target_files" = " " ] ; then

	echo "  Error, no Erlang source file found from '$root_dir'." 1>&2
	exit 25

fi

for f in ${target_files} ; do

	cat $f >> "${tmp_file}"

done


full_line_count=$(cat "${tmp_file}" | wc -l)

if [ $full_line_count -eq 0 ] ; then

	echo "  Error, no Erlang source code found from '$root_dir'." 1>&2
	exit 35

fi


empty_line_count=$(/bin/cat "${tmp_file}" | /bin/grep '^$' | ${WC} -l)
comment_line_count=$(/bin/cat "${tmp_file}" | /bin/grep '^[[:space:]]*%' | ${WC} -l)

code_line_count=$(/bin/expr $full_line_count - $empty_line_count - $comment_line_count)

empty_percentage=$(echo "scale=1; 100 * $empty_line_count / $full_line_count" | bc)
comment_percentage=$(echo "scale=1; 100 * $comment_line_count / $full_line_count" | bc)
code_percentage=$(echo "scale=1 ; 100 * $code_line_count / $full_line_count" | bc)

echo "In the Erlang source code found from $root_dir, we have:"
echo "  + $erl_count source files (*.erl), $hrl_count header files (*.hrl)"
echo "  + a grand total of ${full_line_count} lines:"

echo "    - $empty_line_count of which (${empty_percentage}%) are blank lines"
echo "    - $comment_line_count of which (${comment_percentage}%) are comments"
echo "    - $code_line_count of which (${code_percentage}%) are code"


if [ -f "${tmp_file}" ] ; then

	/bin/rm -f "${tmp_file}"

fi
