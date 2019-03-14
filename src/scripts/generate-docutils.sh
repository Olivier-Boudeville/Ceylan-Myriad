#!/bin/sh

# Copyright (C) 2010-2019 Olivier Boudeville
#
# Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]

# This file is part of the Ceylan-Myriad library.


# Notes:
#
#  - docutils has been finally preferred to txt2tags
#
#  - beware of not running more than one instance of this script against the
# same RST source file: they would trigger at the same time (file saved) and
# each would remove the temporary files of the others, leading to spurious
# errors...



USAGE="Usage: $(basename $0) <target rst file> [ --pdf | --all | <comma-separated path(s) to CSS file to be used, ex: common/css/XXX.css,other.css> ] [--icon-file ICON_FILENAME]

Updates specified file from more recent docutils source (*.rst).
If '--pdf' is specified, a PDF will be created, if '--all' is specified, all output formats (i.e. HTML and PDF) will be created, otherwise HTML files only will be generated, using any specified CSS file.
"


# Left out: --warnings=rst-warnings.txt --traceback --verbose --debug
# Can be removed for debugging: --quiet
docutils_common_opt="--report=error --tab-width=4 --no-generator --no-datestamp --no-source-link --strip-comments --syntax-highlight=short"


# Obtained from 'rst2html -h':
#
docutils_html_opt="${docutils_common_opt} --cloak-email-addresses --link-stylesheet --no-section-numbering"


# Obtained from 'rst2latex -h':
#

doc_class=article
#doc_class=report

docutils_pdf_opt="${docutils_common_opt} --documentclass=${doc_class} --compound-enumerators"


latex_to_pdf_opt="-interaction nonstopmode"

begin_marker="---->"

# By default, generate HTML and not PDF:
do_generate_html=0
do_generate_pdf=1


docutils_opt="${docutils_html_opt}"

docutils_html=$(which rst2html 2>/dev/null)


if [ -z "$1" ] ; then
	echo "Error: no parameter given. $USAGE" 1>&2
	exit 1
fi

rst_file="$1"

if [ -e "${rst_file}" ] ; then

	shift

	if [ "$1" = "--pdf" ] ; then

		do_generate_html=1
		do_generate_pdf=0
		shift

	elif [ "$1" = "--all" ] ; then

		do_generate_html=0
		do_generate_pdf=0
		shift

		css_file="$1"
		shift

		if [ -n "${css_file}" ] ; then
			#echo "Using CSS file ${css_file}."
			css_opt="--stylesheet-path=${css_file} --link-stylesheet"
		fi

		docutils_html_opt="${docutils_html_opt} ${css_opt}"

	else

		css_file="$1"
		shift

		if [ -n "${css_file}" ] ; then
			#echo "Using CSS file ${css_file}."
			css_opt="--stylesheet-path=${css_file}"
		fi

		docutils_html_opt="${docutils_html_opt} ${css_opt}"

	fi

else

	echo "${begin_marker} Error: file '$1' not found. $USAGE" 1>&2
	exit 2

fi


if [ "$1" = "--icon-file" ] ; then

	shift

	icon_file="$1"
	shift

	if [ ! -f "${icon_file}" ] ; then

		echo "  Error, specified icon file (${icon_file}) does not exist." 1>&2
		exit 55

	else

		echo "  Using icon file '${icon_file}'."

	fi

else

	if [ -n "$1" ] ; then

		echo "  Error, unexpected parameter ($1)." 1>&2
		exit 60

	fi

fi



if [ $do_generate_html -eq 0 ] ; then

	docutils_html=$(which rst2html 2>/dev/null)
	if [ -z "${docutils_html}" ] ; then

		echo "${begin_marker} Error: unable to find an executable tool to convert docutils files to HTML (rst2html)." 1>&2
		exit 10

	fi

fi

if [ $do_generate_pdf -eq 0 ] ; then

	docutils_latex=$(which rst2latex 2>/dev/null)
	if [ -z "${docutils_latex}" ] ; then

		echo "${begin_marker} Error: unable to find an executable tool to convert docutils files to LateX (rst2latex)." 1>&2
		exit 11

	fi

	latex_to_pdf=$(which pdflatex 2>/dev/null)
	if [ -z "${latex_to_pdf}" ] ; then

		echo "${begin_marker} Error: unable to find an executable tool to convert LateX files to PDF (pdflatex)." 1>&2
		exit 12

	fi

fi




manage_rst_to_html()
# $1: name of the .rst file to convert to HTML.
# $2: name of the HTML target file
{

	source="$1"
	target="$2"

	echo "${begin_marker} building HTML target ${target} from source"


	#echo ${docutils_html} ${docutils_html_opt} ${source} ${target}
	${docutils_html} ${docutils_html_opt} "${source}" "${target}"


	if [ ! $? -eq 0 ] ; then
		echo "${begin_marker} Error: HTML generation with ${docutils_html} failed for ${source}." 1>&2
		exit 5
	fi

	if [ ! -f "${target}" ] ; then

		echo "  Error, no target file '${target}' generated." 1>&2
		exit 60

	fi

	tmp_file=".tmp-${target}"

	if [ -n "${icon_file}" ] ; then

		echo "  Referencing the icon"

		/bin/cat "${target}" | sed "s|</head>$|<link href=\"${icon_file}\" rel=\"icon\">\n</head>|1" > "${tmp_file}"

		if [ ! -f "${tmp_file}" ] ; then

			echo "  Error, no temporary file '${tmp_file}' obtained." 1>&2
			exit 61

		fi

		/bin/mv -f "${tmp_file}" "${target}"

	fi

	echo "  Adding the viewport settings"

	# Apparently the viewport settings are strongly recommended in all cases,
	# for mobile support:
	#
	/bin/cat "${target}" | sed 's|</head>$|<meta name="viewport" content="width=device-width, initial-scale=1.0">\n</head>|1' > "${tmp_file}"

	if [ ! -f "${tmp_file}" ] ; then

		echo "  Error, no temporary file '${tmp_file}' obtained." 1>&2
		exit 62

	fi

	/bin/mv -f "${tmp_file}" "${target}"

	echo "  Generated file is: file://${PWD}/${target}"

}



manage_rst_to_pdf()
# $1: name of the .rst file to convert to PDF.
{

	source="$1"
	target="$2"

	echo "${begin_marker} building PDF target ${target} corresponding to source ${source}"

	# Never triggered:
	if [ -f "${target}" ]; then

		echo "  (removing pre-existing target '${target}')"

		/bin/rm -f "${target}"

	fi


	# Input extension is generally '.rst' (allows to remove only the final
	# extension, even if there were dots in the base name):
	tex_file=$( echo ${source} | sed 's|\.[^\.]*$|.tex|1' )


	#echo "Docutils command: ${docutils_latex} ${docutils_pdf_opt} ${source} ${tex_file}"

	${docutils_latex} ${docutils_pdf_opt} ${source} ${tex_file}
	res=$?

	if [ ! ${res} -eq 0 ] ; then

		if [ ${res} -eq 1 ] ; then
			echo "${begin_marker} Warning: LateX generation returned code 1 for ${source}." 1>&2
		else
			echo "${begin_marker} Error: LateX generation failed for ${source}." 1>&2
			exit 6
		fi

	fi

	if [ ! -e "${tex_file}" ] ; then
		echo "${begin_marker} Error: generated TeX file '${tex_file}' could not be found, probably due to RST errors." 1>&2
		exit 8

	fi

	rubber=$(which rubber)

	if [ ! -x "${rubber}" ] ; then

		echo "Warning: 'rubber' not found, using pdflatex directly." 1>&2

		# Run thrice on purpose, to fix links:
		#echo "LateX command: ${latex_to_pdf} ${latex_to_pdf_opt} ${tex_file}"

		${latex_to_pdf} ${latex_to_pdf_opt} ${tex_file} && \
		${latex_to_pdf} ${latex_to_pdf_opt} ${tex_file} && \
		${latex_to_pdf} ${latex_to_pdf_opt} ${tex_file}

	else

		# Our preferred build method:
		echo "Using rubber for PDF generation"

		${rubber} --pdf -v ${tex_file}

	fi

	res=$?

	if [ ! ${res} -eq 0 ] ; then

		if [ ${res} -eq 1 ] ; then

			echo "${begin_marker} Warning: PDF generation returned code 1 for ${source}." 1>&2

		else

			# We might receive exit statuses greater than 1, yet have the right
			# PDF produced:

			if [ -f "${target}" ]; then

				echo "${begin_marker} Error reported (code: ${res}), yet the PDF file ($(pwd)/${target}) could be nevertheless generated."

			else

				echo "${begin_marker} Error: PDF generation failed for ${source} (error code: ${res})." 1>&2
				exit 7

			fi

		fi

	fi

}




if [ ${do_generate_html} -eq 0 ] ; then

	target_html_file=$( echo ${rst_file} | sed 's|.rst$|.html|1' )
	#echo "target_html_file = ${target_html_file}"

	manage_rst_to_html ${rst_file} ${target_html_file}

fi


if [ ${do_generate_pdf} -eq 0 ] ; then

	target_pdf_file=$( echo ${rst_file} | sed 's|.rst$|.pdf|1' )
	#echo "target_pdf_file = ${target_pdf_file}"

	# PDF generator will not find includes (ex: images) if not already
	# in target dir:
	current_dir=$(pwd)
	target_dir=$(dirname ${target_pdf_file})

	source_file=$(basename ${rst_file})
	target_file=$(basename ${target_pdf_file})

	cd ${target_dir}
	manage_rst_to_pdf ${source_file} ${target_file}
	cd ${current_dir}

fi
