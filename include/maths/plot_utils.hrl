% Copyright (C) 2023-2023 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Saturday, October 7, 2023.


% Defaults:

-define( default_canvas_width, 1600 ).
-define( default_canvas_height, 800 ).


% File extensions:

-define( command_extension, "p" ).
-define( data_extension, "dat" ).

-define( plot_extension, "png" ).
%-define( plot_extension, "svg" ).



% The settings governing a given plot.
-record( plot_settings, {

	% The name of this plot, whence for example the corresponding file names
	% will be derived.
	%
	name :: text_utils:bin_string(),


	% The title, if any, to display on this plot.
	title :: maybe( text_utils:bin_title() ),

	% Key (legend) options (as a binary):
	key_options :: text_utils:bin_string(),


	% Label for the abscissa axis (as a binary):
	x_label :: text_utils:bin_string(),

	% Label for the ordinate axis (as a binary):
	y_label :: text_utils:bin_string(),


	% Settings for tick layout along the abscissa axis (as a binary):
	x_tick :: text_utils:bin_string(),

	% Settings for tick layout along the ordinate axis (as a binary):
	y_tick :: text_utils:bin_string(),


	% Abscissa range (pair of {MaybeMinX,MaybeMaxX} integers, or 'undefined'),
	% knowing that such a range can be open, if either of the bounds is not
	% specified (e.g. resulting in a "[5:]" range):
	%
	x_range :: maybe(
		{ maybe( gui:coordinate() ), maybe( gui:coordinate() ) } ),

	% Ordinate range (pair of {MaybeMinY,MaybeMaxY} integers, or 'undefined'),
	% knowing that such a range can be open, if either of the bounds is not
	% specified (e.g. resulting in a "[5:]" range):
	%
	y_range :: maybe(
		{ maybe( gui:coordinate() ), maybe( gui:coordinate() ) } ),


	% Fine control of the major (labeled) ticks on the abscissa axis (as a
	% binary):
	%
	x_ticks :: maybe( text_utils:bin_string() ),


	% Tells whether the abscissa axis gathers timestamps.
	is_timestamped = false :: boolean(),

	% The display time format to use if the x axis is a timestamped one:
	x_ticks_timestamp_time_format ::
		maybe( plot_utils:timestamp_time_format() ),


	% Fine control of the major (labeled) ticks on the ordinate axis (as a
	% binary):
	%
	y_ticks :: maybe( text_utils:bin_string() ),


	% Defines how graphs should be rendered (as a binary):
	%
	% (our default is linespoints, in order to add - compared to mere lines - a
	% graphical symbol on top of each data point)
	%
	plot_style :: text_utils:bin_string(),


	% Defines the size of each point; 'set pointsize 2' means the point size is
	% twice the default size.
	%
	point_size = 1 :: non_neg_integer(),


	% Defines how areas like histograms should be filled (as a binary):
	fill_style :: text_utils:bin_string(),


	% Defines the width of the canvas, i.e. the actual width, in pixels, of the
	% corresponding plot:
	%
	canvas_width = ?default_canvas_width :: gui:length(),

	% Defines the height of the canvas, i.e. the actual height, in pixels, of
	% the corresponding plot:
	%
	canvas_height = ?default_canvas_height :: gui:length(),


	% The default image format for probe rendering (as a binary):
	image_format = <<?plot_extension>> :: text_utils:bin_string(),

	% Lists the arbitrary labels that may be defined over the probe rendering:
	labels = [] :: [ plot_utils:plot_label() ],

	% Lists extra defines that shall be added verbatim to the command file (near
	% its top):
	%
	extra_defines = [] :: [ text_utils:bin_string() ],


	% The directory (if any; otherwise the current working directory will be
	% used) in which the plot is to be generated.
	%
	plot_directory :: maybe( file_utils:bin_directory_name() ),

	% The filename (if any; otherwise it will be generated) to the plot that is
	% to be generated.
	%
	plot_filename :: maybe( file_utils:bin_file_name() ),


	% Internals:

	% An ordered list of {CurveIndex, BinCurveName, BinPlotSuffix} triplets,
	% with CurveIndex keeping track of the order according to which the curves
	% were declared and fed (so that, prior to generating a report, curves can
	% be reordered while being still associated to their values), and with curve
	% names being binaries; the order in this list dictates the actual rendering
	% order of curves that will be performed.
	%
	curve_entries :: [ plot_utils:curve_entry() ],


	% A list of definitions of zones, between two curves in a 2D plot:
	zone_entries :: [ plot_utils:zone_entry() ]

}).



% Fully defines a label on a plot:
-record( plot_label, {

	% 2D coordinates of the label on the plot:
	location :: plot_utils:label_location(),

	% Actual text of the label:
	text :: plot_utils:label_text(),

	% Color of the text:
	color :: plot_utils:label_color(),

	% Position of the text based on to the location for the label:
	position :: plot_utils:label_position(),

	% The label may be rendered with an angle from the abscissa axis:
	orientation :: plot_utils:label_orientation() } ).
