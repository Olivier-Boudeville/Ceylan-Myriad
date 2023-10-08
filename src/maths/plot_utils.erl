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


% @doc Gathering of facilities <b>to plot graphs</b>.
-module(plot_utils).


% For the plot_settings record:
-include("plot_utils.hrl").


-type plot_settings() :: #plot_settings{}.
% The settings governing a given plot.


-type user_plot_name() :: any_string().
% The user-specified plot name, whence for example the corresponding file names
% will be derived.



-type user_plot_path() :: any_file_path().
% The path to a file corresponding to a rendering of a plot, as an image.
%
% Typically a PNG file.


-type plot_style() :: 'linespoints' % (default)
					| 'lines'
					| 'points'
					| 'boxes'
					| 'histograms'
					| 'filledcurves'
					| 'fillsteps'
					| atom(). % As many others exist.
% Plot style (default being 'linespoints'):
%
% (see http://gnuplot.info/docs_5.5/Plotting_Styles.html)


-type command_file_path() :: file_path().
% A path to a gnuplot command file (see the command_extension define).

-type bin_command_file_path() :: bin_file_path().
% A path to a gnuplot command file (see the command_extension define).


-type data_file_path() :: file_path().
% A path to a gnuplot data file (see the data_extension define).

-type bin_data_file_path() :: bin_file_path().
% A path to a gnuplot data file (see the data_extension define).


-type bin_plot_path() :: bin_file_path().
% A path to a plot (image) file (see the plot_extension define).

-type bin_plot_filename() :: bin_file_name().
% A filename of a plot (image) file (see the plot_extension define).



-export_type([ plot_settings/0,
			   user_plot_name/0, user_plot_path/0,
			   plot_style/0,
			   command_file_path/0, bin_command_file_path/0,
			   data_file_path/0, bin_data_file_path/0,
			   bin_plot_path/0, bin_plot_filename/0 ]).



-type declared_curve_name() :: ustring().
% The name of a user-specified curve.

-type special_curve_id() :: 'abscissa_top' | 'abscissa_bottom'.
% Identifiers of pseudo-curves (Ordinate = constant), corresponding respectively
% to the highest ordinate value and lowest one.


-type declared_curve_id() :: declared_curve_name() | special_curve_id().
% The identifier of a user-specified extended curve.


-type declared_zone_name() :: ustring().
% The name of a user-specified zone.

-type declared_zone() :: { declared_zone_name(),
		{ declared_curve_id(), declared_curve_id() } }.
% The definition of a user-specified zone.


-type curve_count() :: count().
% A number of curves.


-type point_size_factor() :: pos_integer().
% A factor by which the default point size is multiplied.


-export_type([ declared_curve_name/0, special_curve_id/0, declared_curve_id/0,
			   declared_zone_name/0, declared_zone/0,
			   curve_count/0,
			   point_size_factor/0 ]).



% (we use plain strings here, as opposed to the internal representation)



-type plot_label() :: #plot_label{}.
% Fully defines a label on a plot.

-type label_location() :: gui:point().
% Corresponds to the 2D integer coordinates of the label on the plot.


-type label_text() :: bin_string().
% Actual text of the label.
%
% Note that texts can be "enhanced" (e.g. "for some {/:Bold important} text").


-type label_color() :: color().
% Color of the text (default: "blue").


-type label_position() :: 'left' | 'center' | 'right'.
% Describes the position of the text based on to the specified location for the
% label.


-type label_orientation() :: 'upright' | int_degrees().
% Describes whether the text of the label should be rendered with an angle, from
% the abscissa axis.


-export_type([ plot_label/0,
			   label_location/0, label_text/0, label_color/0, label_position/0,
			   label_orientation/0 ]).



-type gnuplot_version() :: basic_utils:two_digit_version().
% A version of gnuplot.


-export_type([ gnuplot_version/0 ]).





-type rgb_color_spec() :: ustring().
% For example "3ab001" for lightgreen.

-type extra_curve_settings() :: rgb_color_spec().
% The color of a given curve.

-type extra_zone_settings() :: rgb_color_spec().
% The color of a given zone.



-type tick_option() :: maybe( 'rotate' ).
% Option applying to label ticks.


-type ticks_option() :: maybe( ustring() ).
% Option applying to ticks (e.g. axis, border, start, font, textcolor, etc.) for
% a fine control of the major (labelled) tics on an axis.
%
% (e.g. see Xtics, in http://www.gnuplot.info/docs_4.2/node295.html)


-type timestamp_time_format() ::

	% Time then date, on a single line:
	'single_line'

	% Time, newline, date, hence on two lines:
  | 'double_line'.
% The display time format to use for timestamped axes.


-export_type([ rgb_color_spec/0, extra_curve_settings/0, extra_zone_settings/0,
			   tick_option/0, ticks_option/0, timestamp_time_format/0 ]).



-type command_element() :: ustring().
% An element of a gnuplot command.


-type timestamp_string() :: ustring().
% String describing a timestamp (typically either a tick or a textual
% timestamp).


-type timestamp_bin_string() :: bin_string().


-export_type([ command_element/0, timestamp_string/0, timestamp_bin_string/0 ]).





-export([ get_plot_settings/1, set_title/2, plot_samples/2,
		  generate_command_file/1 ]).


% Exported gnuplot helpers:
-export([ get_default_curve_plot_suffix/0, get_default_zone_plot_suffix/0,
		  get_formatted_orientation/1,
		  get_label_definitions/1, get_label_definitions/2,
		  get_xticks_option/1, get_yticks_option/1,
		  get_x_range_option/1, get_y_range_option/1,
		  get_x_ticks_option/1, get_y_ticks_option/1 ]).



% Section for internal types:

-type plot_name() :: bin_string().
% The internal plot name.


-type plot_path() :: bin_file_path().
% The (internal) path to a file corresponding to a rendering of a plot, as an
% image.
%
% Typically a PNG file.



-type curve_name() :: bin_string().
% The internal name for a curve.


-type curve_index() :: curve_count().
% Curves are numbered internally, and correspond to the position of data in
% specified samples.

-type extended_curve_id() :: curve_index() | special_curve_id().
% Extended to allow for zone definitions.

-type curve_entry() :: { curve_index(), curve_name(), curve_plot_suffix() }.
% Information specific to the rendering of a curve.


-type curve_offset() :: count().
% Applies notably if using (unquoted) timestamps that account for more than one
% field in data. Prefer relying on quoted timestamps.



-type curve_plot_suffix() :: bin_string().
% A (binary string) suffix (e.g. <<"noenhanced with filledcurves">>, <<"with
% boxes">> or <<"with boxes lt rgb '#f0f0f0'">>) to be added to the plot command
% of the corresponding curve.


-type zone_name() :: bin_string().
% Tne name of a zone.


-type zone_plot_suffix() :: bin_string().
% A (binary string) suffix (e.g. <<"fillcolor red">>) to be added to the plot
% command of the corresponding zone.


-type zone_entry() ::
		{ zone_name(), { extended_curve_id(), extended_curve_id() },
		  zone_plot_suffix() }.
% Information specific to the rendering of a zone.


% For internal sharing:
-export_type([ plot_name/0, plot_path/0,
			   curve_name/0, curve_index/0, extended_curve_id/0,
			   curve_entry/0, curve_offset/0, curve_plot_suffix/0,
			   zone_name/0, zone_plot_suffix/0, zone_entry/0 ]).





% Implementation notes:
%
% Although there are open-source, cross-platform alternatives to gnuplot
% (e.g. LabPlot, SciDaVis, KAlgebra, KSEG and clip), none seemed to reach its
% richness/level of maturity. So we stick with good old gnuplot, hopefully for
% the best.


% Define section.


% To centralize basic open flags:
%
% (exclusive used to avoid that two plots bearing the same name by mistake may
% end up writing to the same files simultaneously)
%
-define( base_open_flags, [ write, exclusive ] ).



% To have rotated tick labels:

% Rotate clockwise (hence end of text below beginning):
-define( rotate_cw_tick_label_option, "rotate by - 45" ).

% Rotate counter-clockwise (hence end of text above beginning):
% (note the right alignment here, otherwise the labels run from the base of the
% graph upwards)
%
-define( rotate_ccw_tick_label_option, "rotate by 45 right" ).



% Not all plot features are available in older gnuplot versions:
-define( gnuplot_reference_version, { 5, 4 } ).



% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().
-type bin_title() :: text_utils:bin_title().
-type any_title() :: text_utils:any_title().

-type file_path() :: file_utils:file_path().
-type any_file_path() :: file_utils:any_file_path().
-type bin_file_path() :: file_utils:bin_file_path().
-type bin_file_name() :: file_utils:bin_file_name().

-type int_degrees() :: unit_utils:int_degrees().

-type sample_data() :: math_utils:sample_data().
%-type float_sample() :: math_utils:float_sample().

%-type point() :: gui:point().
-type color() :: gui_color:color().
%-type length() :: gui:length().
%-type coordinate() :: gui:coordinate().



% @doc Returns the specification of a plot of the specified name.
-spec get_plot_settings( user_plot_name() ) -> plot_settings().
get_plot_settings( UsrPlotName ) ->
	#plot_settings{ name=text_utils:ensure_binary( UsrPlotName ) }.


% @doc Returns the specification of a plot corresponding to the specified one
% once the specified title (if any) has been set.
%
-spec set_title( maybe( any_title() ), plot_settings() ) -> plot_settings().
set_title( MaybeTitle, PlotSettings ) ->
	PlotSettings#plot_settings{
		title=text_utils:ensure_maybe_binary( MaybeTitle ) }.




% doc Plots the specified samples, based on the specified plot specification,
% and returns the path to the corresponding generated plot file.
%
-spec plot_samples( [ sample_data() ], plot_settings() ) -> bin_plot_path().
plot_samples( _Samples, _PlotSettings=#plot_settings{
		title=MaybeTitle,
		plot_directory=MaybePlotDir,
		plot_filename=MaybePlotFilename } ) ->

	BinPlotDir = case MaybePlotDir of

		undefined ->
			file_utils:get_current_directory();

		PlotDr ->
			PlotDr

	end,

	BinPlotFilename = case MaybePlotFilename of

		undefined ->
			forge_plot_filename( MaybeTitle );

		PlotFln ->
			PlotFln

	end,

	BinPlotPath = file_utils:bin_join( BinPlotDir, BinPlotFilename ),

	trace_utils:debug_fmt( "Plot to be generated in '~ts'.", [ BinPlotPath ] ),

	BinPlotPath.



% (helper)
-spec forge_plot_filename( maybe( bin_title() ) ) -> bin_plot_filename().
forge_plot_filename( undefined ) ->
	% Default:
	forge_plot_filename( <<"myriad-plot">> );

forge_plot_filename( BinTitle ) ->
	file_utils:convert_to_filename_with_extension( BinTitle, ?plot_extension ).





% Section for gnuplot helpers.


% @doc Returns the default per-curve plot suffix, as a binary.
-spec get_default_curve_plot_suffix() -> curve_plot_suffix().
get_default_curve_plot_suffix() ->
	% "noenhanced" to avoid that a name like 'foo_bar' gets displayed as foo
	% with bar put as subscript.
	%
	<<"noenhanced">>.



% @doc Returns the default per-zone plot suffix, as a binary.
-spec get_default_zone_plot_suffix() -> curve_plot_suffix().
get_default_zone_plot_suffix() ->
	<<"">>.



% @doc Returns a gnuplot-compatible rotation specification.
%
% (helper)
%
-spec get_formatted_orientation( label_orientation() ) -> command_element().
get_formatted_orientation( upright ) ->
	"norotate";

get_formatted_orientation( Angle ) when is_number( Angle ) ->
	text_utils:format( "rotate by ~p", [ Angle ] ).



% @doc Returns the gnuplot command appropriate to render all registered labels.
-spec get_label_definitions( [ plot_label() ] ) -> command_element().
get_label_definitions( Labels ) ->
	text_utils:join( _Separator="\n",
					 get_label_definitions( Labels, _Acc=[] ) ).


% (helper)
get_label_definitions( _Labels=[], Acc ) ->
	% Nothing to reverse, labels will end up being rendered in the order they
	% were specified:
	Acc;

get_label_definitions( [ #plot_label{ location={ X, Y }, text=BinText,
		color=Color, position=Position, orientation=Orientation } | T ],
					   Acc ) ->

	% For a list of supported colors, see:
	% www.uni-hamburg.de/Wiss/FB/15/Sustainability/schneider/gnuplot/colors.htm
	%
	ActualColor = gui_color:get_color_for_gnuplot( Color ),

	LabelStr = text_utils:format(
		"set label \"~ts\" at ~p,~p ~ts ~ts textcolor rgbcolor \"~ts\"",
		[ text_utils:binary_to_string( BinText ), X, Y, Position,
		  get_formatted_orientation( Orientation ), ActualColor] ),

	get_label_definitions( T, [ LabelStr | Acc ] ).



% @doc Returns the settings for fine control of the major (labeled) ticks on the
% abscissa axis, as read from the specified settings.
%
-spec get_xticks_option( plot_settings() ) -> command_element().
get_xticks_option( #plot_settings{ x_ticks=undefined } ) ->
	"# No x_ticks set.";

get_xticks_option( #plot_settings{ x_ticks=XticksBinSpec } )
											when is_binary( XticksBinSpec ) ->
	text_utils:format( "set xtics ~ts~n", [ XticksBinSpec ] );

get_xticks_option( #plot_settings{ x_ticks=Other } ) ->
	throw( { invalid_x_ticks_option, Other } ).



% @doc Returns the settings for fine control of the major (labeled) ticks on the
% ordinate axis, as read from the specified settings.
%
-spec get_yticks_option( plot_settings() ) -> command_element().
get_yticks_option( #plot_settings{ y_ticks=undefined } ) ->
	"# No y_ticks set.";

get_yticks_option( #plot_settings{ y_ticks=YticksBinSpec } )
											when is_binary( YticksBinSpec ) ->
	text_utils:format( "set ytics ~ts~n", [ YticksBinSpec ] );

get_yticks_option( #plot_settings{ y_ticks=Other } ) ->
	throw( { invalid_y_ticks_option, Other } ).



% @doc Returns the abscissa range options, as read from the specified plot
% settings.
%
-spec get_x_range_option( plot_settings() ) -> command_element().
get_x_range_option( #plot_settings{ x_range=undefined} ) ->
	"# No xrange set.";

get_x_range_option( #plot_settings{
			x_range={ _MaybeMinX=undefined, _MaybeMaxX=undefined }  } ) ->
	"# No xrange set.";

get_x_range_option( #plot_settings{
			x_range={ _MaybeMinX=undefined, MaxX } } ) when is_number( MaxX )->
	text_utils:format( "set xrange [:~w]", [ MaxX ] );

get_x_range_option( #plot_settings{
			x_range={ MinX, _MaybeMaxX=undefined } } ) when is_number( MinX )->
	text_utils:format( "set xrange [~w:]", [ MinX ] );

get_x_range_option( #plot_settings{ x_range={ MinX, MaxX } } )
						when is_number( MinX ) andalso is_number( MaxX ) ->
	text_utils:format( "set xrange [~w:~w]", [ MinX, MaxX ] ).



% @doc Returns the ordinate range options, as read from the specified plot
% settings.
%
-spec get_y_range_option( plot_settings() ) -> command_element().
get_y_range_option( #plot_settings{ y_range=undefined} ) ->
	"# No yrange set.";

get_y_range_option( #plot_settings{
			y_range={ _MaybeMinY=undefined, _MaybeMaxY=undefined }  } ) ->
	"# No yrange set.";

get_y_range_option( #plot_settings{
			y_range={ _MaybeMinY=undefined, MaxY } } ) when is_number( MaxY )->
	text_utils:format( "set yrange [:~w]", [ MaxY ] );

get_y_range_option( #plot_settings{
			y_range={ MinY, _MaybeMaxY=undefined } } ) when is_number( MinY )->
	text_utils:format( "set yrange [~w:]", [ MinY ] );

get_y_range_option( #plot_settings{ y_range={ MinY, MaxY } } )
						when is_number( MinY ) andalso is_number( MaxY ) ->
	text_utils:format( "set yrange [~w:~w]", [ MinY, MaxY ] ).



% @doc Returns the plot directory that should be used.
get_plot_directory( #plot_settings{ plot_directory=undefined } ) ->
	file_utils:get_bin_current_directory();

get_plot_directory( #plot_settings{ plot_directory=BinPlotDir } ) ->
	file_utils:is_existing_directory( BinPlotDir ) orelse
		throw( { non_existing_plot_directory, BinPlotDir } ),
	BinPlotDir.



% @doc Returns the option to control the labels of the major ticks on the x
% axis, as read from the specified settings.
%
-spec get_x_ticks_option( plot_settings() ) -> command_element().
get_x_ticks_option( #plot_settings{ x_ticks=undefined } ) ->
	"# No label set for the major ticks on the x axis.";

get_x_ticks_option( #plot_settings{ x_ticks=XTicksInfo } ) ->
	text_utils:format( "set xtics ~ts", [ XTicksInfo ] ).



% @doc Returns the option to control the labels of the major ticks on the y
% axis, as read from the specified settings.
%
-spec get_y_ticks_option( plot_settings() ) -> command_element().
get_y_ticks_option( #plot_settings{ x_ticks=undefined } ) ->
	"# No label set for the major ticks on the y axis.";

get_y_ticks_option( #plot_settings{ x_ticks=YTicksInfo } ) ->
	text_utils:format( "set ytics ~ts", [ YTicksInfo ] ).



% @doc Returns the gnuplot command filename corresponding to the specified plot
% name.
%
-spec get_command_filename( plot_name() ) -> bin_file_name().
get_command_filename( Name ) ->
	file_utils:convert_to_filename_with_extension( Name, ?command_extension ).


% @doc Returns the gnuplot data filename corresponding to the specified plot
% name.
%
-spec get_data_filename( plot_name() ) -> bin_file_name().
get_data_filename( Name ) ->
	file_utils:convert_to_filename_with_extension( Name, ?data_extension ).


% @doc Returns the report filename corresponding to the specified plot name.
-spec get_report_filename( maybe( bin_file_name() ), plot_name() ) ->
											bin_file_name().
get_report_filename( _MaybePlotBinFilename=undefined, BinPlotName ) ->
	file_utils:convert_to_filename_with_extension( BinPlotName,
												   ?plot_extension );

get_report_filename( PlotBinFilename, _BinPlotName ) ->
	PlotBinFilename.


% @doc Returns the appropriate settings, depending on whether the abscissa axis
% gathers timestamps or not.
%
% Note: now, thanks to quoting, curve offset is always zero as a timestamp
% occupies only one column.
%
-spec get_timestamp_settings( plot_settings() ) ->
									{ command_element(), curve_offset() }.
get_timestamp_settings( #plot_settings{
		is_timestamped=true,
		x_ticks_timestamp_time_format=MaybeTimeFmt } ) ->

	% Also a check:
	UserTimeFormat = case MaybeTimeFmt of

		undefined ->
			_DefaultTimeFormat=double_line;

		single_line ->
			single_line;

		double_line ->
			double_line;

		OtherTimeFormat ->
			throw( { invalid_timestamp_time_format, OtherTimeFormat } )

	end,

	TimeFormatStr = case UserTimeFormat of

		single_line ->
			"\"%d %b %Y %H:%M:%S\"";

		double_line ->
			"\"%d %b %Y\\n\\n%H:%M:%S\""

	end,

	PreambleStr = text_utils:format(
		"set xdata time~n"

		% As read from the data (our standard format):
		% (we have to add single quotes, so that gnuplot sees:
		%   set timefmt '"%Y/%m/%d %H:%M:%S"'
		% so that we can specify our timestamps as:
		%   "2000/1/1 00:00:00"
		% Indeed, should they be written as:
		%   2000/1/1 00:00:00
		% gnuplot would see two columns
		% (another option is to set another separator than space)
		%
		"set timefmt '\"%Y/%m/%d %H:%M:%S\"'~n"

		 % As shall be rendered across axes:
		"set format x ~ts~n", [ TimeFormatStr ] ),

	% No more curve offset, as now in all cases, thanks to quoting, time
	% translates to exactly to the first column (not the first two columns, as
	% when an unquoted timestamp would be read as a time and a date, not as a
	% single value):
	%
	%{ PreambleStr, _CurveOffset=1 };
	{ PreambleStr, _CurveOffset=0 };


get_timestamp_settings( #plot_settings{ is_timestamped=false } ) ->
	{ _PreambleStr="", _CurveOffset=0 }.



% @doc Returns the gnuplot command appropriate to render that plot.
%
% Defines one plot curve per declared curve, with current plot settings, and
% defines as well any specified zone.
%
-spec get_plot_command( plot_settings(), [ curve_entry() ], curve_offset(),
			[ zone_entry() ], bin_file_name() ) -> command_element().
get_plot_command( _Settings, _CurveEntries=[], _CurveOffset, _ZoneEntries=[],
				  _DataFilename ) ->
	throw( no_curve_or_zone_defined );

get_plot_command( Settings, CurveEntries, CurveOffset, ZoneEntries,
				  DataFilename ) ->

	% Typical expected output:
	%
	% plot 'silver.dat' using 1:2 with lines, 'silver.dat' using 1:3 with lines,
	% 'silver.dat' using 1:2:3 with filledcurves

	% For us it is: "plot " ++ tl(join(_Prefix=", 'silver.dat' using 1:",
	%  ["2 with lines", "3 with lines", "2:3 with filledcurves"]))
	%
	% (knowing that tl is used to remove the prefix head (','), i.e. the first
	% extra comma)
	%
	% So:

	Prefix = text_utils:format( ", \"~ts\" using 1:", [ DataFilename ] ),

	% We prefer not rendering curves that are used to delimit zones:
	FilteredCurveEntries =
		remove_zone_specific_curves( CurveEntries, ZoneEntries ),

	CurvePlots = get_plot_commands_for_curves( FilteredCurveEntries,
											   CurveOffset, Settings ),

	ZonePlots = get_plot_commands_for_zones( ZoneEntries, CurveOffset,
											 Settings ),

	% Note that we specify the zones first, otherwise the curves would be hidden
	% below:
	%
	JoinedCommand = text_utils:join( Prefix, ZonePlots ++ CurvePlots ),

	% Two tl to remove prefix head (i.e. ", "):
	text_utils:format( "plot ~ts~ts~n", [ tl( tl( Prefix ) ), JoinedCommand ] ).



% @doc Returns a list of curve entries in which there are no more curves that
% are used to define a zone among the ones specified.
%
-spec remove_zone_specific_curves( [ curve_entry() ], [ zone_entry() ] ) ->
											[ curve_entry() ].
remove_zone_specific_curves( CurveEntries, ZoneEntries ) ->

	CurvesToRemove = select_curves( ZoneEntries, _Acc=[] ),

	Selector = fun( { CurveIndex, _CurveName, _CurvePlotSuffix } ) ->
				not lists:member( CurveIndex, CurvesToRemove )
			   end,

	lists:filter( Selector, CurveEntries ).



% @doc Returns the plot commands corresponding to the specified curves.
-spec get_plot_commands_for_curves( [ curve_entry() ], curve_offset(),
									plot_settings() ) -> [ command_element() ].
get_plot_commands_for_curves( CurveEntries, CurveOffset, Settings ) ->

	% Curve entries are a list of:
	%  {CurveIndex, BinCurveName, BinPlotSuffix}
	%
	% After some potential reordering, curve entries might be:
	% [{3, <<"c">>, _}, {1,<<"a">>, _}, {2,<<"b">>, _}]
	%
	% We expect: ["4 title \"c\"", "2 title \"a\"", "3 title \"b\""]
	%
	% (note that each curve index is incremented, as the first column is the
	% tick)
	%
	% We simply write them in-order, with an appropriate title:
	[ get_curve_command( C, CurveOffset, Settings ) || C <- CurveEntries ].



% @doc Returns a command element suitable to render the specified curve.
%
% (helper)
%
-spec get_curve_command( curve_entry(), curve_offset(), plot_settings() ) ->
								command_element().
get_curve_command( { CurveIndex, BinCurveName, BinPlotSuffix }, CurveOffset,
				   _Settings ) ->

	Title = text_utils:binary_to_string( BinCurveName ),

	% +1 to account for the abscissa (time) first field.
	text_utils:format( "~B title \"~ts\" ~ts",
		[ CurveIndex + CurveOffset + 1, Title, BinPlotSuffix ] ).



% @doc Returns command elements suitable to render the specified zones.
%
% (helper)
%
-spec get_plot_commands_for_zones( [ zone_entry() ], curve_offset(),
								   plot_settings() ) -> [ command_element() ].
get_plot_commands_for_zones( ZoneEntries, CurveOffset,
		Settings=#plot_settings{ plot_style=BinPlotStyle } ) ->

	% Zone entries are a list of:
	% {BinZoneName, {ExtendedCurveName1, ExtendedCurveName2}}
	%
	% We expect returned values to be either "3:5 with filledcurves" (for a zone
	% between curves 2 and 4) or "3 with filledcurves x1" (for a zone between
	% curve 2 and the abscissa axis).
	%
	% Apparently, in terms of order, for a proper rendering, filledcurves shall
	% be rendered from top to bottom, whereas at least for fillsteps the
	% opposite order shall be used (bottom to top); so:
	%
	OrderedZoneEntries = case BinPlotStyle of

		<<"filledcurves">> ->
			ZoneEntries;

		% For example for fillsteps (side-effects: reverses key order):
		_ ->
			lists:reverse( ZoneEntries )

	end,

	[ get_zone_command( Z, CurveOffset, Settings ) || Z <- OrderedZoneEntries ].



% @doc Returns a command element suitable to render the specified zone.
%
% (helper)
%
-spec get_zone_command( zone_entry(), curve_offset(), plot_settings() ) ->
								command_element().
get_zone_command(
		_ZoneEntry={ BinZoneName, { FirstExtendedCurve, SecondExtendedCurve },
					 ZonePlotSuffix },
		CurveOffset,
		_Settings=#plot_settings{ plot_style=BinPlotStyle } ) ->

	%trace_utils:debug_fmt( "Zone command for entry ~p.", [ ZoneEntry ] ),

	FirstPart = case FirstExtendedCurve of

		'abscissa_top' ->
			% The other curve is necessarily an index (+1, as the first column
			% is the tick/timestamp):
			%
			ActualCurveIndex = SecondExtendedCurve + CurveOffset + 1,
			case BinPlotStyle of

				<<"filledcurves">> ->
					text_utils:format( "~B with ~ts ~ts below x2",
						[ ActualCurveIndex, BinPlotStyle, ZonePlotSuffix ] );


				_ ->
					text_utils:format( "~B with ~ts ~ts",
						[ ActualCurveIndex, BinPlotStyle, ZonePlotSuffix ] )

			end;


		'abscissa_bottom' ->

			% The other curve is necessarily an index (+1, as the first column
			% is the tick/timestamp):
			%
			ActualCurveIndex = SecondExtendedCurve + CurveOffset + 1,

			case BinPlotStyle of

				<<"filledcurves">> ->
					text_utils:format( "~B with ~ts ~ts above x1",
						[ ActualCurveIndex, BinPlotStyle, ZonePlotSuffix ] );

				_ ->
					text_utils:format( "~B with ~ts ~ts",
						[ ActualCurveIndex, BinPlotStyle, ZonePlotSuffix ] )

			end;

		_BinCurveName ->
			ActualFirstCurveIndex = FirstExtendedCurve + 1,
			ActualSecondCurveIndex = SecondExtendedCurve + 1,
			case BinPlotStyle of

				<<"filledcurves">> ->
					text_utils:format( "~B:~B with ~ts ~ts",
						[ ActualFirstCurveIndex, ActualSecondCurveIndex,
						  BinPlotStyle, ZonePlotSuffix ] );

				_ ->
					text_utils:format( "~B with ~ts ~ts",
						[ ActualSecondCurveIndex, BinPlotStyle,
						  ZonePlotSuffix ] )

			end

	end,

	FirstPart ++ text_utils:format( " title \"~ts\"",
		[ text_utils:binary_to_string( BinZoneName ) ] ).




% @doc Selects all curve indexes that are mentioned in zones (possibly with
% duplicates).
%
select_curves( _ZoneEntries=[], Acc ) ->
	Acc;

select_curves(
		_ZoneEntries=[ { _ZoneName, { abscissa_top, C }, _ZPlotSuffix } | T ],
		Acc ) ->
	select_curves( T, [ C | Acc ] );

select_curves( _ZoneEntries=[
				{ _ZoneName, { abscissa_bottom, C }, _ZPlotSuffix } | T ],
			   Acc ) ->
	select_curves( T, [ C | Acc ] );

select_curves( _ZoneEntries=[ { _ZoneName, { C1, C2 }, _ZPlotSuffix } | T ],
			   Acc ) ->
	select_curves( T, [ C1, C2 | Acc ] ).



% @doc Generates unconditionally the appropriate gnuplot command file for the
% specified plot settings.
%
% Returns the name, as a plain string, of the command file.

% (helper)
%
-spec generate_command_file( plot_settings() ) -> command_file_path().
generate_command_file( PlotSettings=#plot_settings{
		name=BinPlotName,
		labels=Labels,
		extra_defines=ExtraDefines,
		plot_filename=MaybePlotBinFilename,
		curve_entries=CurveEntries,
		zone_entries=ZoneEntries } ) ->

	cond_utils:if_defined( myriad_debug_plot,
		trace_utils:debug_fmt( "Generating a command file for plot '~ts'; "
			"curve entries:~n  ~p; zone entries:~n  ~p.",
			[ BinPlotName, CurveEntries, ZoneEntries ] ) ),

	LabelDefs = get_label_definitions( Labels ),

	ExtraDefs = text_utils:join( _Sep="\n",
		[ text_utils:binary_to_string( D ) || D <- ExtraDefines ] ),

	DataFilename = get_data_filename( BinPlotName ),

	XrangeOpt = get_x_range_option( PlotSettings ),

	YrangeOpt = get_y_range_option( PlotSettings ),

	% Corresponding to the '*ticks' counterparts, not the base '*tick' ones:
	XticksOpt = get_xticks_option( PlotSettings ),
	YticksOpt = get_yticks_option( PlotSettings ),

	BinPlotDir = get_plot_directory( PlotSettings ),

	PNGFilename = get_report_filename( MaybePlotBinFilename, BinPlotName ),

	% We prefer defining relative paths, so that a command file and its
	% dependencies are movable afterwards:
	%
	%PNGFilePath = file_utils:bin_join( BinPlotDir, PNGFilename ),

	CommandFilename = get_command_filename( BinPlotName ),

	% This one is an entry point, and shall preferably be absolute:
	CommandFilePath = file_utils:bin_join( BinPlotDir, CommandFilename ),

	% Possible race condition if two processes try to create it at once:
	file_utils:remove_file_if_existing( CommandFilePath ),

	% No 'delayed_write' I/O option useful here:
	File = file_utils:open( CommandFilePath, ?base_open_flags ),

	cond_utils:if_defined( myriad_debug_plot,
		trace_utils:debug_fmt( "Generating command file '~ts'.",
							   [ CommandFilePath ] ) ),

	% Changes shall be applied if using timestamps rather than raw ticks:
	{ PreambleStr, CurveOffset } = get_timestamp_settings( PlotSettings ),

	PlotCommand = get_plot_command( PlotSettings, CurveEntries, CurveOffset,
									ZoneEntries, DataFilename ),

	% Use 'undefined' in sample if not having available data for an element.
	% Set terminal png *transparent* could be used as well.
	%
	file_utils:write_ustring( File,
		"~ts~n"
		"set autoscale~n"
		"unset log~n"
		"set grid~n"
		"set style data ~ts~n"
		"set style fill ~ts~n"
		"set key box ~ts~n"
		"set pointsize ~B~n"
		"set xtic ~ts~n"
		"set ytic ~ts~n"

		% Note that {x,y}tics options shall be specified *after* {x,y}tic ones
		% in order to be taken into account:
		%
		"~ts~n"
		"~ts~n"

		"~ts~n"
		"~ts~n"

		"set title \"~ts\" noenhanced~n"

		% Newline added, otherwise the label of the X axis may collide with the
		% upper part of the box of the key:
		%
		"set xlabel \"~ts\\n\"~n"
		%"set xlabel \"~ts\" offset 0,2~n"

		"set ylabel \"~ts\"~n"
		"set datafile missing 'undefined'~n"
		"set terminal ~ts size ~B, ~B~n"
		"~ts~n"
		"~ts~n"
		"set output \"~ts\"~n"
		"~ts",
		[ PreambleStr,
		  PlotSettings#plot_settings.plot_style,
		  PlotSettings#plot_settings.fill_style,
		  PlotSettings#plot_settings.key_options,
		  PlotSettings#plot_settings.point_size,
		  PlotSettings#plot_settings.x_tick,
		  PlotSettings#plot_settings.y_tick,
		  XticksOpt,
		  YticksOpt,
		  XrangeOpt,
		  YrangeOpt,
		  PlotSettings#plot_settings.title,
		  PlotSettings#plot_settings.x_label,
		  PlotSettings#plot_settings.y_label,
		  PlotSettings#plot_settings.image_format,
		  PlotSettings#plot_settings.canvas_width,
		  PlotSettings#plot_settings.canvas_height,
		  LabelDefs,
		  ExtraDefs,
		  PNGFilename,
		  PlotCommand ] ),

	file_utils:close( File ),

	CommandFilename.
