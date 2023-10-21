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


% @doc Unit tests for the <b>plot-related basic toolbox</b> facilities.
%
% See the plot_utils tested module.
%
-module(plot_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Silencing:
-export([ test_basic_plot/0 ]).



test_basic_plot() ->

	InitPlotSettings = plot_utils:get_plot_settings( "My test plot" ),

	TitlePlotSettings = plot_utils:set_title( "Evaluation of the time factor "
		"due to a larger mass, based on its distance to the observer.",
		InitPlotSettings ),

	XLabelPlotSettings = plot_utils:set_x_label( "Distance in kilometers",
												 TitlePlotSettings ),

	YLabelPlotSettings = plot_utils:set_y_label( "Time factor (percentage)",
												 XLabelPlotSettings ),

	CurvelPlotSettings = plot_utils:declare_curves( [ "Time factor" ],
													YLabelPlotSettings ),

	FinalPlotSettings = CurvelPlotSettings,

	%M = physics_utils:m_sagittarius_a_star(),

	% 10 solar masses:
	M = 10 * physics_utils:m_sun(),

	% Closure:
	TimeFactorFun = fun( R ) ->
						physics_utils:get_time_factor( R, M )
					end,

	% Minimum distance:
	Start = physics_utils:get_schwarzschild_radius( M ),

	Stop = 1000 * Start,

	SampleCount = 1000,

	test_facilities:display(
		"Sampling ~B points from a distance of ~ts (~f m) to ~ts (~f m).",
		[ SampleCount, unit_utils:meters_to_string( Start ), Start,
		  unit_utils:meters_to_string( Stop ), Stop ] ),

	Pairs = math_utils:sample_as_pairs_for( TimeFactorFun, Start, Stop,
											SampleCount ),

	% [{meters(),percent()]:
	%trace_utils:debug_fmt( "Sample pairs: ~w.", [ Pairs ] ),

	{ success, BinPlotPath } =
		plot_utils:plot_samples( Pairs, FinalPlotSettings ),

	test_facilities:display( "Plot file '~ts' generated.", [ BinPlotPath ] ),

	ok.



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_basic_plot(),

	test_facilities:stop().
