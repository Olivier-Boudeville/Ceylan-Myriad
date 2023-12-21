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
% Creation date: Monday, December 18, 2023.


% @doc Unit tests for the management of <b>texts and fonts</b>.
-module(gui_text_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Shorthands:

-type frame() :: gui_frame:frame().


-type my_test_state() :: frame().
% Here the main loop just has to remember the frame whose closing is awaited
% for.


register_display( Text, Family, Style, Weight, Sizer, Panel ) ->

	FullText = text_utils:format( Text, [ Family, Style, Weight ] ),

	Font = gui_font:create( _FontSize=10, Family, Style, Weight ),

	Display = gui_text:create_static_display( FullText, _Parent=Panel ),

	% Allows to check text extent:
	gui_widget:set_background_color( Display, yellow ),

	gui_sizer:add_element( Sizer, Display, [ { proportion, 0 } ] ),

	gui_widget:set_font( Display, Font ),

	gui_font:destruct( Font ).


% @doc Executes the actual test.
-spec run_gui_test() -> void().
run_gui_test() ->

	test_facilities:display( "~nStarting the text test." ),

	gui:start(),

	Frame = gui_frame:create( "This is the overall frame for text testing",
							  _Size={ 1280, 1024 } ),

	Panel = gui_panel:create( Frame ),

	VertSizer = gui_sizer:create( _Orientation=vertical ),

	Text = "This is an example of text for the '~ts' family "
		   "of style '~ts' and weight '~ts'. Enjoy this font!",

	Families = gui_font:list_families(),

	Styles =  gui_font:list_styles(),

	% Not too many:
	%Weights = gui_font:list_weights(),
	%Weights = [ thin, normal, medium, bold, extra_heavy ],
	Weights = [ thin, normal, bold ],

	test_facilities:display( "For fonts, sampling following:"
		"~n - ~B families: ~p~n - ~B styles: ~p~n - ~B weights: ~p",
		[ length( Families ), Families, length( Styles ), Styles,
		  length( Weights ), Weights ] ),


	[ register_display( Text, F, S, W, VertSizer, Panel )
		|| F <- Families, S <- Styles, W <- Weights ],

	gui_widget:set_sizer( Panel, VertSizer ),

	gui:subscribe_to_events( [ { onWindowClosed, Frame } ] ),

	gui_frame:show( Frame ),

	test_main_loop( _InitialState=Frame ).




% @doc A very simple main loop, whose actual state is simply the GUI object
% corresponding to the frame that shall be closed to stop the test
% (i.e. CloseFrame).
%
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( State=Frame ) ->

	trace_utils:info( "Test main loop running..." ),

	receive

		{ onWindowClosed, [ Frame, _FrameId, _EventContext ] } ->
			trace_utils:info( "Main frame has been closed." ),
			stop( Frame );

		Other ->
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message: ~p.", [ Other ] ),
			test_main_loop( State )

	end.


stop( Frame ) ->
	trace_utils:info( "Test success, stopping." ),
	gui_frame:destruct( Frame ),
	gui:stop().



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the MyriadGUI test, being in batch mode)" );

		false ->
			run_gui_test()

	end,

	test_facilities:stop().
