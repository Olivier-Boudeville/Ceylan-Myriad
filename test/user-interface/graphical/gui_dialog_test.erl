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
% Creation date: Monday, August 21, 2023.


% @doc Unit tests for the management of <b>dialogs</b>, that are various
% standard modal windows.
%
-module(gui_dialog_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Shorthands:

-type frame() :: gui:frame().

-type my_test_state() :: frame().
% Here the main loop just has to remember the frame whose closing is awaited
% for.



% @doc Executes the actual test.
-spec run_gui_test() -> void().
run_gui_test() ->

	test_facilities:display( "~nStarting the dialog test." ),

	gui:start(),

	Frame = gui:create_frame( "This is the overall frame for dialog testing" ),

	gui:subscribe_to_events( { onWindowClosed, Frame } ),

	Panel = gui:create_panel( Frame ),

	Sizer = gui:create_sizer( vertical ),

	gui:set_sizer( Panel, Sizer ),

	% Common settings:

	Position = auto,
	ButtonSize = auto,
	ButtonStyle = default,

	Parent = Panel,

	MsgButton = gui:create_button( "Show message dialog", Position, ButtonSize,
		ButtonStyle, message_dialog_button_id, Parent ),

	SingChButton = gui:create_button( "Show single-choice dialog", Position,
		ButtonSize, ButtonStyle, single_choice_dialog_button_id, Parent ),

	MultiChButton = gui:create_button( "Show multi-choice dialog", Position,
		ButtonSize, ButtonStyle, multi_choice_dialog_button_id, Parent ),

	Buttons = [ MsgButton, SingChButton, MultiChButton ],

	gui:add_to_sizer( Sizer, Buttons ),
	gui:subscribe_to_events( [ { onButtonClicked, B } || B <- Buttons ] ),

	gui:show( Frame ),

	test_main_loop( _InitialState=Frame ).




% @doc A very simple main loop, whose actual state is simply the GUI object
% corresponding to the frame that shall be closed to stop the test
% (i.e. CloseFrame).
%
-spec test_main_loop( my_test_state() ) -> no_return().
test_main_loop( State=Frame ) ->

	trace_utils:info( "Test main loop running..." ),

	receive

		{ onButtonClicked,
				[ _Button, _ButtonId=message_dialog_button_id, _Context ] } ->

			MsgDialog = gui:create_message_dialog( "This is a message dialog",
				{ style, [ yes_no_buttons, information_icon, center ] },
				_Parent=Frame ),

			case gui:show_modal( MsgDialog ) of

				yes_returned ->
					trace_utils:debug( "Message dialog returned 'yes'." );

				no_returned ->
					trace_utils:debug( "Message dialog returned 'no'." );

				cancel_returned ->
					trace_utils:debug( "Message dialog cancelled." )

			end,

			test_main_loop( State );


		{ onButtonClicked, [ _Button, _ButtonId=single_choice_dialog_button_id,
							 _Context ] } ->

			ChoiceSpec = [ { choice_a, "Choice A" }, { choice_b, "Choice B" },
						   { choice_c, "Choice C" } ],

			SingChDialog = gui:create_single_choice_dialog(
				"This is a single-choice dialog.", "This is a caption",
				ChoiceSpec, { style, [ ok_button, cancel_button, center ] },
				_Parent=Frame ),

			gui:set_selected_choice( SingChDialog, choice_c, ChoiceSpec ),

			case gui:show_modal( SingChDialog ) of

				ok_returned ->
					Designator =
						gui:get_choice_designator( SingChDialog, ChoiceSpec ),

					trace_utils:debug_fmt( "Single-choice dialog returned "
						"'ok', selected designator being '~ts'.",
						[ Designator ] );

				cancel_returned ->
					trace_utils:debug( "Single-choice dialog cancelled." )

			end,

			test_main_loop( State );


		{ onButtonClicked, [ _Button, _ButtonId=multi_choice_dialog_button_id,
							 _Context ] } ->

			ChoiceSpec = [ { choice_a, "Choice A" }, { choice_b, "Choice B" },
						   { choice_c, "Choice C" }, { choice_d, "Choice D" },
						   { choice_e, "Choice E" } ],

			MultChDialog = gui:create_multi_choice_dialog(
				"This is a multi-choice dialog.", "This is a caption",
				ChoiceSpec, { style, [ ok_button, cancel_button, center ] },
				_Parent=Frame ),

			gui:set_selected_choices( MultChDialog, [ choice_b, choice_c ],
									  ChoiceSpec ),

			case gui:show_modal( MultChDialog ) of

				ok_returned ->
					Designators =
						gui:get_choice_designators( MultChDialog, ChoiceSpec ),

					trace_utils:debug_fmt( "Multi-choice dialog returned "
						"'ok', selected designators being ~w.",
						[ Designators ] );

				cancel_returned ->
					trace_utils:debug( "Multi-choice dialog cancelled." )

			end,

			test_main_loop( State );


		{ onWindowClosed, [ Frame, _FrameId, _Context ] } ->
			trace_utils:info( "Main frame has been closed; test success." ),
			gui:destruct_window( Frame ),
			gui:stop();

		Other ->
			trace_utils:warning_fmt( "Test main loop ignored following "
									 "message: ~p.", [ Other ] ),
			test_main_loop( State )

	end.



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
