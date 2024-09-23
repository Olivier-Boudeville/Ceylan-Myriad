% Copyright (C) 2010-2024 Olivier Boudeville
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
% Creation date: Monday, February 15, 2010.

-module(gui_text_editor).

-moduledoc """
Gathering of various facilities for **text editor**.

See also the corresponding gui_dialog:text_entry_dialog() dialog and the
gui_text_editor_test.erl test.
""".



-doc """
A widget to edit a text (single line or multi-line).

The current font applies to this display.

The horizontal scrollbar ('horiz_scrollbar' style flag) will only be created for
multi-line text editors. Without an horizontal scrollbar, text lines that do not
fit in the editor's size will be wrapped (but no newline character will be
inserted).

Single line controls do not have a horizontal scrollbar, the text is
automatically scrolled so that the insertion point is always visible.

One can subscribe to the following events that can be emitted by a text editor:
- onTextUpdated, if its text has been modified
- onEnterPressed, if the Enter key is pressed whereas the text editor enabled
  its process_enter_key option
- onTextOverflow, if the length limit of the text in the editor is reached
""".
-opaque text_editor() :: wxTextCtrl: wxTextCtrl().



-doc "An option for the creation of a text editor.".
-type text_editor_option() ::
	{ 'position', point() }
  | { 'size', size() }
  | { 'text', text() }
  | { 'validator', text_validator() }
  | { 'style', [ text_editor_style() ] }.



-doc """
A style element specific to a text editor.

See also <https://docs.wxwidgets.org/stable/classwx_text_ctrl.html>.
""".
-type text_editor_style() ::
	'process_enter_key' % Generate an event if Enter is pressed.
  | 'process_tab_key'   % Generate an event if Tab is pressed.
  | 'multiline' % Allow multiple lines of text.
  | 'password' % Text will be echoed as asterisks.
  | 'read_only' % Rext will not be user-editable.
  | 'rich_text_v1' % Use rich text control version 1 on Windows.
  | 'rich_text_v2' % Use rich text control version 2 on Windows.
  | 'auto_url' % Process URLs automatically.
  | 'always_show_selection' % Show the selection even when not having focus.
  | 'horiz_scrollbar' % Use an horizontal scrollbar instead of wrapping text.
  | 'no_vert_scrollbar' % Never create a vertical scrollbar.
  | 'left_justify' % Text will be left-justified (default).
  | 'center' % Text will be centered.
  | 'right_justify' % Text will be right-justified.
  | 'char_wrap' % Wrap lines at character boundaries.
  | 'word_wrap' % Wrap lines at word boundaries.
  | 'best_wrap' % Wrap lines at word boundaries, if possible.
  | 'auto_scroll'. % (undefined)



-doc "A process in charge of validating the text of a widget.".
-type text_validator() :: wx:wx_object().



-doc """
A zero-based index of a position in the text.
""".
-type char_pos() :: non_neg_integer().



-doc """
A type of event possibly emitted by a text editor, to which one can subscribe.
""".
-type text_editor_event_type() ::
	'onTextUpdated' % If its text has been modified

	% If the Enter key is pressed whereas the text editor enabled its
	% process_enter_key option:
	%
	% (then subscribers will receive a {EventType, [GUIObject, BestSrcId,
	% NewText, Context]} message)
	%
  | 'onEnterPressed'

	% If the length limit of the text in the editor is reached.
  | 'onTextOverflow'.



-export_type([ text_editor/0, text_editor_style/0, text_validator/0, char_pos/0,
			   text_editor_event_type/0 ]).



-doc "The settings of a text range.".
-opaque range_settings() :: wxTextAttr:wxTextAttr().


-export_type([ range_settings/0 ]).



% Operations related to text editors:
-export([ create/1, create/2, destruct/1,
		  set_default_font/2, set_text/2, add_text/2, clear/1,
		  show_position/2, show_text_end/1, get_last_position/1 ]).


% For related defines:
-include("gui_base.hrl").

% For related, internal, wx-related defines:
-include("gui_internal_defines.hrl").



% Implementation section:
%
% Currently based on wxTextCtrl (wxStyledTextCtrl would be another option, more
% powerful yet more complex / expensive to integrate).



% Type shorthands:

-type maybe_list( T ) :: list_utils:maybe_list( T ).

-type text() :: ui:text().

-type parent() :: gui:parent().
-type point() :: gui:point().
-type size() :: gui:size().

-type font() :: gui_font:font().

-type wx_opt_pair() :: gui_wx_backend:wx_opt_pair().





-doc "Creates and shows a text editor.".
-spec create( parent() ) -> text_editor().
create( Parent ) ->
	wxTextCtrl:new( Parent, gui_id:get_any_id() ).


-doc """
Creates and shows a text editor, based on the specified option(s).
""".
-spec create( maybe_list( text_editor_option() ), parent() ) ->
										text_editor().
create( Options, Parent ) ->
	WxOpts = to_wx_editor_opts( Options ),
	wxTextCtrl:new( Parent, gui_id:get_any_id(), WxOpts ).



-doc "Destructs the specified text editor.".
-spec destruct( text_editor() ) -> void().
destruct( Editor ) ->
	wxTextCtrl:destroy( Editor ).



-doc """
Sets the default font to be used by this editor.

Returns true on success, false if an error occurred (this may also mean that the
styles are not supported under this platform).
""".
-spec set_default_font( text_editor(), font() ) -> boolean().
set_default_font( Editor, Font ) ->
	RangeSettings = wxTextAttr:new(),
	wxTextAttr:setFont( RangeSettings, Font ),
	Res = wxTextCtrl:setDefaultStyle( Editor, RangeSettings ),
	wxTextAttr:destroy( RangeSettings ),
	Res.



-doc """
Sets the specified text as the new editor text content, from the start of the
control (i.e. position 0).

Does not generate an onEnterPressed event.
""".
-spec set_text( text_editor(), text() ) -> void().
set_text( Editor, NewText ) ->
	wxTextCtrl:changeValue( Editor, NewText ).



-doc """
Adds the specified text to the end of the editor text content, setting the
insertion point at its new end.
""".
-spec add_text( text_editor(), text() ) -> void().
add_text( Editor, Text ) ->
	wxTextCtrl:appendText( Editor, Text ).



-doc """
Clears the text in the editor.

Generates an onEnterPressed event.
""".
-spec clear( text_editor() ) -> void().
clear( Editor ) ->
	wxTextCtrl:clear( Editor ).



-doc """
Makes the line containing the specified position visible.
""".
-spec show_position( text_editor(), char_pos() ) -> void().
show_position( Editor, Pos ) ->
	wxTextCtrl:showPosition( Editor, Pos ).



-doc """
Shows the end of the text: makes the line containing the last position visible.
""".
-spec show_text_end( text_editor() ) -> void().
show_text_end( Editor ) ->
	show_position( Editor, get_last_position( Editor ) ).



-doc """
Returns the last position in the text stored by the editor (equal to its number
of characters).
""".
-spec get_last_position( text_editor() ) -> char_pos().
get_last_position( Editor ) ->
	wxTextCtrl:getLastPosition( Editor ).





% Helper section for text editors.


-doc """
Converts the specified text editor option(s) into the appropriate back-end
specific options.

(helper)
""".
-spec to_wx_editor_opts( maybe_list( text_editor_option() ) ) ->
											[ wx_opt_pair() ].
to_wx_editor_opts( Options ) when is_list( Options )->
	[ to_wx_text_editor_opt( O ) || O <- Options ];

% Probably a pair:
to_wx_editor_opts( Opt ) ->
	to_wx_editor_opts( [ Opt ] ).


% (helper)
-spec to_wx_text_editor_opt( text_editor_option() ) -> wx_opt_pair().
to_wx_text_editor_opt( _Opt={ position, Pos } ) ->
	{ pos, Pos };

to_wx_text_editor_opt( Opt={ size, _S } ) ->
	Opt;

to_wx_text_editor_opt( _Opt={ text, T } ) ->
	{ value, T };

to_wx_text_editor_opt( Opt={ validator, _S } ) ->
	Opt;

to_wx_text_editor_opt( _Opts={ style, Styles } ) ->
	WxStyle = lists:foldl( fun( S, Acc ) ->
		gui_generated:get_second_for_text_editor_style( S ) bor Acc end,
		_InitialAcc=0,
		_List=Styles ),

	{ style, WxStyle }.
