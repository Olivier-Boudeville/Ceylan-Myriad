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

See also the corresponding gui_dialog:text_entry_dialog() dialog.
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
A style element of a text editor.

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


-export_type([ text_editor/0, text_editor_style/0, text_validator/0 ]).


% Operations related to text editors:
-export([ create_editor/1, create_editor/2, destruct_text_editor/1 ]).


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

-type wx_opt_pair() :: gui_wx_backend:wx_opt_pair().




-doc "Creates and shows a text editor.".
-spec create_editor( parent() ) -> text_editor().
create_editor( Parent ) ->
	wxTextCtrl:new( Parent, gui_id:get_any_id() ).


-doc """
Creates and shows a text editor, based on the specified option(s).
""".
-spec create_editor( maybe_list( text_editor_option() ), parent() ) ->
										text_editor().
create_editor( Options, Parent ) ->
	WxOpts = to_wx_editor_opts( Options ),
	wxTextCtrl:new( Parent, gui_id:get_any_id(), WxOpts ).



-doc "Destructs the specified text editor.".
-spec destruct_text_editor( text_editor() ) -> void().
destruct_text_editor( TextEditor ) ->
	wxTextCtrl:destroy( TextEditor ).



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
