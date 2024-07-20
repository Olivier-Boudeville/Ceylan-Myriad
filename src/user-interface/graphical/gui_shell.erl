% Copyright (C) 2024-2024 Olivier Boudeville
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
% Creation date: Wednesday, July 17, 2024.

-module(gui_shell).

-moduledoc """
A GUI component hosting an **Erlang shell**, like an Erlang interpreter (a kind
of REPL) with which the user can interact (input/output) graphically.
""".



-doc """
""".
-opaque shell() :: wxSplitterWindow:wxSplitterWindow().


-export_type([ shell/0 ]).



-export([ create/1 ]).



% For ?gui_any_id:
-include("gui_internal_defines.hrl").

%-include_lib("wx/include/wx.hrl").

% Type shorthands:

%-type parent() :: gui:parent().


% Local type:



% Implementation notes:
%
% wxStyledTextCtrl could be used instead of wxTextCtrl.
%
% Using the Scintilla-compliant Erlang lexers (see wxSTC_ERLANG_*) could be
% convenient, yet it is a rather enormous, complex API, and there is little
% interest in syntax-highlighting the user inputs or the interpreter outputs.


-doc """
Creates a shell, in the specified parent window.
""".
%-spec create( parent() ) -> splitter_window().
create( ParentWindow ) ->
	%wxTextCtrl:new( ParentWindow, 14000 ).
	Ctrl = wxStyledTextCtrl:new( ParentWindow ),
	wxStyledTextCtrl:setLexer(Ctrl, ?wxSTC_ERLANG_UNKNOWN ),
	%wxStyledTextCtrl:setLexerLanguage(Ctrl, "erlang,,," ),
	Ctrl.
