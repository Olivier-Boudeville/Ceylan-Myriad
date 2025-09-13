% Copyright (C) 2025-2025 Olivier Boudeville
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
% Creation date: Wednesday, September 10, 2025.

-module(spell_tree_test).

-moduledoc """
Unit tests for the `spell_tree` features.

See the `spell_tree` tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

    test_facilities:display( "Testing spell trees." ),

    EmptyST = spell_tree:create(),

    test_facilities:display( "Empty spell tree: ~ts.",
                             [ spell_tree:to_string( EmptyST ) ] ),

    AllStrs = [ "test", "place", "platypus", "porridge", "plate", "placebo",
                "eric", "eric", "erlang" ],

    OneWST = spell_tree:register_string( hd( AllStrs ), EmptyST ),

    test_facilities:display( "One-word spell tree: ~ts.",
                             [ spell_tree:to_string( OneWST ) ] ),


    MultiWST = spell_tree:register_strings( AllStrs, OneWST ),

    test_facilities:display( "Multiple-word spell tree: ~ts.",
                             [ spell_tree:to_string( MultiWST ) ] ),


    ToCompleteStr = "pl",

    CompletionOutcome = spell_tree:find_completions( ToCompleteStr, MultiWST ),

    test_facilities:display( "Completions determined for prefix '~ts' "
        "in the following tree:~n ~ts are: ~p.~n",
        [ ToCompleteStr, spell_tree:to_string( MultiWST ),
          CompletionOutcome ] ),


    test_facilities:display( "Testing a series of completions, all based "
        "on the registering of:~n ~p.", [ lists:sort( AllStrs ) ] ),

    TestStrs = AllStrs
        ++ [ "", "other", "p", "pla", "te", "er", "erica", "xyz" ],

    [ test_facilities:display( "Completions determined for prefix '~ts': ~p.",
        [ Pfx, spell_tree:find_completions( Pfx, MultiWST ) ] )
                || Pfx <- TestStrs ],

	test_facilities:stop().
