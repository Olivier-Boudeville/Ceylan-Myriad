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

-module(spell_tree).

-moduledoc """
Support for "spelling trees", or "prefix trees", a datastructure that allows
**listing efficiently the characters that may follow up a given string**, based
on a pre-processed vocabulary.

Typically useful for autocompletion features.

See `spell_tree_test.erl` for the corresponding test.
""".


-doc """
A spelling tree / node, recording its specific substring, which is the local
prefix common to all its child spelling trees.

The root node starts with an empty prefix, and is the only one having such
prefix.

Children are sorted according to the Erlang native ordering, i.e. roughly in
lexicographic order.

For example, if having learnt the words "computer", "composer" and "boat", the
corresponding spelling tree could be: `{[], {{"boat",[]}, {"comp", [{"oser",[]},
{"uter",[]}]}}}`.

Often abbreviated as `ST`.
""".
-type spell_tree() :: {

        % The prefix specific to this node (it is to be suffixed to the one of
        % its parent):
        %
        Prefix :: [ uchar() ],

        % Tells whether this prefix was found terminating an actual word of the
        % vocabulary, or if it exists only to be suffixed by its children:
        %
        IsTerminal :: boolean(),

        % The direct, ordered children of this node, each adding its own prefix:
        [ OrderedChildSTs :: spell_tree() ] }.


-export_type([ spell_tree/0 ]).


-export([ create/0, register_string/2, register_strings/2,
          to_string/1 ]).



% Implementation notes:
%
% TODO: use lexicographic sort to stop lookups earlier.



% Type shorthands:

-type uchar() :: text_utils:uchar().
-type ustring() :: text_utils:ustring().


-doc """
Creates an empty spelling tree: its prefix is empty, it is non-terminal, and has
no child.
""".
-spec create() -> spell_tree().
create() ->
    { _Prefix="", _IsTerminal=false, _OrderedChildSTs=[] }.



-doc """
Registers the specified string in the specified spelling tree.
""".
-spec register_string( ustring(), spell_tree() ) -> spell_tree().
register_string( Str, ST={ STPrefix, IsTerminal, ChildSTs } ) ->

    cond_utils:if_defined( myriad_debug_spell_tree,
                           trace_utils:debug_fmt( "Registering '~ts' in:~n ~ts",
                                                  [ Str, to_string( ST ) ] ) ),

    case text_utils:get_any_suffix( STPrefix, Str ) of

        % Not in this spell tree:
        undefined ->
            throw( { cannot_register_string, Str, ST } );

        % Just the exact prefix has been found, so just having to record that
        % the tree is terminal, if it was not already:
        %
        _StrSuffix="" ->
            { STPrefix, _IsTerminal=true, ChildSTs };

        % Str goes beyond the current prefix; trying to find an appropriate
        % child:
        %
       StrSuffix ->
             get_tree_integrating( StrSuffix, STPrefix, IsTerminal, ChildSTs )

    end.



-doc """
Updates the specified spell tree children so that they integrate the specified
string.
""".
-spec get_tree_integrating( ustring(), ustring(), boolean(),
                            [ spell_tree() ] ) -> spell_tree().
get_tree_integrating( Str, STPrefix, IsTerminal, STs ) ->
    get_tree_integrating( Str, STPrefix, IsTerminal, STs, _Acc=[] ).


% No child found, we have to add a relevant one just for this string:
get_tree_integrating( Str, STPrefix, IsTerminal, _STs=[], Acc ) ->
    NewChildST = { Str, _IsTerminal=true, _ChildSTs=[] },

    %trace_utils:debug_fmt( "Adding ~w to tree of prefix ~w.",
    %                       [ NewChildST, STPrefix ] ),

    % Might pre-order more efficiently:
    NewSTs = lists:sort( [ NewChildST | lists:reverse( Acc ) ] ),
    { STPrefix, IsTerminal, NewSTs };

% Examining this iterated child:
get_tree_integrating( Str, STPrefix, IsTerminal,
        _STs=[ ST={ ChildPfx, ChildIsTerminal, ChildSTs } | T ], Acc ) ->

    case text_utils:get_common_prefix_with_suffixes( ChildPfx, Str ) of

        % No common prefix, try any next child:
        { _CommonPfx=[], _ChildSuffix, _StrSuffix }  ->
            get_tree_integrating( Str, STPrefix, IsTerminal, T, [ ST | Acc ] );

        % From here, common prefix found, the current child is the right one.

        % Here the string corresponds exactly to this child prefix (for example
        % Str="aaa" and ChildPfx="aaa"), so the only needed possible change is
        % to ensure that this child is terminal:
        %
        { _CommonPfx, _ChildSuffix=[], _StrSuffix=[] } ->
            UpdatedST = { ChildPfx, _Terminal=true, ChildSTs },
            % Reorders correctly, still according to the Erlang term order:
            NewSTs = lists:reverse( Acc ) ++ [ UpdatedST | T ],
            { STPrefix, IsTerminal, NewSTs };

        % Here StrSuffix is shorter than ChildSuffix (for example Str="aaa" and
        % ChildPfx="aaabb"), we have to split this child: it is to be replaced
        % by a Str-based tree (e.g. for "aaa") whose single child corresponds to
        % the rest of ChildPfx (e.g. "bb");
        %
        { CommonPfx, ChildSuffix, _StrSuffix=[] } ->
            % Update ST with the suffix to Str:
            NewST = { ChildSuffix, ChildIsTerminal, ChildSTs },
            % Insert it in new StrST:
            StrST = { CommonPfx, _Terminal=true, [ NewST ] },
            % StrST replaces the previous ST, at the same rank:
            NewSTs = lists:reverse( Acc ) ++ [ StrST | T ],
            { STPrefix, IsTerminal, NewSTs };

        % Here ChildSuffix is shorter than StrSuffix (for example Str="aaabb"
        % and ChildPfx="aaa"), thus Str has just to be referenced among the
        % children of this child:
        %
        { _CommonPfx, _ChildSuffix=[], _StrSuffix } ->
            % Just updating it then, in-place:
            %NewST = register_string( StrSuffix, ST ),
            NewST = register_string( Str, ST ),
            NewSTs = lists:reverse( Acc ) ++ [ NewST | T ],
            { STPrefix, IsTerminal, NewSTs };

       % Here these two diverged strictly (for example Str="aaaxx" and
       % ChildPfx="aaay"), we have to create an intermediary splitter node for
       % their common prefix (e.g. "aaa"), and add them both as (direct)
       % children of it:
        %
       { CommonPfx, ChildSuffix, StrSuffix } ->
            % Shortened prefix, just the rest after the common prefix:
            UpdatedST = { ChildSuffix, ChildIsTerminal, ChildSTs },
            StrST = { StrSuffix, _StrTerminal=true, [] },

            SplitterST = { CommonPfx, _InterTerminal=false,
                           lists:sort( [ UpdatedST, StrST ] ) },

            NewSTs = lists:reverse( Acc ) ++ [ SplitterST | T ],
            { STPrefix, IsTerminal, NewSTs }

    end.



-doc """
Registers the specified strings in the specified spelling tree.
""".
-spec register_strings( [ ustring() ], spell_tree() ) -> spell_tree().
register_strings( Strs, ST ) ->
    lists:foldl( fun register_string/2, _Acc0=ST, _List=Strs ).



-doc "Returns a textual description of the specified spelling tree.".
-spec to_string( spell_tree() ) -> ustring().
to_string( ST ) ->
    to_string( ST, _IndentLevel=0 ).


% (helper)
to_string( _ST={ Prefix, IsTerminal, ChildSTs }, IndentLevel ) ->

    IndentStr = text_utils:duplicate( IndentLevel, _Str="  " ),

    TermStr = case IsTerminal of

        true ->
            "terminal ";

        false ->
            ""

    end,

    ChildStr = case ChildSTs of

        [] ->
            "";

        _ ->
            NextIndentLevel = IndentLevel + 1,
            ":\n" ++ text_utils:join( _Spec=$\n,
                [ to_string( ST, NextIndentLevel )
                        || ST <- ChildSTs ] )

    end,

    text_utils:format( "~tstree for ~tsprefix '~ts'~ts",
        [ IndentStr, TermStr, Prefix, ChildStr ] ).
