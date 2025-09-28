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

    % The prefix specific to this node (it is to be suffixed to the one of its
    % parent):
    %
    Prefix :: [ uchar() ],

    % Tells whether this prefix was found terminating an actual word of the
    % vocabulary, or if it exists only to be suffixed by its children:
    %
    IsTerminal :: boolean(),

    % The direct, ordered children of this node, each adding its own prefix:
    [ ChildSTs :: spell_tree() ] }.



-doc """
A string being split into to parts: a prefix that is the shortest, unique,
unambiguous prefix, sufficient to designate that string, and the remainder of
that string.

For example the splitter for `platypus` in a given tree could be `{"plat",
"ypus"}` (thus this tree must comprise at least another string starting with
`pla`).

Often noted `"plat|ypus"`.
""".
-type splitter() :: { UnambiguousPrefix :: ustring(), Rest :: ustring() }.


-export_type([ spell_tree/0, splitter/0 ]).


-export([ create/0, create/1,
          register_string/2, register_strings/2,
          find_completions/2, get_splitters/1, resolve/2,
          to_string/1, splitter_to_string/1 ]).



% Implementation notes:

% Should child-level branching factors be significant:
%
% A lexicographic sort could be used in order to stop child lookups earlier, or
% some kind of set/tree quicker to navigate than through a basic, plain list.
%

% Should the registered strings be very numerous:
%
% Storing prefixes as binaries could reduce the memory footprint, yet probably
% that searching for common prefixes would be significantly penalised then.



% Type shorthands:

-type uchar() :: text_utils:uchar().
-type ustring() :: text_utils:ustring().
-type any_string() :: text_utils:any_string().



-doc """
Creates an empty spelling tree: its prefix is empty, it is non-terminal, and has
no child.
""".
-spec create() -> spell_tree().
create() ->
    { _Prefix="", _IsTerminal=false, _OrderedChildSTs=[] }.


-doc "Creates a spelling tree registering the specified strings.".
-spec create( [ ustring() ] ) -> spell_tree().
create( ToRegisterStrs ) ->
    register_strings( ToRegisterStrs, _InitST=create() ).



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




-doc """
Returns a sorted list of all the matches found in the specified spelling tree
for the specified string.
""".
-spec find_completions( ustring(), spell_tree() ) -> [ ustring() ].
find_completions( ToCompleteStr, ST ) ->

    cond_utils:if_defined( myriad_debug_spell_tree,
        trace_utils:debug_fmt( "Completions requested for '~ts' in:~n ~ts",
                               [ ToCompleteStr, to_string( ST ) ] ) ),

    lists:sort( find_comps( ToCompleteStr, ST ) ).



% If starting from an empty string and a wildcard ST, get all candidates:
find_comps( _ToCompleteStr="", _ST={ _STPfx="", _IsTerminal, ChildSTs } ) ->
    find_all_comps( ChildSTs, _NoPfx="" );

% Catch-all spelling tree prefix:
find_comps( ToCompleteStr, _ST={ _Pfx="", _IsTerminal, ChildSTs } ) ->

    %trace_utils:debug_fmt( "Directly searching children for '~ts'.",
    %                       [ ToCompleteStr ] ),

    Cmpltns = find_child_comps( ToCompleteStr, ChildSTs, _Prefix="" ),

    %trace_utils:debug_fmt( "Completions for '~ts': ~p.",
    %                       [ ToCompleteStr, Cmpltns ] ),

    Cmpltns;


find_comps( ToCompleteStr, ST={ Pfx, IsTerminal, ChildSTs } ) ->
    case text_utils:get_common_prefix_with_suffixes( ToCompleteStr, Pfx ) of

        % Matching exactly the spelling tree prefix:
        { _CommonPrefix, _ToCompleteSuffix="", _PfxSuffix="" } ->

            %trace_utils:debug_fmt( "Exact prefix matching for '~ts'.",
            %                       [ ToCompleteStr ] ),

            % We want the next, deeper proposals as well:
            Cmpltns = find_all_comps( ChildSTs, Pfx ),

            case IsTerminal of

                true ->
                    [ ToCompleteStr | Cmpltns ];

                false ->
                    Cmpltns

            end;

        % Child prefix is longer here, first step is to complete our suffix by
        % this prefix:
        %
        { _CommonPrefix, _ToCompleteSuffix="", _PfxSuffix } ->
            % Advancing automatically to this full (only possible) prefix:

            %trace_utils:debug_fmt( "Advancing '~ts' to '~ts'.",
            %                       [ ToCompleteStr, Pfx ] ),

            find_comps( Pfx, ST );


        % Our suffix is longer, exploring the child ST:
        { CommonPrefix, ToCompleteSuffix, _PfxSuffix="" } ->

            %trace_utils:debug_fmt(
            %    "For '~ts', exploring the children of '~ts'.",
            %    [ ToCompleteStr, Pfx ] ),

            find_child_comps( ToCompleteSuffix, ChildSTs, CommonPrefix );

        % Unrelated strings, no possible completion:
        { _CommonPrefix, _ToCompleteSuffix, _PfxSuffix } ->

            %trace_utils:debug_fmt( "No completion for '~ts'.",
            %                       [ ToCompleteStr ] ),
            []

    end.




-doc """
Returns the matches found in the specified spelling trees for the specified
string, which is assumed non-empty (hence the first matching subtree found is
the - only - right one).
""".
-spec find_child_comps( ustring(), [ spell_tree() ], ustring() ) ->
                                                [ ustring() ].
find_child_comps( _Str, _STs=[], _Pfx ) ->
    [];

find_child_comps( Str, _STs=[ ST | T ], Pfx ) ->
    case find_comps( Str, ST ) of

        [] ->
            find_child_comps( Str, T, Pfx );

        Completions ->
            [ Pfx ++ C || C <- Completions ]

    end.



-doc """
Returns all completion outcomes corresponding to the specified spelling trees,
based on the specified prefix.
""".
-spec find_all_comps( [ spell_tree() ], ustring() ) -> [ ustring() ].
find_all_comps( STs, Pfx ) ->
    find_all_comps( STs, Pfx, _Acc=[] ).


% (helper)
find_all_comps( _STs=[], _Pfx, Acc ) ->
   Acc;

find_all_comps( _STs=[ _ST={ STPfx, _IsTerminal=true, ChildSTs } | T ],
                      Pfx, Acc ) ->
    AddAcc = [ Pfx ++ STPfx | find_all_comps( ChildSTs, Pfx++STPfx ) ],
    find_all_comps( T, Pfx, AddAcc ++ Acc );

find_all_comps( _STs=[ _ST={ STPfx, _IsTerminal=false, ChildSTs } | T ],
                      Pfx, Acc ) ->
    AddAcc = find_all_comps( ChildSTs, Pfx++STPfx ),
    find_all_comps( T, Pfx, AddAcc ++ Acc ).



-doc """
Returns a list of the splitters for all strings registered in the specified
spelling tree, i.e. a list of the shortest, unambiguous prefixes/remainder pairs
for all these strings.

For example, if a spelling tree registered only "place" and "platypus", then
their splitters would be, respectively, `{"plac", "e"}` and `{"plat", "ypus"}`.
""".
-spec get_splitters( spell_tree() ) -> [ splitter() ].
% Algorithm is:
% - if a the string of a node is terminal yet this node has children, that
% string cannot be abbreviated
% - if a node does not have any child, then its unambiguous prefix stops just at
% the first letter of the prefix of this node
%
get_splitters( ST ) ->
    get_splitters( ST, _Pfx="", _AccSplits=[] ).



% (helper)
% Terminal and with no child: first letter sufficient to discriminate.
get_splitters( _ST={ _STPrefix=[ FirstChar | Rest ], _IsTerminal=true,
                         _ChildSTs=[] }, Pfx, AccSplits ) ->
    [ { Pfx ++ [ FirstChar ], Rest } | AccSplits ];

% Terminal with at least one child: cannot be abbreviated, must be complete:
get_splitters( _ST={ STPrefix, _IsTerminal=true, ChildSTs }, Pfx,
                   AccSplits ) ->
    NewPfx = Pfx ++ STPrefix,
    NewAccSplits = [ { NewPfx, "" } | AccSplits ],
    lists:foldl( fun( ST, AccSp ) ->
                    get_splitters( ST, NewPfx, AccSp )
                 end,
                 _Acc0=NewAccSplits,
                 _List=ChildSTs );

% Non-terminal, necessarily has at least one child, just recurse:
get_splitters( _ST={ STPrefix, _IsTerminal, ChildSTs }, Pfx,
                   AccSplits ) ->
    NewPfx = Pfx ++ STPrefix,
    lists:foldl( fun( ST, AccSp ) ->
                    get_splitters( ST, NewPfx, AccSp )
                 end,
                 _Acc0=AccSplits,
                 _List=ChildSTs ).



-doc """
Resolves, if possible, the specified string based on the specified spelling
tree: if this string can be unambiguously completed in a registered string,
returns that string, otherwise returns `undefined`.
""".
-spec resolve( any_string(), spell_tree() ) -> option( ustring() ).
resolve( BinStr, ST ) when is_binary( BinStr )->
    resolve( text_utils:binary_to_string( BinStr ), ST );

resolve( Str, ST ) ->
    % Could be optimised by stopping as the second match:
    case find_completions( _ToCompleteStr=Str, ST ) of

        [ SingleCmpltnStr ] ->
            SingleCmpltnStr;

        % Empty or more than one:
        _CmpltnStrs ->
            undefined

    end.



-doc "Returns a textual description of the specified spelling tree.".
-spec to_string( spell_tree() ) -> ustring().
to_string( ST ) ->
    to_string( ST, _IndentLevel=0 ).


% (helper)
to_string( _ST={ Prefix, IsTerminal, ChildSTs }, IndentLevel ) ->

    IndentStr = text_utils:duplicate( IndentLevel, _Str="  " ),

    TermStr = case IsTerminal of

        true ->
            %" [terminal]";
            " [T]";

        false ->
            ""

    end,

    ChildStr = case ChildSTs of

        [] ->
            "";

        _ ->
            NextIndentLevel = IndentLevel + 1,
            "\n" ++ text_utils:join( _Spec=$\n,
                [ to_string( ST, NextIndentLevel )
                        || ST <- ChildSTs ] )

    end,

    %text_utils:format( "~tstree for ~tsprefix '~ts'~ts",
    text_utils:format( "~ts- '~ts'~ts~ts",
        [ IndentStr, Prefix, TermStr, ChildStr ] ).



-doc "Returns a textual description of the specified splitter.".
-spec splitter_to_string( splitter() ) -> ustring().
splitter_to_string( { UnambiguousPrefix, _Rest=[] } ) ->
    UnambiguousPrefix;

splitter_to_string( { UnambiguousPrefix, Rest } ) ->
    text_utils:format( "~ts|~ts", [ UnambiguousPrefix, Rest ] ).
