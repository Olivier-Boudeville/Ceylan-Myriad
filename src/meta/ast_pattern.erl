% Copyright (C) 2018-2018 Olivier Boudeville
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: Sunday, February 4, 2018.



% Module in charge of handling patterns defined with an AST.
%
% See the "7.3 Patterns" section of http://erlang.org/doc/apps/erts/absform.html
% for more information.
%
-module(ast_pattern).


% The description of a pattern in an AST, with line information.
%
% Ex: {match,12,X,Y}.
%
% Too many patterns to fully specify all (see "7.3 Patterns").
%
-type ast_pattern() :: ast_base:ast_element().


% The description of a sequence of patterns in an AST, with line information.
%
% A pattern sequence is simply an (ordered) list of patterns.
%
% Ex: [{match,12,X,Y}].
%
-type ast_pattern_sequence() :: [ ast_pattern() ].


-export_type([ ast_pattern/0, ast_pattern_sequence/0 ]).



-export([ transform_pattern/2, transform_pattern_sequence/2 ]).


% Shorthands:

-type ast_transforms() :: ast_transform:ast_transforms().


% Regarding patterns vs expressions:
%
% "Notice that every pattern has the same source form as some expression, and is
% represented in the same way as the corresponding expression."
%
% Thus patterns are special cases of (richer) expressions.




% Transforms specified pattern, operating relevant AST transformations onto it
% (e.g. call ones).


% List of patterns found:
%
% (note: this clause may be removed in the future, once all AST elements will
% have been specifically intercepted by a dedicated clause, and when the nature
% of their elements will be established and thus traversed specifically, rather
% than opening the possibility that each element may be a list)
%
% See section "7.3  Patterns" in http://erlang.org/doc/apps/erts/absform.html.

-spec transform_pattern( ast_utils:pattern(), ast_transforms() ) ->
							   ast_utils:pattern().
transform_pattern( PatternList, Transforms ) when is_list( PatternList ) ->

	%ast_utils:display_debug( "Intercepting pattern list ~p...",
	%						 [ PatternList ] ),

	NewPatternList = [ transform_pattern( P, Transforms ) || P <- PatternList ],

	%ast_utils:display_debug( "... returning pattern list ~p",
	%						   [ NewPatternList ] ),

	NewPatternList;


% Match pattern found:
%
% "If P is a compound pattern P_1 = P_2, then Rep(P) =
% {match,LINE,Rep(P_1),Rep(P_2)}."
%
% Left and Right members apparently patterns, not expressions:
%
transform_pattern( E={ match, Line, LeftPattern, RightPattern }, Transforms ) ->

	ast_utils:display_debug( "Intercepting match pattern ~p...", [ E ] ),

	[ NewLeftPattern, NewRightPattern ] = [ transform_pattern( P, Transforms )
							|| P <- [ LeftPattern, RightPattern ] ],

	Res = { match, Line, NewLeftPattern, NewRightPattern },

	ast_utils:display_debug( "... returning match pattern ~p", [ Res ] ),

	Res;


% Cons pattern found:
%
% "If P is a cons pattern [P_h | P_t], then Rep(P) =
% {cons,LINE,Rep(P_h),Rep(P_t)}."
%
% Head and Tail members apparently patterns, not expressions:
%
transform_pattern( E={ cons, Line, HeadPattern, TailPattern }, Transforms ) ->

	ast_utils:display_debug( "Intercepting cons pattern ~p...", [ E ] ),

	[ NewHeadPattern, NewTailPattern ] = [ transform_pattern( P, Transforms )
									   || P <- [ HeadPattern, TailPattern ] ],

	Res = { match, Line, NewHeadPattern, NewTailPattern },

	ast_utils:display_debug( "... returning cons pattern ~p", [ Res ] ),

	Res;



% Receive pattern found:
transform_pattern( E={ 'receive', Line, Clauses }, Transforms ) ->

	ast_utils:display_debug( "Intercepting receive pattern ~p...", [ E ] ),

	NewClauses = [ transform_pattern( C, Transforms ) || C <- Clauses ],

	Res = { 'receive', Line, NewClauses },

	ast_utils:display_debug( "... returning receive pattern ~p", [ Res ] ),

	Res;


% Map pattern found:
%
% "If P is a map pattern #{A_1, ..., A_k}, where each A_i is an association
% P_i_1 := P_i_2, then Rep(P) = {map,LINE,[Rep(A_1), ..., Rep(A_k)]}."
%
transform_pattern( E={ map, Line, Associations }, Transforms ) ->

	ast_utils:display_debug( "Intercepting map pattern ~p...", [ E ] ),

	NewAssociations = [ transform_pattern( A, Transforms )
						|| A <- Associations ],

	Res = { map, Line, NewAssociations },

	ast_utils:display_debug( "... returning map pattern ~p", [ Res ] ),

	Res;


% Bitstring pattern found:
%
% "If P is a bitstring pattern <<P_1:Size_1/TSL_1, ..., P_k:Size_k/TSL_k>>,
% where each Size_i is an pattern that can be evaluated to an integer, and
% each TSL_i is a type specificer list, then Rep(P) =
% {bin,LINE,[{bin_element,LINE,Rep(P_1),Rep(Size_1),Rep(TSL_1)}, ...,
% {bin_element,LINE,Rep(P_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see
% below. An omitted Size_i is represented by default. An omitted TSL_i is
% represented by default."
%
transform_pattern( Clause={ bin, Line, BinElements }, Transforms ) ->

	ast_utils:display_debug( "Intercepting bitstring pattern ~p...",
							 [ Clause ] ),

	NewBinElements = ast_bitstring:transform_bin_elements( BinElements,
						   Transforms, fun transform_pattern/2 ),

	Res = { bin, Line, NewBinElements },

	ast_utils:display_debug( "... returning bitstring pattern ~p", [ Res ] ),

	Res;


% Other pattern found:
transform_pattern( E, _Transforms ) ->
	%ast_utils:display_warning( "Letting undhandled pattern ~p as is.",
	%  [ E ] ),
	%E.
	ast_utils:raise_error( [ unexpected_pattern, E ] ).



% Transforms specified pattern sequence, operating relevant AST transformations.
%
%
% Note: the cases where the sequence is empty is managed here as well.
%
-spec transform_pattern_sequence( ast_pattern_sequence(), ast_transforms() ) ->
									  ast_pattern_sequence().
transform_pattern_sequence( Patterns, Transforms ) ->
	[ transform_pattern( G, Transforms ) || G <- Patterns ].
