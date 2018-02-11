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
% Note: a pattern is different from an expression: even if they share at least
% some types of forms, they are to be interpreted differently (ex: their
% sub-elements are of the same kind as they are, and at least some rules
% differ).
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
-type ast_variable() :: ast_type:ast_variable().


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
%
% "If Ps is a sequence of patterns P_1, ..., P_k, then Rep(Ps) = [Rep(P_1), ...,
% Rep(P_k)]."
%
% "Such sequences occur as the list of arguments to a function or fun."
%
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
% Left and Right members are patterns here (not general expressions).
%
transform_pattern( E={ 'match', Line, LeftPattern, RightPattern },
				   Transforms ) ->

	ast_utils:display_debug( "Intercepting match pattern ~p...", [ E ] ),

	NewLeftPattern = transform_pattern( LeftPattern, Transforms ),

	NewRightPattern = transform_pattern( RightPattern, Transforms ),

	Res = { match, Line, NewLeftPattern, NewRightPattern },

	ast_utils:display_debug( "... returning match pattern ~p", [ Res ] ),

	Res;


% Cons pattern found:
%
% "If P is a cons pattern [P_h | P_t], then Rep(P) =
% {cons,LINE,Rep(P_h),Rep(P_t)}."
%
transform_pattern( E={ 'cons', Line, HeadPattern, TailPattern },
				   Transforms ) ->

	ast_utils:display_debug( "Intercepting cons pattern ~p...", [ E ] ),

	[ NewHeadPattern, NewTailPattern ] =
		[ ast_pattern:transform_pattern( P, Transforms )
		  || P <- [ HeadPattern, TailPattern ] ],

	Res = { cons, Line, NewHeadPattern, NewTailPattern },

	ast_utils:display_debug( "... returning cons pattern ~p", [ Res ] ),

	Res;


% Nil pattern found:
%
% "If P is a nil pattern [], then Rep(P) = {nil,LINE}."
%
transform_pattern( E={ 'nil', _Line }, _Transforms ) ->

	ast_utils:display_debug( "Intercepting nil pattern ~p...", [ E ] ),

	Res = E,

	ast_utils:display_debug( "... returning nil pattern ~p", [ Res ] ),

	Res;


% Receive pattern found:

% "If E is a receive expression receive Cc_1 ; ... ; Cc_k end, where each Cc_i
% is a case clause, then Rep(E) = {'receive',LINE,[Rep(Cc_1), ..., Rep(Cc_k)]}."
%
transform_pattern( E={ 'receive', Line, Clauses }, Transforms ) ->

	ast_utils:display_debug( "Intercepting receive pattern ~p...", [ E ] ),

	NewClauses = [ ast_clause:transform_case_clause( C, Transforms )
				   || C <- Clauses ],

	Res = { 'receive', Line, NewClauses },

	ast_utils:display_debug( "... returning receive pattern ~p", [ Res ] ),

	Res;


% "If E is a receive expression receive Cc_1 ; ... ; Cc_k after E_0 -> B_t end,
% where each Cc_i is a case clause, E_0 is an expression, and B_t is a body,
% then Rep(E) = {'receive',LINE,[Rep(Cc_1), ..., Rep(Cc_k)],Rep(E_0),Rep(B_t)}."
%
transform_pattern( E={ 'receive', Line, Clauses, Expression, Body },
				   Transforms ) ->

	ast_utils:display_debug( "Intercepting receive pattern with after ~p...",
							 [ E ] ),

	NewClauses = [ ast_clause:transform_case_clause( C, Transforms )
				   || C <- Clauses ],

	NewExpression = ast_expression:transform_expression( Expression,
														 Transforms ),

	NewBody = ast_clause:transform_body( Body, Transforms ),

	Res = { 'receive', Line, NewClauses, NewExpression, NewBody },

	ast_utils:display_debug( "... returning receive pattern with after ~p",
							 [ Res ] ),

	Res;



% Map pattern found:
%
% "If P is a map pattern #{A_1, ..., A_k}, where each A_i is an association
% P_i_1 := P_i_2, then Rep(P) = {map,LINE,[Rep(A_1), ..., Rep(A_k)]}."
%
transform_pattern( E={ 'map', Line, Associations }, Transforms ) ->

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
transform_pattern( Clause={ 'bin', Line, BinElements }, Transforms ) ->

	ast_utils:display_debug( "Intercepting bitstring pattern ~p...",
							 [ Clause ] ),

	NewBinElements = ast_bitstring:transform_bin_elements( BinElements,
						   Transforms, fun transform_pattern/2 ),

	Res = { bin, Line, NewBinElements },

	ast_utils:display_debug( "... returning bitstring pattern ~p", [ Res ] ),

	Res;


% Tuple pattern found:
%
% "If P is a tuple pattern {P_1, ..., P_k}, then Rep(P) = {tuple,LINE,[Rep(P_1),
% ..., Rep(P_k)]}."
%
transform_pattern( Clause={ 'tuple', Line, Expressions }, Transforms ) ->

	ast_utils:display_debug( "Intercepting tuple pattern ~p...",
							 [ Clause ] ),

	NewExpressions = [ ast_expression:transform_expression( E, Transforms )
					   || E <- Expressions ],

	Res = { tuple, Line, NewExpressions },

	ast_utils:display_debug( "... returning tuple pattern ~p", [ Res ] ),

	Res;


% Variable pattern found:
%
% "If P is a universal pattern _, then Rep(P) = {var,LINE,'_'}."
% and
% "If P is a variable pattern V, then Rep(P) = {var,LINE,A}, where A is an atom
% with a printname consisting of the same characters as V."
%
transform_pattern( Clause={ 'var', Line, VariableName }, Transforms ) ->

	ast_utils:display_debug( "Intercepting variable pattern ~p...",
							 [ Clause ] ),

	NewVariable = transform_variable( VariableName, Line, Transforms ),

	Res = { var, Line, NewVariable },

	ast_utils:display_debug( "... returning variable pattern ~p", [ Res ] ),

	Res;


% Atomic value literal found:
%
% (difficult to discriminate more)
%
transform_pattern( Clause={ LiteralType, _Line, _Value }, Transforms )
  when is_atom( LiteralType ) ->

	% Maybe Value could just be sent (or no transformation be considered):
	ast_value:transform_value( Clause, Transforms );


% Other pattern found:
transform_pattern( E, _Transforms ) ->
	%ast_utils:display_warning( "Letting undhandled pattern ~p as is.",
	%  [ E ] ),
	%E.
	ast_utils:raise_error( [ unexpected_pattern, E ] ).



% Transforms specified pattern sequence, operating relevant AST transformations.
%
% Note: the cases where the sequence is empty is managed here as well.
%
-spec transform_pattern_sequence( ast_pattern_sequence(), ast_transforms() ) ->
									  ast_pattern_sequence().
transform_pattern_sequence( Patterns, Transforms ) ->
	[ transform_pattern( G, Transforms ) || G <- Patterns ].



% Transforms specified variable (possibly the universal one, '_'), operating
% relevant AST transformations.
%
-spec transform_variable( meta_utils:variable_name(), line(),
						  ast_transforms() ) -> ast_element().
transform_variable( VariableName, Line, Transforms )  ->
	ast_type:transform_variable( VariableName, Line, Transforms ).
