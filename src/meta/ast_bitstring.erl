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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Sunday, February 4, 2018.



% Module in charge of handling bitstrings defined or used within an AST.
%
% See http://erlang.org/doc/apps/erts/absform.html for more information.
%
-module(ast_bitstring).



% Element size in a bitstring.
%
% Actual size maybe be omitted:
%
-type maybe_size() :: 'default' | ast_expression:ast_integer_expression().



% Bitstring Element Type Specifier.
%
% If TS is a type specifier A, where A is an atom, then Rep(TS) = A.
%
% If TS is a type specifier A:Value, where A is an atom and Value is an integer,
% then Rep(TS) = {A,Value}.
%
% Actual type maybe be omitted:
%
-type type_specifier() ::
		type_utils:type_name() | { type_utils:type_name(), integer() }.



% Type Specifier List (TSL).

% "A type specifier list TSL for a bitstring element is a sequence of type
% specifiers TS_1 - ... - TS_k, and Rep(TSL) = [Rep(TS_1), ..., Rep(TS_k)].
%
-type type_specifier_list() :: [ type_specifier() ].


% Alias:
%
-type tsl() :: type_specifier_list().


% An actual TSL may be omitted in some cases:
-type maybe_type_specifier_list() :: tsl() | 'default'.


% General form of a bitstring constructor, as a polymorphic type so that it can
% be specialized for dedicated contexts (ex: in guards, expressions, etc.):
%
-type constructor( ContentType ) :: { 'bin', line(),
									  [ bin_element( ContentType ) ] }.




% A binary element within a bitstring constructor:
%
-type bin_element() :: bin_element( ast_base:ast_element() ).


% A binary element within a bitstring constructor, with a specific content type:
%
-type bin_element( ContentType ) :: { 'bin_element', ast_base:line(),
					 ContentType, maybe_size(), maybe_type_specifier_list() }.


-type element_transform_fun() :: ast_transform:transform_fun( bin_element() ).


-export_type([ maybe_size/0, type_specifier/0,
			   type_specifier_list/0, tsl/0, maybe_type_specifier_list/0,
			   constructor/1, bin_element/0, bin_element/1,
			   element_transform_fun/0 ]).



-export([ transform_bin_elements/2, transform_bin_elements/3,
		  transform_bin_element/3 ]).


% Shorthands:

-type line() :: ast_base:line().
-type ast_transforms() :: ast_transform:ast_transforms().



% Transforms specified list of binary elements involved in a bitstring
% expression.
%
% Note: finally common to patterns, expressions and guard expressions.
%
transform_bin_elements( BinElements, Transforms ) ->

	% Note: context-insensitive function, considering that any kind of
	% expression can be found here.
	%
	%transform_bin_elements( BinElements, Transforms,
	%				   fun ast_expression:transform_expression/2 ) .

	[ transform_bin_element( BE, Transforms ) || BE <- BinElements ].



% Transforms specified binary element involved in a bitstring expression.
%
% Note: finally common to patterns, expressions and guard expressions.
%
% (corresponds to pattern_grp/1 in erl_id_trans)
%
-spec transform_bin_element( bin_element(), ast_transforms() ) -> bin_element().
transform_bin_element( _BinElem={ bin_element, Line, Element, Size,
								  TypeSpecifierList }, Transforms ) ->

	NewElement = ast_expression:transform_expression( Element, Transforms ),

	NewSize = case Size of

		default ->
			default;

		_ ->
			ast_expression:transform_expression( Size, Transforms )

	end,

	NewTypeSpecifierList = case TypeSpecifierList of

		default ->
			default;

		_ ->
			[ transform_type_specifier( TypeSpecifier, Transforms )
						|| TypeSpecifier <- TypeSpecifierList ]

	end,

	{ 'bin_element', Line, NewElement, NewSize, NewTypeSpecifierList };

transform_bin_element( Unexpected, _Transforms ) ->
	ast_utils:raise_error( [ unexpected_bitstring_bin_element, Unexpected ] ).




% Transforms specified list of binary elements involved in a bitstring
% expression, applying to each element the specified function to perform the
% relevant transformations (that depends on the context; ex: if being in a
% guard, in an expression, etc.).
%
-spec transform_bin_elements( [ bin_element() ], ast_transforms(),
							  element_transform_fun() ) -> [ bin_element() ].
transform_bin_elements( BinElements, Transforms, TransformFun ) ->
	[ transform_bin_element( E, Transforms, TransformFun )
	  || E <- BinElements ].





% Transforms specified binary element involved in a bitstring expression.
%
% Note: context-insensitive function, considering that any kind of expression
% can be found here (ex: a guard test).
%
-spec transform_bin_element( bin_element(), ast_transforms(),
							 element_transform_fun() ) -> bin_element().
transform_bin_element( _BinElem={ 'bin_element', Line, Element, Size,
							 TypeSpecifierList }, Transforms, TransformFun ) ->

	NewElement = TransformFun( Element, Transforms ),

	NewSize = case Size of

		default ->
			default;

		_ ->
			ast_expression:transform_expression( Size, Transforms )

	end,

	NewTypeSpecifierList = case TypeSpecifierList of

		default ->
			default;

		_ ->
			[ transform_type_specifier( TypeSpecifier, Transforms )
						|| TypeSpecifier <- TypeSpecifierList ]

	end,

	{ 'bin_element', Line, NewElement, NewSize, NewTypeSpecifierList };

transform_bin_element( Unexpected, _Transforms, _TransformFun ) ->
	ast_utils:raise_error( [ unexpected_bitstring_bin_element, Unexpected ] ).





% "A type specifier list TSL for a bitstring element is a sequence of type
% specifiers TS_1 - ... - TS_k, and Rep(TSL) = [Rep(TS_1), ..., Rep(TS_k)].
%
% If TS is a type specifier A, where A is an atom, then Rep(TS) = A.
%
% If TS is a type specifier A:Value, where A is an atom and Value is an integer,
% then Rep(TS) = {A,Value}."
%
% Note: maybe the types there shall be transformed as well.
%
transform_type_specifier( TypeSpecifier, _Transforms )
  when is_atom( TypeSpecifier ) ->
	TypeSpecifier;

transform_type_specifier( TypeSpecifier={ A, Value }, _Transforms )
  when is_atom( A ) andalso is_integer( Value ) ->
	TypeSpecifier;

transform_type_specifier( Unexpected, _Transforms ) ->
	ast_utils:raise_error(
	  [ unexpected_bitstring_type_specifier, Unexpected ] ).
