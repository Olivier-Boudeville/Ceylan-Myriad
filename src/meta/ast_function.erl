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
% Creation date: Wednesday, February 7, 2018.




% Module in charge of providing constructs to manage functions in an AST.
%
% Note: function clauses are managed in the ast_clause module.
%
-module(ast_function).



% Checking:
-export([ check_function_name/1, check_function_name/2,

		  check_function_id/1, check_function_id/2,
		  check_function_ids/1, check_function_ids/2,

		  check_function_type/2, check_function_type/3,
		  check_function_types/2, check_function_types/3 ]).


% Transformation:
-export([ transform_functions/2, transform_function/2,
		  transform_function_spec/2,
		  transform_spec/3, transform_function_type/3,
		  transform_function_constraints/3, transform_function_constraint/3 ]).


% Recomposition:
-export([ get_located_forms_for/2 ]).


% Shorthands:

-type function_arity() :: meta_utils:function_arity().
-type function_spec() :: meta_utils:function_spec().

%-type line() :: ast_base:line().
-type form_context() :: ast_base:form_context().

-type located_form() :: ast_info:located_form().
-type function_info() :: ast_info:function_info().
-type function_table() :: ast_info:function_table().

-type ast_transforms() :: ast_transform:ast_transforms().


% For the table macro:
-include("meta_utils.hrl").


% For the function_info record:
-include("ast_info.hrl").

% For the ast_transform record:
-include("ast_transform.hrl").



% Checks that specified function name is legit.
%
-spec check_function_name( term() ) -> basic_utils:function_name().
check_function_name( Name ) ->
	check_function_name( Name, _Context=undefined ).



% Checks that specified function name is legit.
%
-spec check_function_name( term(), form_context() ) ->
								 basic_utils:function_name().
check_function_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_function_name( Other, Context ) ->
	ast_utils:raise_error( [ invalid_function_name, Other ], Context ).






% Checks that specified function identifier is legit.
%
-spec check_function_id( term() ) -> meta_utils:function_id().
check_function_id( Id ) ->
	check_function_id( Id, _Context=undefined ).


% Checks that specified function identifier is legit.
%
-spec check_function_id( term(), form_context() ) -> meta_utils:function_id().
check_function_id( FunctionId={ FunctionName, FunctionArity }, Context ) ->
	check_function_name( FunctionName, Context ),
	ast_utils:check_arity( FunctionArity, Context ),
	FunctionId;

check_function_id( Other, Context ) ->
	ast_utils:raise_error( [ invalid_function_identifier, Other ], Context ).



% Checks that specified function identifiers are legit.
%
-spec check_function_ids( term() ) -> [ meta_utils:function_id() ].
check_function_ids( Ids ) ->
	check_function_ids( Ids, _Context=undefined ).


% Checks that specified function identifiers are legit.
%
-spec check_function_ids( term(), form_context() ) ->
								[ meta_utils:function_id() ].
check_function_ids( List, Context ) when is_list( List ) ->
	[ check_function_id( Id, Context ) || Id <- List ];

check_function_ids( Other, Context ) ->
	ast_utils:raise_error( [ invalid_function_identifier_list, Other ],
						   Context ).



% Checks that specified function type is legit.
%
-spec check_function_type( term(), function_arity() ) ->
								 meta_utils:function_type().
check_function_type( Type, FunctionArity ) ->
	check_function_type( Type, FunctionArity, _Context=undefined ).


% Checks that specified function type is legit.
%
-spec check_function_type( term(), function_arity(), form_context() ) ->
								 meta_utils:function_type().
check_function_type( _FunctionType, _FunctionArity, _Context ) ->
	%display_warning( "Function type ~p not checked (context: ~p).",
	%				 [ FunctionType, Context ] ).
	%raise_error( [ fixme_function_type ], Context ).
	ok.

%check_function_type( Other, _FunctionArity, Context ) ->
%	raise_error( [ invalid_function_type, Other ], Context ).



% Checks that specified function types are legit.
%
-spec check_function_types( term(), function_arity() ) ->
								  [ meta_utils:function_type() ].
check_function_types( Types, FunctionArity ) ->
	check_function_types( Types, FunctionArity, _Context=undefined ).


% Checks that specified function types are legit.
%
-spec check_function_types( term(), function_arity(), form_context() ) ->
								[ meta_utils:function_type() ].
check_function_types( List, FunctionArity, Context ) when is_list( List ) ->
	[ check_function_type( Type, FunctionArity, Context ) || Type <- List ];

check_function_types( Other, _FunctionArity, Context ) ->
	ast_utils:raise_error( [ invalid_function_type_list, Other ], Context ).



% Transforms the functions in specified table, based on specified transforms.
%
-spec transform_functions( function_table(), ast_transforms() ) ->
								 function_table().
transform_functions( FunctionTable, Transforms ) ->

	%ast_utils:display_debug( "Transforming functions..." ),

	%ast_utils:display_debug( "Transforming known types in function specs..." ),

	FunIdInfoPairs = ?table:enumerate( FunctionTable ),

	NewFunIdInfoPairs = [ { FunId, transform_function( FunInfo, Transforms ) }
						  || { FunId, FunInfo } <- FunIdInfoPairs ],

	?table:new( NewFunIdInfoPairs ).



% Transforms specified function.
%
-spec transform_function( function_info(), ast_transforms() ) ->
								function_info().
transform_function( FunctionInfo=#function_info{ clauses=ClauseDefs,
												 spec=MaybeLocFunSpec },
					Transforms ) ->

	% We have to transform the clauses and the spec:

	NewClauseDefs = [ ast_clause:transform_function_clause( ClauseDef,
															Transforms )
					  || ClauseDef <- ClauseDefs ],

	NewLocFunSpec = case MaybeLocFunSpec of

		undefined ->
		undefined;

		{ Loc, FunSpec } ->
			{ Loc, transform_function_spec( FunSpec, Transforms ) }

	end,

	FunctionInfo#function_info{ clauses=NewClauseDefs, spec=NewLocFunSpec }.



% Transforms the specified function specification.
%
% "If F is a function specification -Spec Name Ft_1; ...; Ft_k, where Spec is
% either the atom spec or the atom callback, and each Ft_i is a possibly
% constrained function type with an argument sequence of the same length Arity,
% then Rep(F) = {attribute,Line,Spec,{{Name,Arity},[Rep(Ft_1), ...,
% Rep(Ft_k)]}}.
%
-spec transform_function_spec( function_spec(), ast_transforms() ) ->
									 function_spec().
transform_function_spec( { 'attribute', Line, SpecType, { FunId, SpecList } },
				 #ast_transforms{ local_types=MaybeLocalTypeTable,
								  remote_types=MaybeRemoteTypeTable } ) ->

	% Ex for '-spec f( type_a() ) -> type_b().':

	% SpecList = [ {type,652,'fun',
	% [{type,652,product,[{user_type,652,type_a,[]}]},
	% {user_type,652,type_b,[]}] } ]
	%

	%ast_utils:display_trace( "SpecList = ~p", [ SpecList ] ),
	NewSpecList = [ transform_spec( Spec, MaybeLocalTypeTable,
								MaybeRemoteTypeTable ) || Spec <- SpecList ],

	{ 'attribute', Line, SpecType, { FunId, NewSpecList } }.




% Transforms the specified function specification.
%
% (corresponds to function_type_list/1 in erl_id_trans)
%
% "If Ft is a constrained function type Ft_1 when Fc, where Ft_1 is a function
% type and Fc is a function constraint, then Rep(T) =
% {type,LINE,bounded_fun,[Rep(Ft_1),Rep(Fc)]}."
%
transform_spec( { 'type', Line, 'bounded_fun',
				  [ FunctionType, FunctionConstraint ] },
				MaybeLocalTypeTable, MaybeRemoteTypeTable ) ->

	NewFunctionType = transform_function_type( FunctionType,
							 MaybeLocalTypeTable, MaybeRemoteTypeTable ),

	NewFunctionConstraint = transform_function_constraints( FunctionConstraint,
							   MaybeLocalTypeTable, MaybeRemoteTypeTable ),

	{ 'type', Line, 'bounded_fun', [ NewFunctionType, NewFunctionConstraint ] };


transform_spec( OtherSpec, MaybeLocalTypeTable, MaybeRemoteTypeTable ) ->
	transform_function_type( OtherSpec, MaybeLocalTypeTable,
							 MaybeRemoteTypeTable ).



% (helper, corresponding to function_type/1 in erl_id_trans)
%
% "If Ft is a function type (T_1, ..., T_n) -> T_0, where each T_i is a type,
% then Rep(Ft) = {type,LINE,'fun',[{type,LINE,product,[Rep(T_1), ...,
% Rep(T_n)]},Rep(T_0)]}."
%
transform_function_type( { 'type', LineFirst, 'fun',
	   [ { 'type', LineSecond, 'product', ParamTypes }, ResultType ] },
						 MaybeLocalTypeTable, MaybeRemoteTypeTable ) ->

	[ NewResultType | NewParamTypes ] = ast_type:transform_types(
			[ ResultType | ParamTypes ], MaybeLocalTypeTable,
			MaybeRemoteTypeTable ),

	{ 'type', LineFirst, 'fun',
	  [ { 'type', LineSecond, 'product', NewParamTypes }, NewResultType ] };

transform_function_type( UnexpectedFunType, _MaybeLocalTypeTable,
						 _MaybeRemoteTypeTable ) ->
	ast_utils:raise_error( [ unexpected_function_type, UnexpectedFunType ] ).




% (helper, corresponding to function_constraint/1 in erl_id_trans)
%
% "A function constraint Fc is a non-empty sequence of constraints C_1, ...,
% C_k, and Rep(Fc) = [Rep(C_1), ..., Rep(C_k)]."
%
transform_function_constraints( FunctionConstraints, MaybeLocalTypeTable,
								MaybeRemoteTypeTable ) ->

	[ transform_function_constraint( FC, MaybeLocalTypeTable,
				 MaybeRemoteTypeTable ) || FC <- FunctionConstraints ].



% "If C is a constraint V :: T, where V is a type variable and T is a type, then
% Rep(C) = {type,LINE,constraint,[{atom,LINE,is_subtype},[Rep(V),Rep(T)]]}. "
%
transform_function_constraint( { 'type', Line, 'constraint',
		[ AtomConstraint={ atom, _LineAtom, _SomeAtom }, [ TypeVar, Type ] ] },
		MaybeLocalTypeTable, MaybeRemoteTypeTable ) ->

	NewTypeVar = ast_type:transform_type( TypeVar, MaybeLocalTypeTable,
										  MaybeRemoteTypeTable ),

	NewType = ast_type:transform_type( Type, MaybeLocalTypeTable,
									   MaybeRemoteTypeTable ),

	{ 'type', Line, 'constraint', [ AtomConstraint, [ NewTypeVar, NewType ] ] }.



% Returns a pair made of (two) lists of located forms corresponding to:
%
% - all the function export declarations that are described in the specified
% function export table
%
% - all the function definitions and specs that are described in the specified
% function table
%
-spec get_located_forms_for( ast_info:function_export_table(),
							 ast_info:function_table() ) ->
								   { [ located_form() ], [ located_form() ] }.
get_located_forms_for( FunctionExportTable, FunctionTable ) ->

	FunExportInfos = ?table:enumerate( FunctionExportTable ),

	%ast_utils:display_debug( "FunExportInfos = ~p",
	%  [ FunExportInfos ] ),

	FunExportLocDefs = [ { Loc, { attribute, Line, export, FunIds } }
				   || { Loc, { Line, FunIds } } <- FunExportInfos ],


	% Dropping the keys (the function_id(), i.e. function identifiers), focusing
	% on their associated function_info():
	%
	FunInfos = ?table:values( FunctionTable ),

	FunctionLocDefs = lists:foldl( fun( #function_info{
										   name=Name,
										   arity=Arity,
										   location=Location,
										   line=Line,
										   clauses=Clauses,
										   spec=MaybeSpec }, Acc ) ->

						 LocFunForm = { Location,
								  { function, Line, Name, Arity, Clauses } },

						 case MaybeSpec of

							 undefined ->
								 [ LocFunForm | Acc ];

							 LocSpecForm ->
								 [ LocSpecForm, LocFunForm | Acc ]

						 end

				 end,
				 _Acc0=[],
				 _List=FunInfos ),

	{ FunExportLocDefs, FunctionLocDefs }.
