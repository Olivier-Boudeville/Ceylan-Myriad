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



% Module in charge of handling types, but also variables and values defined with
% an AST.
%
% See the "7.7 Types" section of http://erlang.org/doc/apps/erts/absform.html
% for more information.
%
-module(ast_type).




% Section for types about types.



% An in-AST definition of a type:
-type ast_type_definition() :: ast_base:form().


% Reference to a built-in type, in an AST.
%
% Ex:
% - {type,45,atom,[]}                       -- for atom()
% - {type,44,list,[{type,44,boolean,[]}]}   -- for [ boolean() ]
%
% Note: the order of fields matters (not arbitrary, to correspond to the actual
% AST terms)
%
% Not possible: -record( builtin_type, {
-record( type, {

		   % Line of this form in the current source file:
		   line = 0 :: line(),

		   % Name of the target type:
		   name :: type_name(),

		   % Type variables, i.e. types on which this type depends:
		   variables = [] :: [ ast_type() ]

}).

-type ast_builtin_type() :: #type{}.



% Reference to a user-defined (local) type, in an AST.
%
% Ex: {user_type,45,foo,[{type,45,atom,[]}]}     -- for foo( atom() )
%
% Note: the order of fields matters (not arbitrary, to correspond to the actual
% AST terms)
%
-record( user_type, {

		   % Line of this form in the current source file:
		   line = 0 :: line(),

		   % Name of the target type:
		   name :: type_name(),

		   % Type variables, i.e. types on which this type depends:
		   variables = [] :: [ ast_type() ]

}).

-type ast_user_type() :: #user_type{}.



% Reference to a remote type, in an AST.
%
% Example for basic_utils:maybe( float() ):
% {remote_type,43,[{atom,43,basic_utils},{atom,43,maybe},[{type,43,float,[]}]]}
%
% Note: the order of fields matters (not arbitrary, to correspond to the actual
% AST terms)
%
-record( remote_type, {

		   % Line of this form in the current source file:
		   line = 0 :: line(),

		   % More precisely, a list of three elements, two atoms and a list of
		   % type variables, like in:
		   % [ {atom,43,basic_utils}, {atom,43,maybe}, [{type,43,float,[]}] ]
		   spec :: [ ast_builtin_type() | [ ast_type() ] ]

}).

-type ast_remote_type() :: #remote_type{}.




% Any kind of reference onto a type:
%
-type ast_type() :: ast_builtin_type() | ast_user_type() | ast_remote_type().

-type maybe_ast_type() :: basic_utils:maybe( ast_type() ).


% May be constrained or not (see http://erlang.org/doc/apps/erts/absform.html):
%
%-type function_type().


% The description of a field of a record.
%
% Ex : {typed_record_field, {record_field,76, {atom,76,my_index}},
%               {remote_type,76, [{atom,76,linear}, {atom,76,coordinate}, []]}},
%
-type ast_field_description() :: tuple().



% Includes '_':
-type ast_variable_name() :: atom().


% Variable definition:
%
-type ast_variable() :: { 'var', line(), ast_variable_name() }.




-export_type([ ast_type_definition/0,
			   ast_builtin_type/0, ast_user_type/0, ast_remote_type/0,
			   ast_type/0, maybe_ast_type/0,
			   ast_field_description/0,
			   ast_variable_name/0, ast_variable/0 ]).


% Transformations:
-export([ transform_type/3, transform_association_type/3 ]).




% Currently, the overall transform record is not propagated when transforming
% types (only the two local/remote type tables are), this may change in the
% future.



% Forging AST types:
%
% Note that when using the forge_*_type/N functions, type variables are expected
% to be already forged.
%
-export([ forge_boolean_type/0, forge_boolean_type/1,
		  forge_atom_type/0, forge_atom_type/1,
		  forge_pid_type/0, forge_pid_type/1,
		  forge_integer_type/0, forge_integer_type/1,
		  forge_float_type/0, forge_float_type/1,
		  forge_tuple_type/1, forge_tuple_type/2,
		  forge_list_type/1, forge_list_type/2,
		  forge_union_type/1, forge_union_type/2,
		  forge_builtin_type/3, forge_local_type/3,
		  forge_remote_type/4, forge_remote_type/6,
		  forge_type_variable/2 ]).


% Checking:
-export([ check_type_name/1, check_type_name/2,
		  check_type_definition/1, check_type_definition/2,

		  check_record_name/1, check_record_name/2,
		  check_type_id/1, check_type_id/2,
		  check_type_ids/1, check_type_ids/2,

		  check_type_variable/1, check_type_variable/2,
		  check_type_variables/1, check_type_variables/2,

		  check_ast_atom/1 ]).


% Recomposition:
-export([ get_located_forms_for/2 ]).



% Shorthands:

-type line() :: ast_base:line().

-type module_name() :: meta_utils:module_name().
-type variable_name() :: meta_utilsvariable_name().

-type type_name() :: type_utils:type_name().

-type form_context() :: ast_base:form_context().



% For the table macro:
-include("meta_utils.hrl").



% Transformation section.


% Transforming types: traversing them recursively according to their specified
% structure, applying on them the specified transformations.
%
% Currently not going for a fully specialised, strict and 'just sufficient'
% traversal as permitted by http://erlang.org/doc/apps/erts/absform.html; yet
% still getting inspiration from its section 7.7.
%
% We currently consider that all type definitions correspond to an
% ast_utils:ast_type(), i.e. one of:
%
% - ast_utils:ast_builtin_type(): { type, Line, TypeName, TypeVars },
% where TypeVars are often (not always) a list; ex: {type,LINE,union,[Rep(T_1),
% ..., Rep(T_k)]} or {type,LINE,map,any}; we manage specifically the most common
% type designators, and traverse generically the others
%
% - ast_utils:ast_remote_type(): { remote_type, Line, [ ModuleType, TypeName,
% TypeVars ] }
%
% - ast_utils:ast_user_type(): { user_type, Line, TypeName, TypeVars }
%
%
% Notes:
%
% - clauses ordered according to the first atom (all plain types, then all
% remote types, then all user types)
%
% - records like #type, #user_type, could be used instead
%
% (helper)
%
-spec transform_type( ast_utils:ast_type(),
		basic_utils:maybe( meta_utils:local_type_transform_table() ),
		basic_utils:maybe( meta_utils:remote_type_transform_table() ) ) ->
						   ast_utils:ast_type().

% Handling tuples:

% Fully-qualified tuple type found, ex:
% {type,42,tuple,[{type,42,integer,[]},{type,42,float,[]}]}
%
% "If T is a tuple type {T_1, ..., T_k}, then
% Rep(T) = {type,LINE,tuple,[Rep(T_1), ..., Rep(T_k)]}."
%
transform_type( _TypeDef={ 'type', Line, 'tuple', ElementTypes },
				LocalTransformTable, RemoteTransformTable )
  when is_list( ElementTypes ) ->
	{ type, Line, tuple,
	  [ transform_type( Elem, LocalTransformTable, RemoteTransformTable )
		|| Elem <- ElementTypes ] };


% General tuple type found (i.e. tuple()):
%
% "If T is a tuple type tuple(), then Rep(T) = {type,LINE,tuple,any}."
%
transform_type( TypeDef={ 'type', _Line, 'tuple', 'any' }, _LocalTransformTable,
				_RemoteTransformTable ) ->
	TypeDef;

transform_type( TypeDef={ 'type', Line, 'tuple', _Any }, _LocalTransformTable,
				_RemoteTransformTable ) ->
	ast_utils:raise_error( [ unexpected_typedef_tuple_form, TypeDef ],
						   _Context=Line );



% Handling lists:


% Fully-qualified list type found, ex:
% {type,43,list,[{type,43,boolean,[]}]}
%
% Lacking specification in the doc, extrapolated to:
%
% "If T is a list of elements of type A, then Rep(T) = {type,LINE,list,Rep(A)}."
%
transform_type( _TypeDef={ 'type', Line, 'list', [ ElementType ] },
				LocalTransformTable, RemoteTransformTable ) ->

	NewElementType = transform_type( ElementType, LocalTransformTable,
								RemoteTransformTable ),

	{ type, Line, list, [ NewElementType ] };


% General list type found (i.e. list()):
%
% Lacking specification in the doc, extrapolated to:
%
% "If T is a list type list(), then Rep(T) = {type,LINE,list,any}."
%
transform_type( TypeDef={ 'type', _Line, 'list', 'any' }, _LocalTransformTable,
		   _RemoteTransformTable ) ->
	TypeDef;


% Yes, at least in some cases list() may be translated as {type,LINE,list,[]}:
transform_type( TypeDef={ 'type', _Line, 'list', [] }, _LocalTransformTable,
		   _RemoteTransformTable ) ->
	TypeDef;


transform_type( TypeDef={ 'type', _Line, 'list', _Any }, _LocalTransformTable,
				_RemoteTransformTable ) ->
	ast_utils:raise_error( [ unexpected_typedef_list_form, TypeDef ] );


% Empty list type found (i.e. []):
%
% "If T is the empty list type [], then Rep(T) = {type,Line,nil,[]}"
%
transform_type( TypeDef={ 'type', _Line, 'nil', [] }, _LocalTransformTable,
				_RemoteTransformTable ) ->
	TypeDef;



% Handling binaries:

% "If T is a bitstring type <<_:M,_:_*N>>, where M and N are singleton integer
% types, then Rep(T) = {type,LINE,binary,[Rep(M),Rep(N)]}."
%
transform_type( _TypeDef={ 'type', Line, 'binary', [ M, N ] },
				LocalTransformTable, RemoteTransformTable ) ->

	% To be removed once ever seen displayed:
	%ast_utils:display_warning( "Not transforming binary elements ~p and ~p.",
	%						   [ M, N ] ),

	% Finally transformed, as managed in erl_id_trans:

	NewM = transform_type( M, LocalTransformTable, RemoteTransformTable ),

	NewN = transform_type( N, LocalTransformTable, RemoteTransformTable ),

	{ type, Line, binary, [ NewM, NewN ] };



% "If T is an integer range type L .. H, where L and H are singleton integer
% types, then Rep(T) = {type,LINE,range,[Rep(L),Rep(H)]}."
%
transform_type( _TypeDef={ 'type', _Line, 'range', [ L, H ] },
				LocalTransformTable, RemoteTransformTable ) ->

	% To be removed once ever seen displayed:
	%ast_utils:display_warning( "Not transforming range bound ~p and ~p.",
	%						   [ L, H ] ),

	% Finally transformed, as managed in erl_id_trans:

	NewL = transform_type( L, LocalTransformTable, RemoteTransformTable ),

	NewH = transform_type( H, LocalTransformTable, RemoteTransformTable ),

	{ type, Line, range, [ NewL, NewH ] };


% Handling maps:

% "If T is a map type map(), then Rep(T) = {type,LINE,map,any}."
%
transform_type( TypeDef={ 'type', _Line, 'map', 'any' }, _LocalTransformTable,
				_RemoteTransformTable ) ->
	TypeDef;


% "If T is a map type #{A_1, ..., A_k}, where each A_i is an association type,
% then Rep(T) = {type,LINE,map,[Rep(A_1), ..., Rep(A_k)]}."
%
transform_type( _TypeDef={ 'type', Line, 'map', AssocTypes },
				LocalTransformTable, RemoteTransformTable ) ->

	NewAssocTypes = [ transform_association_type( T, LocalTransformTable,
						RemoteTransformTable ) || T <- AssocTypes ],

	{ type, Line, map, NewAssocTypes };



% Handling lambda functions:


% "If T is a fun type fun(), then Rep(T) = {type,LINE,'fun',[]}."
transform_type( TypeDef={ 'type', _Line, 'fun', [] }, _LocalTransformTable,
				_RemoteTransformTable ) ->
	TypeDef;


% "If T is a fun type fun((...) -> T_0), then Rep(T) =
% {type,LINE,'fun',[{type,LINE,any},Rep(T_0)]}."
%
transform_type( _TypeDef={ 'type', Line1, 'fun',
						   [ Any={ 'type', _Line2, 'any' }, ResultType ] },
				LocalTransformTable, RemoteTransformTable ) ->

	NewResultType = transform_type( ResultType, LocalTransformTable,
								   RemoteTransformTable ),

	{ type, Line1, 'fun', [ Any, NewResultType ] };



% Handling union types:
%
% "If T is a type union T_1 | ... | T_k, then Rep(T) =
% {type,LINE,union,[Rep(T_1), ..., Rep(T_k)]}."
%
transform_type( _TypeDef={ 'type', Line, 'union', UnifiedTypes },
				LocalTransformTable, RemoteTransformTable ) ->

	NewUnifiedTypes = [ transform_type( T, LocalTransformTable,
									   RemoteTransformTable )
						|| T <- UnifiedTypes ],

	{ type, Line, union, NewUnifiedTypes };


% Simple built-in type, like 'boolean()', translating in '{ type, 57, boolean,
% [] }':
%
transform_type( TypeDef={ 'type', _Line, BuiltinType, _TypeVars=[] },
				_LocalTransformTable, _RemoteTransformTable ) ->

	case lists:member( BuiltinType, type_utils:get_immediate_types() ) of

		true ->
			TypeDef;

		false ->
			ast_utils:display_warning( "Not expecting type '~s', assuming "
									   "simple builtin type (in ~p).",
									   [ BuiltinType, TypeDef ] ),
			TypeDef

	end;


% "If T is a record type #Name{F_1, ..., F_k}, where each F_i is a record field
% type, then Rep(T) = {type,LINE,record,[Rep(Name),Rep(F_1), ..., Rep(F_k)]}."
%
% Like '-type my_record() :: #my_record{}.', translating in { type, 89, record,
% [ {atom, 89, my_record } ] }:
%
transform_type( _TypeDef={ 'type', Line, 'record',
				   _TypeVars=[ N={ atom, _LineT, _RecordName } | FieldTypes ] },
				LocalTransformTable, RemoteTransformTable ) ->

	NewFieldTypes = [ transform_field_type( FT, LocalTransformTable,
							RemoteTransformTable ) || FT <- FieldTypes ],

	{ type, Line, record, [ N, NewFieldTypes ] };


% Known other built-in types (catch-all for all remaining 'type'):
%
%
transform_type( TypeDef={ 'type', Line, BuiltinType, TypeVars },
				LocalTransformTable, RemoteTransformTable )
  when is_list( TypeVars ) ->

	ast_utils:display_warning( "Not expecting type '~s', assuming unknown "
							   "parametrized builtin type (in ~p).",
							   [ BuiltinType, TypeDef ] ),

	NewTypeVars = [ transform_type( T, LocalTransformTable,
							   RemoteTransformTable ) || T <- TypeVars ],

	{ type, Line, BuiltinType, NewTypeVars };




% Handling user type (necessary a local one):


transform_type( _TypeDef={ 'user_type', Line, TypeName, TypeVars },
				LocalTransformTable, RemoteTransformTable ) ->

	TypeArity = length( TypeVars ),

	NewTypeVars = [ transform_type( T, LocalTransformTable,
							   RemoteTransformTable ) || T <- TypeVars ],

	% Note: no user-to-local type rewriting deemed useful.

	Outcome = case LocalTransformTable of

		undefined ->
			unchanged;

		_ ->

			% Returning the new type information:
			case ?table:lookupEntry( { TypeName, TypeArity },
									 LocalTransformTable ) of

				% Module *and* type overridden:
				{ value, E={ _NewModuleName, _NewTypeName } } ->
					E;

				% Same type, only module overridden:
				% (never happens, as module always specified in table)
				%{ value, NewModuleName } when is_atom( NewModuleName ) ->
				%	{ NewModuleName, TypeName };

				{ value, TransformFun } when is_function( TransformFun ) ->
					TransformFun( TypeName, TypeArity );

				key_not_found ->

					% Maybe a wildcard arity was defined then?
					case ?table:lookupEntry( { TypeName, _AnyArity='_' },
											 LocalTransformTable ) of

						{ value, E={ _NewModuleName, _NewTypeName } } ->
							E;

						% Same type, only module overridden:
						% (was commented-out out, but may happen?)
						%
						{ value, NewModuleName }
						  when is_atom( NewModuleName ) ->
							{ NewModuleName, TypeName };

						{ value, TransformFun }
						  when is_function( TransformFun ) ->
							TransformFun( TypeName, TypeArity );

						key_not_found ->
							% Nope, let it as it is:
							unchanged

					end

			end

	end,

	case Outcome of

		unchanged ->
			% TypeDef with only updated TypeVars:
			{ user_type, Line, TypeName, NewTypeVars };

		{ SetModuleName, SetTypeName } ->
			forge_remote_type( SetModuleName, SetTypeName, NewTypeVars, Line )

	end;



% Handling remote user type:


% "If T is a remote type M:N(T_1, ..., T_k), then Rep(T) =
% {remote_type,LINE,[Rep(M),Rep(N),[Rep(T_1), ..., Rep(T_k)]]}."
%
% First, the special (yet most common) case of immediate values specified for
% module and type:
%
transform_type( _TypeDef={ 'remote_type', Line,
						 [ M={ atom, LineM, ModuleName },
						   T={ atom, LineT, TypeName }, TypeVars ] },
		   LocalTransformTable, RemoteTransformTable ) ->

	NewTypeVars = [ transform_type( TypeVar, LocalTransformTable,
							   RemoteTransformTable ) || TypeVar <- TypeVars ],

	TypeArity = length( TypeVars ),

	% Returning the new type information:
	Outcome = case RemoteTransformTable of

		undefined ->
			unchanged;

		_ ->

			case ?table:lookupEntry( { ModuleName, TypeName, TypeArity },
									 RemoteTransformTable ) of

				 % Module *and* type overridden:
				{ value, E={ _NewModuleName, _NewTypeName } } ->
					E;

				 % Same type; only the module is overridden:
				{ value, NewModuleName } when is_atom( NewModuleName ) ->
					{ NewModuleName, TypeName };

				{ value, TransformFun } when is_function( TransformFun ) ->
					TransformFun( ModuleName, TypeName, TypeArity );

				key_not_found ->

					% Maybe a wildcard arity was defined for that type then?

					AnyArity = '_',

					case ?table:lookupEntry( { ModuleName, TypeName, AnyArity },
											 RemoteTransformTable ) of

						{ value, E={ _NewModuleName, _NewTypeName } } ->
							E;

						 % Same type, only module overridden (never happens by
						 % design):
						 %{ value, NewModuleName }
						 %        when is_atom( NewModuleName ) ->
						 %    { NewModuleName, TypeName };

						{ value, TransformFun }
						  when is_function( TransformFun ) ->
							TransformFun( ModuleName, TypeName, TypeArity );

						key_not_found ->

							% Nope; maybe a wildcard type (and arity) then?
							case ?table:lookupEntry( { ModuleName, _AnyType='_',
										 AnyArity }, RemoteTransformTable ) of

								{ value, E={ _NewModuleName, _NewTypeName } } ->
									E;

								% Same type, only module overridden:
								{ value, NewModuleName }
								  when is_atom( NewModuleName ) ->
									{ NewModuleName, TypeName };

								{ value, TransformFun }
								  when is_function( TransformFun ) ->
									TransformFun( ModuleName, TypeName,
												  TypeArity );

								key_not_found ->
									% Nope, let it as it is:
									unchanged

							end

					end

			end

	end,

	case Outcome of

		unchanged ->
			% TypeDef with updated TypeVars:
			{ remote_type, Line, [ M, T, NewTypeVars ] };

		{ SetModuleName, SetTypeName } ->
			forge_remote_type( SetModuleName, SetTypeName, NewTypeVars, Line,
							   LineM, LineT )

	end;


% Second, the case where at least either the module or the type name is not
% immediate:
%
transform_type( _TypeDef={ 'remote_type', Line1, [ Mod, Typ, TypeVars ] },
				LocalTransformTable, RemoteTransformTable ) ->

	% Wondering what these could be:
	ast_utils:display_debug( "Transforming a remote type whose module and type "
							 "information are ~p and ~p.", [ Mod, Typ ] ),

	[ NewMod, NewTyp ] = [ transform_type( T, LocalTransformTable,
							   RemoteTransformTable ) || T <- [ Mod, Typ ] ],

	NewTypeVars = [ transform_type( T, LocalTransformTable,
								   RemoteTransformTable ) || T <- TypeVars ],

	{ remote_type, Line1, [ NewMod, NewTyp, NewTypeVars ] };


% Variable declaration, possibly obtained through declarations like:
% -type my_type( T ) :: other_type( T ).
% or:
% -opaque tree( T ) :: { T, [ tree(T) ] }.
transform_type( TypeDef={ 'var', Line, TypeName }, LocalTransformTable,
				RemoteTransformTable ) ->

	%NewVar = transform_type_variable( TypeName, Line, SomeTransform ),
	TypeDef;


% Annotated type, most probably obtained from the field of a record like:
%  pointDrag :: {X::integer(), Y::integer()}}
%
% Resulting then in:
% {typed_record_field,
%		   {record_field,342,{atom,342,pointDrag}},
%		   {type,342,tuple,
%			   [{ann_type,342,[{var,342,'X'},{type,342,integer,[]}]},
%				{ann_type,342,
%					[{var,342,'Y'},{type,342,integer,[]}]} ] }}
%
transform_type( _TypeDef={ 'ann_type', Line,
					  [ Var={ 'var', Line2, VariableName }, InternalTypeDef ] },
				LocalTransformTable, RemoteTransformTable ) ->

	%NewVar = transform_type_variable( VariableName, Line2, _SomeTransform ),
	NewVar = Var,

	NewInternalTypeDef = transform_type( InternalTypeDef, LocalTransformTable,
										 RemoteTransformTable ),

	{ ann_type, Line, [ NewVar, NewInternalTypeDef ] };



% Binary operator.
%
% "If T is an operator type T_1 Op T_2, where Op is a binary operator (this is
% an occurrence of an expression that can be evaluated to an integer at compile
% time), then Rep(T) = {op,LINE,Op,Rep(T_1),Rep(T_2)}."
%
transform_type( TypeDef={ 'op', Line, Operator, LeftType, RightType },
				LocalTransformTable, RemoteTransformTable ) ->

	NewLeftType = transform_type( LeftType, LocalTransformTable,
								  RemoteTransformTable ),

	NewRightType = transform_type( RightType, LocalTransformTable,
								   RemoteTransformTable ),

	{ op, Line, Operator, NewLeftType, NewRightType };



% Unary operator.
%
% "If T is an operator type Op T_0, where Op is a unary operator (this is an
% occurrence of an expression that can be evaluated to an integer at compile
% time), then Rep(T) = {op,LINE,Op,Rep(T_0)}."
%
transform_type( TypeDef={ 'op', Line, Operator, OperandType },
				LocalTransformTable, RemoteTransformTable ) ->

	NewOperandType = transform_type( OperandType, LocalTransformTable,
									 RemoteTransformTable ),

	{ op, Line, Operator, NewOperandType };



% Immediate values like {atom,42,foobar}, possibly obtained through
% declarations like: -type my_type() :: integer() | 'foobar'.
%
% Note: this clause must remain at the end of the series, as a near-default one.
%
transform_type( TypeDef={ TypeName, _Line, Value }, _LocalTransformTable,
				_RemoteTransformTable ) ->

	% For some unknown reason, in erl_id_trans.erl only a subset of the
	% immediate types are managed (in type/1; ex: 'integer' but not 'float'):
	%
	%AllowedTypes = type_utils:get_immediate_types(),
	AllowedTypes = [ atom, integer ],

	case lists:member( TypeName, AllowedTypes ) of

		true ->
			%ast_value:transform_value( TypeDef, _SomeTransforms ),
			TypeDef;

		false ->
			ast_utils:raise_error( [ unexpected_immediate_value, TypeDef ] )

	end;


transform_type( TypeDef, _LocalTransformTable, _RemoteTransformTable ) ->
	ast_utils:raise_error( [ unhandled_typedef, TypeDef ] ).





% Transforming association types (from maps).


% "If A is an association type K => V, where K and V are types, then Rep(A) =
% {type,LINE,map_field_assoc,[Rep(K),Rep(V)]}."
%
-spec transform_association_type( ast_utils:ast_type(),
		basic_utils:maybe( meta_utils:local_type_transform_table() ),
		basic_utils:maybe( meta_utils:remote_type_transform_table() ) ) ->
						   ast_utils:ast_type().
transform_association_type( { 'type', Line, 'map_field_assoc',
							  Types=[ _K, _V ] },
							LocalTransformTable, RemoteTransformTable ) ->

	NewTypes = [ transform_type( T, LocalTransformTable, RemoteTransformTable )
				 || T <- Types ],

	{ type, Line, map_field_assoc, NewTypes };


% "If A is an association type K := V, where K and V are types, then Rep(A) =
% {type,LINE,map_field_exact,[Rep(K),Rep(V)]}.
%
transform_association_type( { 'type', Line, 'map_field_exact',
							  Types=[ _K, _V ] },
							LocalTransformTable, RemoteTransformTable ) ->

	NewTypes = [ transform_type( T, LocalTransformTable, RemoteTransformTable )
				 || T <- Types ],

	{ type, Line, map_field_exact, NewTypes }.



% Transforming field types (from records).

% "If F is a record field type Name :: Type, where Type is a type, then Rep(F) =
% {type,LINE,field_type,[Rep(Name),Rep(Type)]}."
%
transform_field_type( { 'type', Line, 'field_type',
						[ N={ atom, LineN, _FieldName }, FieldType ]  },
					  LocalTransformTable, RemoteTransformTable ) ->

	NewFieldType = transform_type( FieldType, LocalTransformTable,
								   RemoteTransformTable ),

	{ type, Line, field_type, [ N, NewFieldType ] }.


% Transforms specified AST variable.
%
-spec transform_type_variable( variable_name(), line(), ast_transform() ) ->
								ast_element().
transform_type_variable( VariableName, Line, _Transforms )
  when is_atom( VariableName ) ->
	forge_type_variable( VariableName, Line ).






% Section for type forging.



% Returns an AST-compliant type description for a boolean, defined at line #0 of
% the current source file.
%
% Ex: forge_boolean_type() returns: {type,0,boolean,[]}.
%
-spec forge_boolean_type() -> ast_builtin_type().
forge_boolean_type() ->
	forge_boolean_type( _Line=0 ).


% Returns an AST-compliant type description for a boolean, defined on specified
% line of the current source file.
%
% Ex: forge_boolean_type( 45 ) returns: {type,45,boolean,[]}.
%
-spec forge_boolean_type( line() ) -> ast_builtin_type().
forge_boolean_type( Line ) ->
	forge_builtin_type( _TypeName=boolean, _TypeVars=[], Line ).




% Returns an AST-compliant type description for an atom, defined at line #0 of
% the current source file.
%
% Ex: forge_atom_type() returns: {type,0,atom,[]}.
%
-spec forge_atom_type() -> ast_builtin_type().
forge_atom_type() ->
	forge_atom_type( _Line=0 ).


% Returns an AST-compliant type description for an atom, defined on specified
% line of the current source file.
%
% Ex: forge_atom_type( 45 ) returns: {type,45,atom,[]}.
%
-spec forge_atom_type( line() ) -> ast_builtin_type().
forge_atom_type( Line ) ->
	forge_builtin_type( _TypeName=atom, _TypeVars=[], Line ).




% Returns an AST-compliant type description for a PID, defined at line #0 of the
% current source file.
%
% Ex: forge_pid_type() returns: {type,0,pid,[]}.
%
-spec forge_pid_type() -> ast_builtin_type().
forge_pid_type() ->
	forge_pid_type( _Line=0 ).


% Returns an AST-compliant type description for a PID, defined on specified line
% of the current source file.
%
% Ex: forge_pid_type( 45 ) returns: {type,45,pid,[]}.
%
-spec forge_pid_type( line() ) -> ast_builtin_type().
forge_pid_type( Line ) ->
	forge_builtin_type( _TypeName=pid, _TypeVars=[], Line ).



% Returns an AST-compliant type description for an integer, defined at line #0
% of the current source file.
%
% Ex: forge_integer_type() returns: {type,0,integer,[]}.
%
-spec forge_integer_type() -> ast_builtin_type().
forge_integer_type() ->
	forge_integer_type( _Line=0 ).


% Returns an AST-compliant type description for an integer, defined on specified
% line of the current source file.
%
% Ex: forge_integer_type( 45 ) returns: {type,45,integer,[]}.
%
-spec forge_integer_type( line() ) -> ast_builtin_type().
forge_integer_type( Line ) ->
	forge_builtin_type( _TypeName=integer, _TypeVars=[], Line ).



% Returns an AST-compliant type description for a float, defined at line #0 of
% the current source file.
%
% Ex: forge_float_type() returns: {type,0,float,[]}.
%
-spec forge_float_type() -> ast_builtin_type().
forge_float_type() ->
	forge_float_type( _Line=0 ).


% Returns an AST-compliant type description for a float, defined on specified
% line of the current source file.
%
% Ex: forge_float_type( 45 ) returns: {type,45,float,[]}.
%
-spec forge_float_type( line() ) -> ast_builtin_type().
forge_float_type( Line ) ->
	forge_builtin_type( _TypeName=float, _TypeVars=[], Line ).



% Returns an AST-compliant type description for a tuple, defined at line #0 of
% the current source file.
%
-spec forge_tuple_type( [ ast_type() ] ) -> ast_builtin_type().
forge_tuple_type( ElementTypes ) ->
	forge_tuple_type( ElementTypes, _Line=0 ).


% Returns an AST-compliant type description for a tuple, defined on specified
% line of the current source file.
%
% Ex: to represent the following type defined at line 39: { integer(), float()
% }, forge_tuple_type( 39, [ forge_integer_type(39), forge_float_type(39) ] )
% returns: {type,39,tuple,[{type,39,integer,[]},{type,39,float,[]}]}.
%
-spec forge_tuple_type( [ ast_type() ], line() ) -> ast_builtin_type().
forge_tuple_type( ElementTypes, Line ) ->
	forge_builtin_type( _TypeName=tuple, _TypeVars=ElementTypes, Line ).



% Returns an AST-compliant type description for a list, defined at line #0 of
% the current source file.
%
-spec forge_list_type( ast_type() ) -> ast_builtin_type().
forge_list_type( ElementType ) ->
	forge_list_type( ElementType, _Line=0 ).


% Returns an AST-compliant type description for a list, defined on specified
% line of the current source file.
%
% Ex: to represent the following type defined at line 39: [ integer() ],
% forge_list_type( 39, forge_integer_type(39) ) returns:
% {type,39,list,[{type,39,integer,[]}]}.
%
-spec forge_list_type( ast_type(), line() ) -> ast_builtin_type().
forge_list_type( ElementType, Line ) ->
	forge_builtin_type( _TypeName=list, _TypeVars=[ ElementType ], Line ).



% Returns an AST-compliant type description for an union, defined at line #0 of
% the current source file.
%
-spec forge_union_type( [ ast_type() ] ) -> ast_builtin_type().
forge_union_type( UnitedTypes ) ->
	forge_union_type( UnitedTypes, _Line=0 ).


% Returns an AST-compliant type description for an union, defined on specified
% line of the current source file.
%
% Ex: to represent the following type defined at line 39: integer() | float(),
% forge_union_type( [ forge_integer_type(39), forge_float_type(39) ], 39 )
% returns: {type,39,union,[{type,39,integer,[]},{type,39,float,[]}]}.
%
-spec forge_union_type( [ ast_type() ], line() ) -> ast_builtin_type().
forge_union_type( UnitedTypes, Line ) ->
	forge_builtin_type( _TypeName=union, _TypeVars=UnitedTypes, Line ).



% Returns an AST-compliant type description for the specified built-in type.
%
% Ex: forge_builtin_type( atom, [], 45 ) returns: {type,45,atom,[]}.
%
-spec forge_builtin_type( type_name(), [ ast_type() ], line() ) ->
									ast_builtin_type().
forge_builtin_type( TypeName, TypeVars, Line ) ->
	#type{ line=Line, name=TypeName, variables=TypeVars }.





% Returns an AST-compliant representation of specified local, user-defined type
% definition.
%
% Ex: to designate my_type() at line 40, forge_local_type( my_type, 40 )
% returns: {user_type,40,my_type,[]}.
%
-spec forge_local_type( type_name(), [ ast_type() ], line() ) ->
							  ast_user_type().
forge_local_type( TypeName, TypeVars, Line ) ->
	#user_type{ line=Line, name=TypeName, variables=TypeVars }.


% Returns an AST-compliant representation of specified remote type.
%
% Ex: to designate basic_utils:some_type( float() ) at line 43, use:
% forge_remote_type( basic_utils, some_type, [], 43 ) returns:
% {remote_type,43,[{atom,43,basic_utils},{atom,43,some_type},
%   [{type,43,float,[]}]]}
%
-spec forge_remote_type( module_name(), type_name(), [ ast_type() ], line() ) ->
							   ast_remote_type().
forge_remote_type( ModuleName, TypeName, TypeVars, Line ) ->
	forge_remote_type( ModuleName, TypeName, TypeVars, Line, Line, Line ).


% Returns an AST-compliant representation of specified remote type.
%
% Ex: to designate basic_utils:some_type( float() ) at lines 43, 44 and 45, use:
% forge_remote_type( basic_utils, some_type, [], { 43, 44, 45 } ) - which
% returns: {remote_type,43,[{atom,44,basic_utils},{atom,45,some_type},
% [{type,43,float,[]}]]}.
%
-spec forge_remote_type( module_name(), type_name(), [ ast_type() ],
						 line(), line(), line() ) -> ast_remote_type().
forge_remote_type( ModuleName, TypeName, TypeVars, Line1, Line2, Line3 ) ->

	Spec = [ ast_value:forge_atom_value( ModuleName, Line2 ),
			 ast_value:forge_atom_value( TypeName, Line3 ), TypeVars ],

	#remote_type{ line=Line1, spec=Spec }.



% Returns an AST-compliant representation of specified variable reference.
%
%
-spec forge_type_variable( variable_name(), line() ) ->
forge_type_variable( VariableName, Line ) when is_atom( VariableName ) ->
	{ var, Line, VariableName }.



% Checking section.


% Checks that specified type name is legit.
%
-spec check_type_name( term() ) -> basic_utils:type_name().
check_type_name( Name ) ->
	check_type_name( Name, _Context=undefined ).


% Checks that specified type name is legit.
%
-spec check_type_name( term(), form_context() ) -> basic_utils:type_name().
check_type_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_type_name( Other, Context ) ->
	ast_utils:raise_error( [ invalid_type_name, Other ], Context ).



% Checks that specified type definition is legit.
%
-spec check_type_definition( term() ) -> ast_utils:ast_type_definition().
check_type_definition( TypeDef ) ->
	check_type_definition( TypeDef, _Context=undefined ).


% Checks that specified type definition is legit.
%
-spec check_type_definition( term(), form_context() ) ->
								   ast_utils:ast_type_definition().
check_type_definition( TypeDef, _Context ) when is_tuple( TypeDef ) ->
	TypeDef;

check_type_definition( Other, Context ) ->
	ast_utils:raise_error( [ invalid_type_definition, Other ], Context ).



% Checks that specified record name is legit.
%
-spec check_record_name( term() ) -> basic_utils:record_name().
check_record_name( Name ) ->
	check_record_name( Name, _Context=undefined ).


% Checks that specified record name is legit.
%
-spec check_record_name( term(), form_context() ) -> basic_utils:record_name().
check_record_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_record_name( Other, Context ) ->
	ast_utils:raise_error( [ invalid_record_name, Other ], Context ).




% Checks that specified type identifier is legit.
%
-spec check_type_id( term() ) -> type_utils:type_id().
check_type_id( Id ) ->
	check_type_id( Id, _Context=undefined ).


% Checks that specified type identifier is legit.
%
-spec check_type_id( term(), form_context() ) -> type_utils:type_id().
check_type_id( TypeId={ TypeName, TypeArity }, Context ) ->
	check_type_name( TypeName, Context ),
	ast_utils:check_arity( TypeArity, Context ),
	TypeId;

check_type_id( Other, Context ) ->
	ast_utils:raise_error( [ invalid_type_identifier, Other ], Context ).



% Checks that specified type identifiers are legit.
%
-spec check_type_ids( term() ) -> [ type_utils:type_id() ].
check_type_ids( Ids ) ->
	check_type_ids( Ids, _Context=undefined ).


% Checks that specified type identifiers are legit.
%
-spec check_type_ids( term(), form_context() ) -> [ type_utils:type_id() ].
check_type_ids( List, Context ) when is_list( List ) ->
	[ check_type_id( Id, Context ) || Id <- List ];

check_type_ids( Other, Context ) ->
	ast_utils:raise_error( [ invalid_type_identifier_list, Other ], Context ).

% Checks that specified variable is legit.
%
-spec check_type_variable( term() ) -> ast_variable().
check_type_variable( ASTVariable ) ->
	check_type_variable( ASTVariable, _Context=undefined ).


% Checks that specified variable is legit.
%
-spec check_type_variable( term(), form_context() ) ->
								 ast_variable().
check_type_variable( ASTVariable={ 'var', Line, VariableName }, Context )
  when is_atom( VariableName ) ->
	ast_utils:check_line( Line, Context ),
	ASTVariable;

check_type_variable( Other, Context ) ->
	ast_utils:raise_error( [ invalid_ast_variable, Other ], Context ).



% Checks that specified variables are legit.
%
-spec check_type_variables( term() ) -> [ ast_variable() ].
check_type_variables( ASTVariables ) ->
	check_type_variables( ASTVariables, _Context=undefined ).


% Checks that specified variables are legit.
%
-spec check_type_variables( term(), form_context() ) -> [ ast_variable() ].
check_type_variables( List, Context ) when is_list( List ) ->
	[ check_type_variable( ASTVariable, Context ) || ASTVariable <- List ];

check_type_variables( Other, Context ) ->
	ast_utils:raise_error( [ invalid_ast_variable_list, Other ], Context ).



% Checks that specified term is the AST version of an atom.
%
-spec check_ast_atom( term() ) -> ast_base:ast_atom().
check_ast_atom( ASTAtom ) ->
	check_ast_atom( ASTAtom, _Context=undefined ).


% Checks that specified term is the AST version of an atom.
%
-spec check_ast_atom( term(), form_context() ) -> ast_base:ast_atom().
check_ast_atom( ASTAtom={ atom, _Line, Atom }, _Context )
  when is_atom( Atom ) ->
	ASTAtom;

check_ast_atom( Other, Context ) ->
	ast_utils:raise_error( [ invalid_ast_atom, Other ], Context ).




% Returns a pair made of (two) lists of located forms corresponding to:
%
% - all the type export declarations that are described in the specified type
% export table
%
% - all the types definitions that are described in the specified type table
%
-spec get_located_forms_for( ast_info:type_export_table(), type_table() ) ->
								   { [ located_form() ], [ located_form() ] }.
get_located_forms_for( TypeExportTable, TypeTable ) ->

	TypeExportInfos = ?table:enumerate( TypeExportTable ),

	%ast_utils:display_debug( "TypeExportInfos = ~p",
	%  [ TypeExportInfos ] ),

	TypeExportLocDefs = [ { Loc, { attribute, Line, export_type, TypeIds } }
				   || { Loc, { Line, TypeIds } } <- TypeExportInfos ],

	% Dropping the keys (the type_id(), i.e. type identifiers), focusing on
	% their associated type_info()
	%
	TypeInfos = ?table:values( TypeTable ),

	lists:foldl( fun( #type_info{ name=TypeName,
								  variables=TypeVariables,
								  opaque=IsOpaque,
								  location=Location,
								  line=Line,
								  definition=TypeDef
								  %exported
								}, Acc ) ->

						 TypeDesignator = case IsOpaque of

							 true ->
								 opaque;

							 false ->
								 type

						 end,

						 Form = { attribute, Line, TypeDesignator,
						   { TypeName, TypeDef, TypeVariables } },

						 LocTypeForm = { Location, Form },

						 [ LocTypeForm | Acc ]

				 end,
				 _Acc0=[],
				 _List=TypeInfos ),
	
	{ TypeExportLocDefs, TypeLocDefs }.

