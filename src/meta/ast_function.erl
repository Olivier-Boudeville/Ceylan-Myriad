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
% Creation date: Wednesday, February 7, 2018.




% Module in charge of providing constructs to manage functions in an AST.
%
-module(ast_function).



-export_type([]).



% Checking:
-export([ check_function_name/1, check_function_name/2,

		  check_function_id/1, check_function_id/2,
		  check_function_ids/1, check_function_ids/2,

		  check_function_type/2, check_function_type/3,
		  check_function_types/2, check_function_types/3 ]).


% Shorthands:

-type function_arity() :: meta_utils:function_arity().
%-type line() :: ast_base:line().
-type form_context() :: ast_base:form_context().




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


