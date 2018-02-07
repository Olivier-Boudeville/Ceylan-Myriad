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



% Module in charge of handling clauses defined within an AST.
%
% Refer to the "7.5 Clauses" section of
% http://erlang.org/doc/apps/erts/absform.html for more information.
%
-module(ast_clause).



% There are 4 different kinds of clauses:
% - function clauses
% - if clauses
% - case clauses
% - catch clauses



% There are 4 different kinds of clauses in an AST:
% - function clauses
% - if clauses
% - case clauses
% - catch clauses
%
-type ast_clause() :: ast_function_clause() | ast_if_clause()
					| ast_case_clause() | ast_catch_clause().


% Describes a function clause in an AST:
%
-type ast_function_clause() :: { 'clause', line(),
								 ast_pattern:ast_pattern_sequence(),
								 ast_guard:ast_guard_sequence(), ast_body() }.


% Describes an if clause in an AST:
%
-type ast_if_clause() :: basic_utils:fixme().


% Describes a case clause in an AST:
%
-type ast_case_clause() :: basic_utils:fixme().


% Describes a catch clause in an AST:
%
-type ast_catch_clause() :: basic_utils:fixme().


% The description of a body (ex: of a function clause) in an AST.
%
% "A body B is a non-empty sequence of expressions E_1, ..., E_k, and Rep(B) =
% [Rep(E_1), ..., Rep(E_k)]."
%
-type ast_body() :: nonempty_list( ast_expression() ).


-export_type([ ast_clause/0, ast_function_clause/0, ast_if_clause/0,
			   ast_case_clause/0, ast_catch_clause/0, ast_body/0 ]).



% Forging:
-export([ forge_local_call/3, forge_local_call/4,
		  forge_remote_call/4, forge_remote_call/5 ]).


% Checking:
-export([ check_function_clauses/2, check_function_clauses/3 ]).




-export([ transform_function_clause/2, transform_if_clause/2,
		  transform_case_clause/2, transform_catch_clause/2,
		  transform_body/2 ]).



% Shorthands:

-type module_name() :: meta_utils:module_name().
-type function_name() :: meta_utils:function_name().
-type function_arity() :: meta_utils:function_arity().
-type ast_expression() :: ast_expression:ast_expression().
-type line() :: ast_base:line().
-type form_context() :: ast_base:form_context().



% Transformations section.



% Function clause section.


% Transforms specified function clause.
%
% Handled the same, with or without guard(s):
%
%  - without: "If C is a function clause ( Ps ) -> B, where Ps is a pattern
%  sequence and B is a body, then Rep(C) = {clause,LINE,Rep(Ps),[],Rep(B)}."
%
%  - with: "If C is a function clause ( Ps ) when Gs -> B, where Ps is a pattern
%  sequence, Gs is a guard sequence and B is a body, then Rep(C) =
%  {clause,LINE,Rep(Ps),Rep(Gs),Rep(B)}."
%
-spec transform_function_clause( ast_function_clause(),
		 ast_transform:ast_transforms() ) -> ast_function_clause().
transform_function_clause(
  Clause={ clause, Line, PatternSequence, GuardSequence, Body },
  Transforms ) ->

	ast_utils:display_debug( "Intercepting function clause ~p...", [ Clause ] ),

	% Rather complete, out of safety:

	NewPatternSequence = ast_pattern:transform_pattern_sequence(
						   PatternSequence, Transforms ),

	% Guard example: {call,102, {atom,102,is_integer}, [{var,102,'X'}]}
	NewGuardSequence = ast_guard:transform_guard_sequence( GuardSequence,
														   Transforms ),

	NewBody = transform_body( Body, Transforms ),

	Res = { clause, Line, NewPatternSequence, NewGuardSequence, NewBody },

	ast_utils:display_debug( "... returning function clause ~p", [ Res ] ),

	Res.



% If clause section.


-spec transform_if_clause( ast_if_clause(), ast_transform:ast_transforms() ) ->
								 ast_if_clause().
transform_if_clause(
  _Clause,
  _Transforms ) ->
	throw( fixme ).



% Case clause section.

-spec transform_case_clause( ast_case_clause(),
				ast_transform:ast_transforms() ) -> ast_case_clause().
transform_case_clause(
  _Clause,
  _Transforms ) ->
	throw( fixme ).


% Catch clause section.


-spec transform_catch_clause( ast_catch_clause(),
				  ast_transform:ast_transforms() ) -> ast_catch_clause().
transform_catch_clause(
  _Clause,
  _Transforms ) ->
	throw( fixme ).



% Transforms the specified AST body.
%
% "A body B is a non-empty sequence of expressions E_1, ..., E_k, and Rep(B) =
% [Rep(E_1), ..., Rep(E_k)]."
%
-spec transform_body( ast_body(), ast_transform:ast_transforms() ) ->
							ast_body().
transform_body( _Body=[], _Transforms ) ->
	ast_utils:raise_error( invalid_empty_body );

transform_body( Body, Transforms ) when is_list( Body ) ->
	[ ast_expression:transform_expression( E, Transforms ) || E <- Body ];

transform_body( Other, _Transforms ) ->
	ast_utils:raise_error( [ invalid_body, Other ] ).




% Forging section.



% Returns an AST-compliant representation of specified local call.
%
% Ex: to designate 'some_fun( a, b )' at line 102, use;
% forge_local_call( some_fun, ParamDefs, 102 ) - which returns:
% {call,102,{atom,102,some_fun},[{atom,102,a},{atom,102,b}]}.
%
-spec forge_local_call( function_name(), [ ast_expression() ],
						line() ) -> ast_expression().
forge_local_call( FunctionName, Params, Line ) ->
	forge_local_call( FunctionName, Params, Line, Line ).


% Returns an AST-compliant representation of specified local call.
%
% Ex: to designate 'some_fun( a, b )' at line 102, use;
% forge_local_call( some_fun, ParamDefs, 102 ) - which returns:
% {call,102,{atom,102,some_fun},[{atom,102,a},{atom,102,b}]}.
%
-spec forge_local_call( function_name(), [ ast_expression() ], line(),
						line() ) -> ast_expression().
forge_local_call( FunctionName, Params, Line1, Line2 ) ->
	{ call, Line1, ast_type:forge_atom_value( FunctionName, Line2 ), Params }.




% Returns an AST-compliant representation of specified remote call.
%
% Ex: to designate 'some_module:some_fun( a, b )' at line 102, use;
% forge_remote_call( some_module, some_fun, ParamDefs, 102 ) - which returns:
% {{remote,102, {atom,102,some_module}, {atom,102,some_fun},
%              [{atom,102,a},{atom,102,b}]}.
%
-spec forge_remote_call( module_name(), function_name(), [ ast_expression() ], 
						 line() ) -> ast_expression().
forge_remote_call( ModuleName, FunctionName, Params, Line ) ->
	forge_remote_call( ModuleName, FunctionName, Params, Line, Line ).



% Returns an AST-compliant representation of specified (immediate, in terms of
% name of module and function) remote call.
%
% Ex: to designate 'some_module:some_fun( a, b )' at lines 101 and 102, use;
% forge_remote_call( some_module, some_fun, ParamDefs, 102 ) - which returns:
% {{remote,102, {atom,102,some_module}, {atom,102,some_fun},
%              [{atom,102,a},{atom,102,b}]}.
%
-spec forge_remote_call( module_name(), function_name(), [ ast_expression() ],
						 line(), line() ) -> ast_expression().
forge_remote_call( ModuleName, FunctionName, Params, Line1, Line2 ) ->
	{ call, Line1, { remote, Line2,
					 ast_type:forge_atom_value( ModuleName, Line2 ),
					 ast_type:forge_atom_value( FunctionName, Line2 ) },
	  Params }.



% Checking section.


% Checks that specified function clauses are legit.
%
-spec check_function_clauses( term(), function_arity() ) ->
									[ meta_utils:function_clause() ].
check_function_clauses( Clauses, FunctionArity ) ->
	check_function_clauses( Clauses, FunctionArity, _Context=undefined ).


% Checks that specified function clauses are legit.
%
-spec check_function_clauses( term(), function_arity(), form_context() ) ->
									[ meta_utils:function_clause() ].
check_function_clauses( Clauses, FunctionArity, Context )
  when is_list( Clauses ) ->
	ast_utils:check_arity( FunctionArity, Context ),
	Clauses;

check_function_clauses( Other, _FunctionArity, Context ) ->
	ast_utils:raise_error( [ invalid_function_clauses, Other ], Context ).
