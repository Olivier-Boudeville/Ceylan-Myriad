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



% Module in charge of handling expressions defined with an AST.
%
% See http://erlang.org/doc/apps/erts/absform.html for more information.
%
-module(ast_expression).


% The description of an expression in an AST, with line information.
%
% Ex: '{integer,97,2}' or '{match,117, {var,117,'A'}, {atom,117,foobar}}' , etc.
%
% Note: an expression is different from a pattern: even if they share at least
% some types of forms, they are to be interpreted differently (ex: their
% sub-elements are of the same kind as they are, and at least some rules
% differ).
%
-type ast_expression() :: ast_base:ast_element().


% An expression that can be evaluated to an integer:
%
-type ast_integer_expression() :: ast_expression:ast_expression().


-export_type([ ast_expression/0, ast_integer_expression/0 ]).


-export([ transform_expression/2, transform_expressions/2 ]).



% For the table macro:
-include("meta_utils.hrl").

% For the ast_transforms record:
-include("ast_transform.hrl").


% Shorthands:

%-type line() :: ast_base:line().
-type ast_transforms() :: ast_transform:ast_transforms().



% Transforms specified expression.
%
% See section "7.4 Expressions" in http://erlang.org/doc/apps/erts/absform.html.
%
-spec transform_expression( ast_expression(), ast_transforms() ) ->
								  ast_expression().



% Remote call expression found:
%
% "If E is a function call E_m:E_0(E_1, ..., E_k), then Rep(E) =
% {call,LINE,{remote,LINE,Rep(E_m),Rep(E_0)},[Rep(E_1), ..., Rep(E_k)]}.

% Remote call expression found, with an immediate name for both the module and
% the function:
%
transform_expression( E={ 'call', Line1, { remote, _Line2,
			_M={ atom, _Line3, ModuleName }, _F={ atom, Line4, FunctionName } },
			Params }, Transforms ) ->

	ast_utils:display_debug( "Intercepting remote call expression ~p...",
							 [ E ] ),

	Arity = length( Params ),

	% First recurses, knowing that function parameters are expressions:
	NewParams = transform_expressions( Params, Transforms ),

	Outcome = case Transforms#ast_transforms.remote_calls of

		undefined ->
			unchanged;

		RemoteReplaceTable ->

			case ?table:lookupEntry( { ModuleName, FunctionName, Arity },
									 RemoteReplaceTable ) of

				{ value, E={ _NewModuleName, _NewFunctionName } } ->
					E;

				{ value, TransformFun } when is_function( TransformFun ) ->
					TransformFun( FunctionName, Arity );

				key_not_found ->

					% Maybe a wildcard arity was defined then?
					case ?table:lookupEntry(
							{ ModuleName, FunctionName, _AnyArity='_' },
							RemoteReplaceTable ) of

						{ value, E={ _NewModuleName, _NewFunctionName } } ->
							E;

						% Same function name, only module overridden:
						% (never happens)
						%{ value, NewModuleName }
						%       when is_atom( NewModuleName ) ->
						%	{ NewModuleName, FunName };

						{ value, TransformFun }
						  when is_function( TransformFun ) ->
							TransformFun( FunctionName, Arity );

						key_not_found ->
							% Maybe a wildcard function name was defined then?

							% (note: the case of a wildcard function name and a
							% set, actual arity is not deemed relevant)

							case ?table:lookupEntry( { ModuleName,
									   _AnyFunctionName='_', _AnyArity='_' },
													 RemoteReplaceTable ) of

								{ value,
								  { NewModuleName, _NewFunctionName='_' } } ->
									{ NewModuleName, FunctionName } ;

								{ value,
								  E={ _NewModuleName, _NewFunctionName } } ->
									E;

									% Same function name, only module
									% overridden: (never happens)
									%
									%{ value, NewModuleName }
									%       when is_atom( NewModuleName ) ->
									%    { NewModuleName, FunName };

								{ value, TransformFun }
								  when is_function( TransformFun ) ->
									TransformFun( FunctionName, Arity );

								key_not_found ->
									unchanged

							end

					end

			end

	end,

	case Outcome of

		unchanged ->
			% Original expression, yet with updated parameters:
			%Res = { call, Line1, { remote, Line2, M, F }, NewParams },
			% Used for uniformity:
			Res = ast_clause:forge_remote_call( ModuleName, FunctionName,
												NewParams, Line1, Line4 ),
			ast_utils:display_debug( "... returning remote call expression "
									 "(case R1) ~p", [ Res ] ),
			Res;

		{ SetModuleName, SetFunctionName } ->
			Res = ast_clause:forge_remote_call( SetModuleName, SetFunctionName,
												NewParams, Line1, Line4 ),
			ast_utils:display_debug( "... returning remote call expression "
									 "(case R2) ~p", [ Res ] ),
			Res

	end;



% Here, at least one name (module and/or function) is not immediate in that
% remote call expression:
%
% (note: we do not manage yet the case where for example the function name
% results from an expression yet a wildcard has been defined for it)
%
transform_expression( E={ 'call', Line1,
						  { remote, Line2, ModuleExpr, FunctionExpr }, Params },
					 Transforms ) ->

	ast_utils:display_debug( "Intercepting non-immediate remote call "
							 "expression ~p...", [ E ] ),

	NewModuleExpr = transform_expression( ModuleExpr, Transforms ),

	NewFunctionExpr = transform_expression( FunctionExpr, Transforms ),

	NewParams = transform_expressions( Params, Transforms ),

	% Cannot use ast_clause:forge_remote_call, we have not atoms:
	%
	Res = { call, Line1, { remote, Line2, NewModuleExpr, NewFunctionExpr },
			NewParams },

	ast_utils:display_debug( "... returning non-immediate remote call "
							 "expression (case R3) ~p", [ Res ] ),

	Res;



% Local call expression found:
%
% "If E is a function call E_0(E_1, ..., E_k), then Rep(E) =
% {call,LINE,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}."
%
transform_expression( E={ 'call', Line1, F={ atom, Line2, FunName }, Params },
					  Transforms ) ->

	ast_utils:display_debug( "Intercepting local call expression ~p...",
							 [ E ] ),

	Arity = length( Params ),

	% First recurses:
	NewParams = transform_expressions( Params, Transforms ),

	Outcome = case Transforms#ast_transforms.local_calls of

		undefined ->
			unchanged;

		LocalReplaceTable ->

			case ?table:lookupEntry( { FunName, Arity }, LocalReplaceTable ) of

				{ value, E={ _NewModuleName, _NewFunName } } ->
					E;

				{ value, TransformFun } when is_function( TransformFun ) ->
					TransformFun( FunName, Arity );

				key_not_found ->

					% Maybe a wildcard arity was defined then?
					case ?table:lookupEntry( { FunName, _AnyArity='_' },
											 LocalReplaceTable ) of

						{ value, E={ _NewModuleName, _NewFunName } } ->
							E;

						% Same function name, only module overridden: (never
						% happens)
						%{ value, NewModuleName }
						%       when is_atom( NewModuleName ) ->
						%	{ NewModuleName, FunName };

						{ value, TransformFun }
						  when is_function( TransformFun ) ->
							TransformFun( FunName, Arity );

						key_not_found ->
							% Nope, let it as it is:
							unchanged

					end

			end

	end,

	case Outcome of

		unchanged ->
			% Original expression yet with updated parameters:
			Res={ call, Line1, F, NewParams },
			ast_utils:display_debug( "... returning local call expression ~p",
									 [ Res ] ),
			Res;

		{ SetModuleName, SetFunctionName } ->
			Res = ast_clause:forge_remote_call( SetModuleName, SetFunctionName,
											   NewParams, Line1, Line2 ),
			ast_utils:display_debug( "... returning remote call expression ~p",
									 [ Res ] ),
			Res

	end;


% Case expression found:
%
% "If E is a case expression case E_0 of Cc_1 ; ... ; Cc_k end, where E_0 is an
% expression and each Cc_i is a case clause, then Rep(E) =
% {'case',LINE,Rep(E_0),[Rep(Cc_1), ..., Rep(Cc_k)]}."
%
transform_expression( E={ 'case', Line, TestExpression, CaseClauses },
					  Transforms ) ->

	ast_utils:display_debug( "Intercepting case expression ~p...", [ E ] ),

	NewTestExpression = transform_expression( TestExpression, Transforms ),

	NewCaseClauses = ast_clause:transform_case_clauses( CaseClauses,
														Transforms ),

	Res = { 'case', Line, NewTestExpression, NewCaseClauses },

	ast_utils:display_debug( "... returning case expression ~p", [ Res ] ),
	Res;



% Catch expression found:
%
% "If E is a catch expression catch E_0, then Rep(E) = {'catch',LINE,Rep(E_0)}."
%
transform_expression( E={ 'catch', Line, Expression }, Transforms ) ->

	ast_utils:display_debug( "Intercepting catch expression ~p...", [ E ] ),

	NewExpression = transform_expression( Expression, Transforms ),

	Res = { 'case', Line, NewExpression  },

	ast_utils:display_debug( "... returning catch expression ~p", [ Res ] ),
	Res;


% Cons expression found:
%
% "If E is a cons skeleton [E_h | E_t], then Rep(E) =
% {cons,LINE,Rep(E_h),Rep(E_t)}."
%
% Head and Tail members are expressions (not just patterns), as a member can
% for example be : {call,56, {remote, ...
%
transform_expression( E={ 'cons', Line, HeadExpression, TailExpression },
					  Transforms ) ->

	ast_utils:display_debug( "Intercepting cons expression ~p...", [ E ] ),

	NewHeadExpression = transform_expression( HeadExpression, Transforms ),

	NewTailExpression = transform_expression( TailExpression, Transforms ),

	Res = { cons, Line, NewHeadExpression, NewTailExpression },

	ast_utils:display_debug( "... returning cons expression ~p", [ Res ] ),

	Res;


% Fun expression found:
%
% "If E is a fun expression fun Name/Arity, then Rep(E) =
% {'fun',LINE,{function,Name,Arity}}."
%
transform_expression( E={ 'fun', Line, { function, Name, Arity } },
					  Transforms ) ->

	ast_utils:display_debug( "Intercepting local fun expression ~p...", [ E ] ),

	NewName = transform_expression( Name, Transforms ),

	NewArity = transform_expression( Arity, Transforms ),

	%Res = E,
	Res = { 'fun', Line, { function, NewName, NewArity } },

	ast_utils:display_debug( "... returning local fun expression ~p", [ Res ] ),

	Res;


% "If E is a fun expression fun Module:Name/Arity, then Rep(E) =
% {'fun',LINE,{function,Rep(Module),Rep(Name),Rep(Arity)}}."
%
transform_expression( E={ 'fun', Line, _F={ function, Module, Name, Arity } },
					  Transforms ) ->

	ast_utils:display_debug( "Intercepting remote fun expression ~p...",
							 [ E ] ),

	NewModule = transform_expression( Module, Transforms ),

	NewName = transform_expression( Name, Transforms ),

	NewArity = transform_expression( Arity, Transforms ),

	Res = { 'fun', Line, { function, NewModule, NewName, NewArity } },

	ast_utils:display_debug( "... returning remote fun expression ~p",
							 [ Res ] ),

	Res;


% "If E is a fun expression fun Fc_1 ; ... ; Fc_k end, where each Fc_i is a
% function clause, then Rep(E) = {'fun',LINE,{clauses,[Rep(Fc_1), ...,
% Rep(Fc_k)]}}."
%
transform_expression( E={ 'fun', Line, { clauses, FunctionClauses } },
					  Transforms ) ->

	ast_utils:display_debug( "Intercepting fun expression with clauses ~p...",
							 [ E ] ),

	NewFunctionClauses = ast_clause:transform_function_clauses(
						   FunctionClauses, Transforms ),

	Res = { 'fun', Line, { clauses, NewFunctionClauses } },

	ast_utils:display_debug( "... returning fun expression with clauses ~p",
							 [ Res ] ),

	Res;


% "If E is a fun expression fun Name Fc_1 ; ... ; Name Fc_k end, where Name is a
% variable and each Fc_i is a function clause, then Rep(E) =
% {named_fun,LINE,Name,[Rep(Fc_1), ..., Rep(Fc_k)]}."
%
transform_expression( E={ 'named_fun', Line, Name, FunctionClauses  },
					  Transforms ) ->

	ast_utils:display_debug( "Intercepting named fun expression ~p...",
							 [ E ] ),

	NewFunctionClauses = ast_clause:transform_function_clauses(
						   FunctionClauses, Transforms ),

	Res = { 'named_fun', Line, Name, NewFunctionClauses },

	ast_utils:display_debug( "... returning named fun expression ~p",
							 [ Res ] ),

	Res;


% "If E is an atomic literal L, then Rep(E) = Rep(L)."
%
transform_expression( E={ AtomicLiteralType, _Line, _Value },
					  Transforms ) when AtomicLiteralType =:= 'atom' orelse
										AtomicLiteralType =:= 'char' orelse
										AtomicLiteralType =:= 'float' orelse
										AtomicLiteralType =:= 'integer' orelse
										AtomicLiteralType =:= 'string' ->

	ast_value:transform_value( E, Transforms );


% Default catch-all:
transform_expression( Expression, _Transforms ) ->

	% Was incorrect, as patterns are not a special case of expressions:

	% None of the expressions above matched, this expression must be a pattern
	% then:
	%
	%ast_pattern:transform_pattern( Expression, Transforms ).

	ast_utils:raise_error( [ unexpected_expression, Expression ] ).



% For convenience:
%
-spec transform_expressions( [ ast_expression() ], ast_transforms() ) ->
								  [ ast_expression() ].
transform_expressions( Expressions, Transforms ) ->
	[ transform_expression( E, Transforms ) || E <- Expressions ].
