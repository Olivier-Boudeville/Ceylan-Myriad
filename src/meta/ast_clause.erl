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



% Module in charge of handling clauses defined within an AST.
%
% Refer to the "7.5 Clauses" section of
% http://erlang.org/doc/apps/erts/absform.html for more information.
%
-module(ast_clause).



% There are 5 different kinds of clauses in an AST:
% - function clauses
% - if clauses
% - case clauses
% - try clauses
% - catch clauses
%
-type ast_clause() :: ast_function_clause() | ast_if_clause()
					| ast_case_clause() | ast_try_clause()
					| ast_catch_clause().


% Describes a generic (most general) clause in an AST:
%
-type ast_generic_clause() :: { 'clause', line(),
								ast_pattern:ast_pattern_sequence(),
								ast_guard:ast_guard_sequence(), ast_body() }.


% Describes a function clause in an AST:
%
% "If C is a function clause ( Ps ) -> B, where Ps is a pattern sequence and B
% is a body, then Rep(C) = {clause,LINE,Rep(Ps),[],Rep(B)}.
%
% If C is a function clause ( Ps ) when Gs -> B, where Ps is a pattern sequence,
% Gs is a guard sequence and B is a body, then Rep(C) =
% {clause,LINE,Rep(Ps),Rep(Gs),Rep(B)}."
%
-type ast_function_clause() :: ast_generic_clause().



% Describes an if clause in an AST:
%
% "If C is an if clause Gs -> B, where Gs is a guard sequence and B is a body,
% then Rep(C) = {clause,LINE,[],Rep(Gs),Rep(B)}."
%
% (special case of ast_generic_clause/0, no pattern sequence)
%
-type ast_if_clause() :: { 'clause', line(), [], ast_guard:ast_guard_sequence(),
						   ast_body() }.



% Describes a case clause in an AST:
%
-type ast_case_clause() :: ast_generic_clause().


% Describes a try clause in an AST:
%
-type ast_try_clause() :: ast_generic_clause().


% Describes a catch clause in an AST:
%
-type ast_catch_clause() :: ast_generic_clause().


% The description of a body (ex: of a function clause) in an AST.
%
% "A body B is a non-empty sequence of expressions E_1, ..., E_k, and Rep(B) =
% [Rep(E_1), ..., Rep(E_k)]."
%
-type ast_body() :: nonempty_list( ast_expression() ).


-export_type([ ast_clause/0, ast_function_clause/0, ast_if_clause/0,
			   ast_case_clause/0, ast_try_clause/0, ast_catch_clause/0,
			   ast_body/0 ]).



% Forging:
-export([ forge_local_call/3, forge_local_call/4,
		  forge_remote_call/4, forge_remote_call/5 ]).


% Checking:
-export([ check_function_clauses/2, check_function_clauses/3 ]).




-export([ transform_function_clauses/2, transform_function_clause/2,

		  transform_try_clauses/2, transform_try_clause/2,
		  transform_catch_clauses/2, transform_catch_clause/2,

		  transform_if_clauses/2, transform_if_clause/2,
		  transform_case_clauses/2, transform_case_clause/2,

		  transform_body/2 ]).



% Shorthands:

-type module_name() :: meta_utils:module_name().
-type function_name() :: meta_utils:function_name().
-type function_arity() :: meta_utils:function_arity().
-type ast_expression() :: ast_expression:ast_expression().
-type line() :: ast_base:line().
-type form_context() :: ast_base:form_context().



% Transformations section.


% Apparently, for all (4) kinds of clauses, according to erl_id_trans the same
% structure applies: {clause,Line,H,G,B}, where:
%
% - H is Head, a list of patterns (a pattern sequence)
% - G is Guard, a list of guard tests (a guard sequence)
% - B is Body, a list of expressions
%
% However, depending of the actual kind, more specific rules apply (ex: a list
% having a single element), which are enforced here.



% Generic clause section.
%
% In quite a few occasions, clauses can be managed generically, regardless of
% whether they belong to a 'if', a 'catch', etc. (see icr_clauses/1 in
% erl_id_trans).
%
% Here is the corresponding generic clause transformation.
%
% (helper)
%
-spec transform_clauses_generic( [ ast_clause() ],
		 ast_transform:ast_transforms() ) -> [ ast_clause() ].
transform_clauses_generic( Clauses, Transforms ) ->
	[ transform_clause_generic( C, Transforms ) || C <- Clauses ].



-spec transform_clause_generic( ast_clause(),
		 ast_transform:ast_transforms() ) -> ast_clause().
transform_clause_generic(
  _Clause={ 'clause', Line, HeadPatternSequence, GuardSequence, BodyExprs },
  Transforms ) ->

	%ast_utils:display_debug( "Intercepting generic clause ~p...", [ Clause ] ),

	NewHeadPatternSequence = ast_pattern:transform_pattern_sequence(
										 HeadPatternSequence, Transforms ),

	% Possibly empty guard list:
	NewGuardSequence = ast_guard:transform_guard_sequence( GuardSequence,
														   Transforms ),

	NewBodyExprs = transform_body( BodyExprs, Transforms ),

	Res = { 'clause', Line, NewHeadPatternSequence, NewGuardSequence,
			NewBodyExprs },

	%ast_utils:display_debug( "... returning generic clause ~p", [ Res ] ),

	Res.



% Function clause section.


% Transforms specified list of function clauses.
%
-spec transform_function_clauses( [ ast_function_clause() ],
		 ast_transform:ast_transforms() ) -> [ ast_function_clause() ].
transform_function_clauses( FunctionClauses, Transforms ) ->
	transform_clauses_generic( FunctionClauses, Transforms ).



% Transforms specified function clause.
%
% Handled the same, with or without guard(s), as a guard sequence may be empty:
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
transform_function_clause( Clause, Transforms ) ->
	transform_clause_generic( Clause, Transforms ).



% Try clause section.


% Transforms specified list of try clauses.
%
-spec transform_try_clauses( [ ast_try_clause() ],
		 ast_transform:ast_transforms() ) -> [ ast_try_clause() ].
transform_try_clauses( TryClauses, Transforms ) ->
	transform_clauses_generic( TryClauses, Transforms ).


% Transforms specified try clause.
%
-spec transform_try_clause( ast_try_clause(),
		 ast_transform:ast_transforms() ) -> ast_try_clause().
transform_try_clause( TryClause, Transforms ) ->
	transform_clause_generic( TryClause, Transforms ).





% Catch clause section.
%
% (with both possibilities, having an empty guard sequence is just a special
% case of a more general rule)


% Transforms specified list of 'catch' clauses.
%
-spec transform_catch_clauses( [ ast_catch_clause() ],
		 ast_transform:ast_transforms() ) -> [ ast_catch_clause() ].
transform_catch_clauses( CatchClauses, Transforms ) ->
	[ transform_catch_clause( CC, Transforms ) || CC <- CatchClauses ].



% Catch clause with no variable, with or without a guard sequence (1/4 and 3/4):
%
% - "If C is a catch clause P -> B, where P is a pattern and B is a body, then
% Rep(C) = {clause,LINE,[Rep({throw,P,_})],[],Rep(B)}."
%
% - "If C is a catch clause P when Gs -> B, where P is a pattern, Gs is a guard
% sequence, and B is a body, then Rep(C) =
% {clause,LINE,[Rep({throw,P,_})],Rep(Gs),Rep(B)}."
%
-spec transform_catch_clause( ast_catch_clause(),
				  ast_transform:ast_transforms() ) -> ast_catch_clause().
transform_catch_clause(
  _Clause={ 'clause', Line, [ { throw, Pattern, Any } ], GuardSequence,
		   BodyExprs },
  Transforms ) ->

	ast_utils:display_warning( "transform_catch_clause: Any= ~p", [ Any ] ),

	%ast_utils:display_debug( "Intercepting catch clause ~p...", [ Clause ] ),

	NewPattern = ast_pattern:transform_pattern( Pattern, Transforms ),

	NewGuardSequence = ast_guard:transform_guard_sequence( GuardSequence,
														   Transforms ),

	NewBodyExprs = transform_body( BodyExprs, Transforms ),

	Res = { 'clause', Line, [ { 'throw', NewPattern, Any } ], NewGuardSequence,
			NewBodyExprs },

	%ast_utils:display_debug( "... returning catch clause ~p", [ Res ] ),

	Res;


% Catch clause with X variable, with or without a guard sequence (2/4 and 4/4):
%
% - "If C is a catch clause X : P -> B, where X is an atomic literal or a
% variable pattern, P is a pattern, and B is a body, then Rep(C) =
% {clause,LINE,[Rep({X,P,_})],[],Rep(B)}."
%
% - "If C is a catch clause X : P when Gs -> B, where X is an atomic literal or
% a variable pattern, P is a pattern, Gs is a guard sequence, and B is a body,
% then Rep(C) = {clause,LINE,[Rep({X,P,_})],Rep(Gs),Rep(B)}."
%
transform_catch_clause(
  _Clause={ 'clause', Line, [ HeadPattern={ _X, _P, _Any } ], GuardSequence,
			BodyExprs },
  Transforms ) ->

	%ast_utils:display_debug( "transform_catch_clause: X=~p, P=~p, Any= ~p",
	%						  [ X, P, Any ] ),

	%ast_utils:display_debug( "Intercepting catch clause with variable ~p...",
	%						 [ Clause ] ),

	% Includes atomic literals:
	NewHeadPattern = ast_pattern:transform_pattern( HeadPattern, Transforms ),

	NewGuardSequence = ast_guard:transform_guard_sequence( GuardSequence,
														   Transforms ),

	NewBodyExprs = transform_body( BodyExprs, Transforms ),

	Res = { 'clause', Line, [ NewHeadPattern ], NewGuardSequence,
			NewBodyExprs },

	%ast_utils:display_debug( "... returning catch clause with variable ~p",
	%						 [ Res ] ),

	Res.





% If clause section.


% Transforms specified list of 'if' clauses.
%
-spec transform_if_clauses( [ ast_if_clause() ],
		 ast_transform:ast_transforms() ) -> [ ast_if_clause() ].
transform_if_clauses( IfClauses, Transforms ) ->
	[ transform_if_clause( IC, Transforms ) || IC <- IfClauses ].



% Transforms specified 'if' clause.
%
% "If C is an if clause Gs -> B, where Gs is a guard sequence and B is a body,
% then Rep(C) = {clause,LINE,[],Rep(Gs),Rep(B)}."
%
% (no pattern sequence allowed)
%
-spec transform_if_clause( ast_if_clause(), ast_transform:ast_transforms() ) ->
								 ast_if_clause().
transform_if_clause(
  _Clause={ 'clause', Line, HeadPatternSequence=[], GuardSequence, BodyExprs },
  Transforms ) ->

	%ast_utils:display_debug( "Intercepting if clause ~p...", [ Clause ] ),

	NewGuardSequence = ast_guard:transform_guard_sequence( GuardSequence,
														   Transforms ),

	NewBodyExprs = transform_body( BodyExprs, Transforms ),

	Res = { 'clause', Line, HeadPatternSequence, NewGuardSequence,
			NewBodyExprs },

	%ast_utils:display_debug( "... returning if clause ~p", [ Res ] ),

	Res.







% Case clause section.


% Transforms specified list of 'case' clauses.
%
-spec transform_case_clauses( [ ast_case_clause() ],
		 ast_transform:ast_transforms() ) -> [ ast_case_clause() ].
transform_case_clauses( CaseClauses, Transforms ) ->
	[ transform_case_clause( CC, Transforms ) || CC <- CaseClauses ].



% Transforms specified 'case' clause.
%
% "If C is a case clause P -> B, where P is a pattern and B is a body, then
% Rep(C) = {clause,LINE,[Rep(P)],[],Rep(B)}.
%
% If C is a case clause P when Gs -> B, where P is a pattern, Gs is a guard
% sequence, and B is a body, then Rep(C) =
% {clause,LINE,[Rep(P)],Rep(Gs),Rep(B)}."
%
% (a single pattern allowed)
%
-spec transform_case_clause( ast_case_clause(),
				ast_transform:ast_transforms() ) -> ast_case_clause().
transform_case_clause(
  _Clause={ 'clause', Line, [ Pattern ], GuardSequence, BodyExprs },
  Transforms ) ->

	%ast_utils:display_debug( "Intercepting case clause ~p...", [ Clause ] ),

	NewPattern = ast_pattern:transform_pattern( Pattern, Transforms ),

	NewGuardSequence = ast_guard:transform_guard_sequence( GuardSequence,
														   Transforms ),

	NewBodyExprs = transform_body( BodyExprs, Transforms ),

	Res = { 'clause', Line, [ NewPattern ], NewGuardSequence, NewBodyExprs },

	%ast_utils:display_debug( "... returning case clause ~p", [ Res ] ),

	Res.



% Transforms the specified AST body.
%
% "A body B is a non-empty sequence of expressions E_1, ..., E_k, and Rep(B) =
% [Rep(E_1), ..., Rep(E_k)]."
%
-spec transform_body( ast_body(), ast_transform:ast_transforms() ) ->
							ast_body().

% Actually bodies can be empty lists (ex: if a try/catch does not have an
% 'after' clause, its associated body will be empty).
%
%transform_body( _BodyExprs=[], _Transforms ) ->
%	ast_utils:raise_error( invalid_empty_body );

transform_body( BodyExprs, Transforms ) when is_list( BodyExprs ) ->
	ast_expression:transform_expressions( BodyExprs, Transforms );

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
	{ 'call', Line1, ast_value:forge_atom_value( FunctionName, Line2 ),
	  Params }.




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
	{ 'call', Line1, { remote, Line2,
					   ast_value:forge_atom_value( ModuleName, Line2 ),
					   ast_value:forge_atom_value( FunctionName, Line2 ) },
	  Params }.



% Checking section.


% Checks that specified function clauses are legit.
%
-spec check_function_clauses( term(), function_arity() ) ->
									[ ast_function_clause() ].
check_function_clauses( Clauses, FunctionArity ) ->
	check_function_clauses( Clauses, FunctionArity, _Context=undefined ).


% Checks that specified function clauses are legit.
%
-spec check_function_clauses( term(), function_arity(), form_context() ) ->
									[ ast_function_clause() ].
check_function_clauses( Clauses, FunctionArity, Context )
  when is_list( Clauses ) ->
	ast_utils:check_arity( FunctionArity, Context ),
	Clauses;

check_function_clauses( Other, _FunctionArity, Context ) ->
	ast_utils:raise_error( [ invalid_function_clauses, Other ], Context ).
