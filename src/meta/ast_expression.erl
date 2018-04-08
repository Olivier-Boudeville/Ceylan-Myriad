% Copyright (C) 2018-2018 Olivier Boudeville (olivier.boudeville@esperide.com)
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

-type line() :: ast_base:line().
-type ast_transforms() :: ast_transform:ast_transforms().




% List-comprehension generator.
%
-type lc_generator_qualifier() :: { 'generate', line(),
						ast_pattern:ast_pattern(), ast_expression() }.


% Bitstring generator.
%
-type bitstring_generator_qualifier() :: { 'b_generate', line(),
						ast_pattern:ast_pattern(), ast_expression() }.



% A qualifier is one of the following: an expression-based filter, a
% list-comprehension generator or a bitstring generator.
%
-type ast_qualifier() :: ast_expression() | lc_generator_qualifier()
						 | bitstring_generator_qualifier().



% Transforms specified expression.
%
% See section "7.4 Expressions" in http://erlang.org/doc/apps/erts/absform.html.
%
-spec transform_expression( ast_expression(), ast_transforms() ) ->
								  ast_expression().


% Function call found:
%
% Once it is transformed, expected to fall within:
%
% "If E is a function call E_0(E_1, ..., E_k), then Rep(E) =
% {call,LINE,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}."
%
% or
%
% "If E is a function call E_m:E_0(E_1, ..., E_k), then Rep(E) =
% {call,LINE,{remote,LINE,Rep(E_m),Rep(E_0)},[Rep(E_1), ..., Rep(E_k)]}.
%
transform_expression( _E={ 'call', LineCall, FunctionRef, Params },
					  Transforms ) ->

	%ast_utils:display_debug( "Intercepting call expression ~p...", [ E ] ),

	TransformedFunctionRef = transform_expression( FunctionRef, Transforms ),

	% First recurses, knowing that function parameters are expressions:
	NewParams = transform_expressions( Params, Transforms ),

	NewArity = length( NewParams ),

	FinalFunctionRef = transform_call_expression( TransformedFunctionRef,
												  NewArity, Transforms ),

	Res = { 'call', LineCall, FinalFunctionRef, NewParams },

	%ast_utils:display_debug( "... returning call expression ~p", [ Res ] ),

	Res;


% If expression found:
%
% "If E is an if expression if Ic_1 ; ... ; Ic_k end, where each Ic_i is an if
% clause, then Rep(E) = {'if',LINE,[Rep(Ic_1), ..., Rep(Ic_k)]}."
%
transform_expression( _E={ 'if', Line, Clauses }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting if expression ~p...", [ E ] ),

	NewClauses = ast_clause:transform_if_clauses( Clauses, Transforms ),

	Res = { 'if', Line, NewClauses },

	%ast_utils:display_debug( "... returning if expression ~p", [ Res ] ),

	Res;


% Case expression found:
%
% "If E is a case expression case E_0 of Cc_1 ; ... ; Cc_k end, where E_0 is an
% expression and each Cc_i is a case clause, then Rep(E) =
% {'case',LINE,Rep(E_0),[Rep(Cc_1), ..., Rep(Cc_k)]}."
%
transform_expression( _E={ 'case', Line, TestExpression, CaseClauses },
					  Transforms ) ->

	%ast_utils:display_debug( "Intercepting case expression ~p...", [ E ] ),

	NewTestExpression = transform_expression( TestExpression, Transforms ),

	NewCaseClauses = ast_clause:transform_case_clauses( CaseClauses,
														Transforms ),

	Res = { 'case', Line, NewTestExpression, NewCaseClauses },

	%ast_utils:display_debug( "... returning case expression ~p", [ Res ] ),

	Res;


% Match expression found:
%
% "If E is a match operator expression P = E_0, where P is a pattern, then
% Rep(E) = {match,LINE,Rep(P),Rep(E_0)}."
%
transform_expression( _E={ 'match', Line, MatchPattern, MatchExpression },
					  Transforms ) ->

	%ast_utils:display_debug( "Intercepting match expression ~p...", [ E ] ),

	NewMatchPattern = ast_pattern:transform_pattern( MatchPattern, Transforms ),

	NewMatchExpression = transform_expression( MatchExpression, Transforms ),

	Res = { 'match', Line, NewMatchPattern, NewMatchExpression },

	%ast_utils:display_debug( "... returning match expression ~p", [ Res ] ),

	Res;



% Bin expression found:
%
% "If E is a bitstring constructor <<E_1:Size_1/TSL_1, ..., E_k:Size_k/TSL_k>>,
% where each Size_i is an expression and each TSL_i is a type specificer list,
% then Rep(E) = {bin,LINE,[{bin_element,LINE,Rep(E_1),Rep(Size_1),Rep(TSL_1)},
% ..., {bin_element,LINE,Rep(E_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see
% below. An omitted Size_i is represented by default. An omitted TSL_i is
% represented by default."
%
transform_expression( _E={ 'bin', Line, BinElemPatterns }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting bin expression ~p...", [ E ] ),

	NewBinElemPattern = ast_bitstring:transform_bin_elements( BinElemPatterns,
															  Transforms ),

	Res = { 'bin', Line, NewBinElemPattern },

	%ast_utils:display_debug( "... returning bin expression ~p", [ Res ] ),

	Res;


% Unary operation expression found:
%
% "If E is an operator expression Op E_0, where Op is a unary operator, then
% Rep(E) = {op,LINE,Op,Rep(E_0)}."
%
transform_expression( _E={ 'op', Line, Operator, Operand }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting unary operation expression ~p...",
	%						 [ E ] ),

	NewOperand = transform_expression( Operand, Transforms ),

	Res = { 'op', Line, Operator, NewOperand },

	%ast_utils:display_debug( "... returning unary operation expression ~p",
	%						 [ Res ] ),

	Res;


% Binary operation expression found:
%
% "If E is an operator expression E_1 Op E_2, where Op is a binary operator
% other than match operator =, then Rep(E) = {op,LINE,Op,Rep(E_1),Rep(E_2)}."
%
transform_expression( _E={ 'op', Line, Operator, LeftOperand, RightOperand },
					  Transforms ) ->

	%ast_utils:display_debug( "Intercepting binary operation expression ~p...",
	%						 [ E ] ),

	NewLeftOperand = transform_expression( LeftOperand, Transforms ),

	NewRightOperand = transform_expression( RightOperand, Transforms ),

	Res = { 'op', Line, Operator, NewLeftOperand, NewRightOperand },

	%ast_utils:display_debug( "... returning binary operation expression ~p",
	%						 [ Res ] ),

	Res;



% Receive "simple" (with no 'after' clause) expression found:
%
% "If E is a receive expression receive Cc_1 ; ... ; Cc_k end, where each Cc_i
% is a case clause, then Rep(E) = {'receive',LINE,[Rep(Cc_1), ...,
% Rep(Cc_k)]}.."
%
transform_expression( _E={ 'receive', Line, ReceiveClauses }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting simple receive expression ~p...",
	%						 [ E ] ),

	% 'case' clauses relevant here:
	NewReceiveClauses = ast_clause:transform_case_clauses( ReceiveClauses,
														   Transforms ),

	Res = { 'receive', Line, NewReceiveClauses },

	%ast_utils:display_debug( "... returning simple receive expression ~p",
	%						 [ Res ] ),

	Res;


% Receive expression with 'after' found:
%
% "If E is a receive expression receive Cc_1 ; ... ; Cc_k after E_0 -> B_t end,
% where each Cc_i is a case clause, E_0 is an expression, and B_t is a body,
% then Rep(E) = {'receive',LINE,[Rep(Cc_1), ..., Rep(Cc_k)],Rep(E_0),Rep(B_t)}.
%
transform_expression( _E={ 'receive', Line, ReceiveClauses, AfterTest,
						   AfterExpressions }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting receive expression with after "
	%						 "~p...", [ E ] ),

	% 'case' clauses relevant here:
	NewReceiveClauses = ast_clause:transform_case_clauses( ReceiveClauses,
														   Transforms ),

	NewAfterTest = transform_expression( AfterTest, Transforms ),

	NewAfterExpressions = transform_expressions( AfterExpressions, Transforms ),

	Res = { 'receive', Line, NewReceiveClauses, NewAfterTest,
			NewAfterExpressions },

	%ast_utils:display_debug( "... returning receive expression with after "
	%						 "~p", [ Res ] ),

	Res;



% Try expression found (6 different forms treated in this single clause):
%
% - "If E is a try expression try B catch Tc_1 ; ... ; Tc_k end, where B is a
% body and each Tc_i is a catch clause, then Rep(E) =
% {'try',LINE,Rep(B),[],[Rep(Tc_1), ..., Rep(Tc_k)],[]}."
%
% - "If E is a try expression try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n
% end, where B is a body, each Cc_i is a case clause, and each Tc_j is a catch
% clause, then Rep(E) = {'try',LINE,Rep(B),[Rep(Cc_1), ...,
% Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],[]}."
%
% - "If E is a try expression try B after A end, where B and A are bodies, then
% Rep(E) = {'try',LINE,Rep(B),[],[],Rep(A)}."
%
% - "If E is a try expression try B of Cc_1 ; ... ; Cc_k after A end, where B
% and A are a bodies, and each Cc_i is a case clause, then Rep(E) =
% {'try',LINE,Rep(B),[Rep(Cc_1), ..., Rep(Cc_k)],[],Rep(A)}."
%
% - "If E is a try expression try B catch Tc_1 ; ... ; Tc_k after A end, where B
% and A are bodies, and each Tc_i is a catch clause, then Rep(E) =
% {'try',LINE,Rep(B),[],[Rep(Tc_1), ..., Rep(Tc_k)],Rep(A)}."
%
% - "If E is a try expression try B of Cc_1 ; ... ; Cc_k catch Tc_1 ; ... ; Tc_n
% after A end, where B and A are a bodies, each Cc_i is a case clause, and each
% Tc_j is a catch clause, then Rep(E) = {'try',LINE,Rep(B),[Rep(Cc_1), ...,
% Rep(Cc_k)],[Rep(Tc_1), ..., Rep(Tc_n)],Rep(A)}."
%
transform_expression( _E={ 'try', Line, TryBody, TryClauses, CatchClauses,
						   AfterBody }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting try expression ~p...", [ E ] ),

	NewTryBody = ast_clause:transform_body( TryBody, Transforms ),

	NewTryClauses = ast_clause:transform_try_clauses( TryClauses,
													  Transforms ),

	NewCatchClauses = ast_clause:transform_catch_clauses( CatchClauses,
														  Transforms ),

	NewAfterBody = ast_clause:transform_body( AfterBody, Transforms ),

	Res = { 'try', Line, NewTryBody, NewTryClauses, NewCatchClauses,
			NewAfterBody },

	%ast_utils:display_debug( "... returning try expression ~p", [ Res ] ),

	Res;


transform_expression( _E={ 'remote', Line, ModuleExpr, FunctionExpr },
					  Transforms ) ->

	% Apparently useful indeed:
	%ast_utils:display_warning( "Remote transform expression actually useful, "
	%						   "this warning can be silenced." ),

	NewModuleExpr = transform_expression( ModuleExpr, Transforms ),

	NewFunctionExpr = transform_expression( FunctionExpr, Transforms ),

	{ 'remote', Line, NewModuleExpr, NewFunctionExpr };


% Catch expression found:
%
% "If E is a catch expression catch E_0, then Rep(E) = {'catch',LINE,Rep(E_0)}."
%
transform_expression( _E={ 'catch', Line, Expression }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting catch expression ~p...", [ E ] ),

	NewExpression = transform_expression( Expression, Transforms ),

	Res = { 'catch', Line, NewExpression },

	%ast_utils:display_debug( "... returning catch expression ~p", [ Res ] ),

	Res;


% Cons expression found:
%
% "If E is a cons skeleton [E_h | E_t], then Rep(E) =
% {cons,LINE,Rep(E_h),Rep(E_t)}."
%
% Head and Tail members are expressions (not just patterns), as a member can
% for example be : {call,56, {remote, ...
%
transform_expression( _E={ 'cons', Line, HeadExpression, TailExpression },
					  Transforms ) ->

	%ast_utils:display_debug( "Intercepting cons expression ~p...", [ E ] ),

	NewHeadExpression = transform_expression( HeadExpression, Transforms ),

	NewTailExpression = transform_expression( TailExpression, Transforms ),

	Res = { 'cons', Line, NewHeadExpression, NewTailExpression },

	%ast_utils:display_debug( "... returning cons expression ~p", [ Res ] ),

	Res;


% List comprehension found:
%
% "If E is a list comprehension [E_0 || Q_1, ..., Q_k], where each Q_i is a
% qualifier, then Rep(E) = {lc,LINE,Rep(E_0),[Rep(Q_1), ..., Rep(Q_k)]}. For
% Rep(Q), see below.."
%
transform_expression( _E={ 'lc', Line, Expression, Qualifiers }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting list comprehension ~p...", [ E ] ),

	NewExpression = transform_expression( Expression, Transforms ),

	NewQualifiers = transform_qualifiers( Qualifiers, Transforms ),

	Res = { 'lc', Line, NewExpression, NewQualifiers },

	%ast_utils:display_debug( "... returning list comprehension ~p", [ Res ] ),

	Res;


% Bitstring comprehension found:
%
% "If E is a bitstring comprehension <<E_0 || Q_1, ..., Q_k>>, where each Q_i is
% a qualifier, then Rep(E) = {bc,LINE,Rep(E_0),[Rep(Q_1), ..., Rep(Q_k)]}."
%
transform_expression( _E={ 'bc', Line, Expression, Qualifiers }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting bitstring comprehension ~p...",
	%						 [ E ] ),

	NewExpression = transform_expression( Expression, Transforms ),

	NewQualifiers = transform_qualifiers( Qualifiers, Transforms ),

	Res = { 'bc', Line, NewExpression, NewQualifiers },

	%ast_utils:display_debug( "... returning bitstring comprehension ~p",
	%						 [ Res ] ),

	Res;


% Tuple skeleton found:
%
% "If E is a tuple skeleton {E_1, ..., E_k}, then Rep(E) =
% {tuple,LINE,[Rep(E_1), ..., Rep(E_k)]}."
%
transform_expression( _E={ 'tuple', Line, Expressions }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting tuple skeleton ~p...", [ E ] ),

	NewExpressions = transform_expressions( Expressions, Transforms ),

	Res = { 'tuple', Line, NewExpressions },

	%ast_utils:display_debug( "... returning tuple skeleton ~p", [ Res ] ),

	Res;



% Map creation found:
%
% "If E is a map creation #{A_1, ..., A_k}, where each A_i is an association
% E_i_1 => E_i_2 or E_i_1 := E_i_2, then Rep(E) = {map,LINE,[Rep(A_1), ...,
% Rep(A_k)]}."
%
transform_expression( _E={ 'map', Line, Expressions }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting map creation ~p...", [ E ] ),

	NewExpressions = transform_expressions( Expressions, Transforms ),

	Res = { 'map', Line, NewExpressions },

	%ast_utils:display_debug( "... returning map creation ~p", [ Res ] ),

	Res;


% Map update found:
%
% "If E is a map update E_0#{A_1, ..., A_k}, where each A_i is an association
% E_i_1 => E_i_2 or E_i_1 := E_i_2, then Rep(E) = {map,LINE,Rep(E_0),[Rep(A_1),
% ..., Rep(A_k)]}."
%
transform_expression( _E={ 'map', Line, MapRefExpression, AssocExpressions },
					  Transforms ) ->

	%ast_utils:display_debug( "Intercepting map update ~p...", [ E ] ),

	[ NewMapRefExpression | NewAssocExpressions ] = transform_expressions(
				  [ MapRefExpression | AssocExpressions ], Transforms ),

	Res = { 'map', Line, NewMapRefExpression, NewAssocExpressions },

	%ast_utils:display_debug( "... returning map update ~p", [ Res ] ),

	Res;


% Map field association found:
%
% "If A is an association K => V, then Rep(A) =
% {map_field_assoc,LINE,Rep(K),Rep(V)}."
%
transform_expression( _E={ 'map_field_assoc', Line, KeyExpression,
						  ValueExpression }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting map association ~p...", [ E ] ),

	NewKeyExpression = transform_expression( KeyExpression, Transforms ),

	NewValueExpression = transform_expression( ValueExpression, Transforms ),

	Res = { 'map_field_assoc', Line, NewKeyExpression, NewValueExpression },

	%ast_utils:display_debug( "... returning map association ~p", [ Res ] ),

	Res;


% Map exact field association found:
%
% "If A is an association K := V, then Rep(A) =
% {map_field_exact,LINE,Rep(K),Rep(V)}."
%
transform_expression( _E={ 'map_field_exact', Line, KeyExpression,
						  ValueExpression }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting map exact association ~p...",
	%						 [ E ] ),

	NewKeyExpression = transform_expression( KeyExpression, Transforms ),

	NewValueExpression = transform_expression( ValueExpression, Transforms ),

	Res = { 'map_field_exact', Line, NewKeyExpression, NewValueExpression },

	%ast_utils:display_debug( "... returning map exact association ~p",
	%						 [ Res ] ),

	Res;



% No 'struct' to be managed (cf. erl_id_trans, commented-out).



% Record creation expression found:
%
% "If E is a record creation #Name{Field_1=E_1, ..., Field_k=E_k}, where each
% Field_i is an atom or _, then Rep(E) =
% {record,LINE,Name,[{record_field,LINE,Rep(Field_1),Rep(E_1)}, ...,
% {record_field,LINE,Rep(Field_k),Rep(E_k)}]}."
%
transform_expression( _E={ 'record', Line, RecordName, FieldInits },
					  Transforms ) ->

	%ast_utils:display_debug( "Intercepting record creation expression ~p...",
	%						 [ E ] ),

	NewFieldInits = transform_record_field_inits( FieldInits, Transforms ),

	Res = { 'record', Line, RecordName, NewFieldInits },

	%ast_utils:display_debug( "... returning record creation expression ~p",
	%						 [ Res ] ),

	Res;


% Record index expression found:
%
% "If E is a record field index #Name.Field, where Field is an atom, then Rep(E)
% = {record_index,LINE,Name,Rep(Field)}."
%
transform_expression( _E={ 'record_index', Line, RecordName, FieldName },
					  Transforms ) ->

	%ast_utils:display_debug( "Intercepting record index expression ~p...",
	%						 [ E ] ),

	NewFieldName = transform_expression( FieldName, Transforms ),

	Res = { 'record_index', Line, RecordName, NewFieldName },

	%ast_utils:display_debug( "... returning record index expression ~p",
	%						 [ Res ] ),

	Res;


% Record field access found:
%
% "If E is a record field access E_0#Name.Field, where Field is an atom, then
% Rep(E) = {record_field,LINE,Rep(E_0),Name,Rep(Field)}."
%
transform_expression( _E={ 'record_field', Line, RecordRef, RecordName,
						  FieldName }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting record field access expression "
	%						 "~p...", [ E ] ),

	NewRecordRef = transform_expression( RecordRef, Transforms ),

	NewFieldName = transform_expression( FieldName, Transforms ),

	Res = { 'record_field', Line, NewRecordRef, RecordName, NewFieldName },

	%ast_utils:display_debug( "... returning record field access expression ~p",
	%						 [ Res ] ),

	Res;


% Record field found:
%
% (not found apparently in http://erlang.org/doc/apps/erts/absform.html)
%
transform_expression( _E={ 'record_field', Line, RecordRef, Field },
					  Transforms ) ->

	% Expected never to be displayed:
	ast_utils:display_warning( "Clause about record field expression "
							   "actually triggered." ),

	%ast_utils:display_debug( "Intercepting record field expression ~p...",
	%						 [ E ] ),

	NewRecordRef = transform_expression( RecordRef, Transforms ),

	NewField = transform_expression( Field, Transforms ),

	Res = { 'record_field', Line, NewRecordRef, NewField },

	%ast_utils:display_debug( "... returning record field expression ~p",
	%						 [ Res ] ),

	Res;


% Record update found:
%
% "If E is a record update E_0#Name{Field_1=E_1, ..., Field_k=E_k}, where each
% Field_i is an atom, then Rep(E) =
% {record,LINE,Rep(E_0),Name,[{record_field,LINE,Rep(Field_1),Rep(E_1)}, ...,
% {record_field,LINE,Rep(Field_k),Rep(E_k)}]}."
%
transform_expression( _E={ 'record', Line, RecordRef, RecordName,
						  FieldUpdates }, Transforms ) ->

	%ast_utils:display_debug( "Intercepting record update expression ~p...",
	%						 [ E ] ),

	NewRecordRef = transform_expression( RecordRef, Transforms ),

	NewFieldUpdates = transform_record_field_updates( FieldUpdates,
													  Transforms ),

	Res = { 'record', Line, NewRecordRef, RecordName, NewFieldUpdates },

	%ast_utils:display_debug( "... returning record update expression ~p",
	%						 [ Res ] ),

	Res;



% Block expression found:
%
% "If E is a block expression begin B end, where B is a body, then Rep(E) =
% {block,LINE,Rep(B)}."
%
transform_expression( _E={ 'block', Line, Expressions }, Transforms ) ->

	% Unfolds this block into a sequence of expressions:
	NewExpressions = transform_expressions( Expressions, Transforms ),

	{ 'block', Line, NewExpressions };



% Fun expression found:


% "If E is a fun expression fun Fc_1 ; ... ; Fc_k end, where each Fc_i is a
% function clause, then Rep(E) = {'fun',LINE,{clauses,[Rep(Fc_1), ...,
% Rep(Fc_k)]}}."
%
transform_expression( _E={ 'fun', Line, { 'clauses', FunctionClauses } },
					  Transforms ) ->

	%ast_utils:display_debug( "Intercepting clause-based fun expression ~p...",
	%						 [ E ] ),

	NewFunctionClauses = ast_clause:transform_function_clauses( FunctionClauses,
																Transforms ),

	Res = { 'fun', Line, { 'clauses', NewFunctionClauses } },

	%ast_utils:display_debug( "... returning clause-based fun expression ~p",
	%						 [ Res ] ),

	Res;


% "If E is a fun expression fun Name/Arity, then Rep(E) =
% {'fun',LINE,{function,Name,Arity}}."
%
transform_expression( E={ 'fun', _Line, { 'function', _Name, _Arity } },
					  _Transforms ) ->

	%ast_utils:display_debug( "Intercepting local fun expression ~p...",
	%						 [ E ] ),

	%NewName = transform_expression( Name, Transforms ),

	%NewArity = transform_expression( Arity, Transforms ),

	% Apparently no possible transformation, already fully resolved:
	% (see expr/1 in erl_id_trans)
	%
	%Res = { 'fun', Line, { function, NewName, NewArity } },
	Res = E,

	%ast_utils:display_debug( "... returning local fun expression ~p",
	%						 [ Res ] ),

	Res;


% Managing specifically the fact that, before Erlang/OTP R15, Rep(E) =
% {'fun',LINE,{function,Module,Name,Arity}}.
%
transform_expression( E={ 'fun', _Line,
						  _F={ 'function', Module, Name, Arity } },
					  _Transforms ) when is_atom( Module )
										andalso is_atom( Name )
										andalso is_integer( Arity ) ->

	ast_utils:display_warning( "Pre-R15 fun expression '~p' detected, "
							   "this warning should be silenced.", [ E ] ),

	%ast_utils:display_debug( "Intercepting pre-R15 fun expression ~p...",
	%						 [ E ] ),

	Res = E,

	%ast_utils:display_debug( "... returning pre-R15 fun expression ~p",
	%						 [ Res ] ),

	Res;


% "If E is a fun expression fun Module:Name/Arity, then Rep(E) =
% {'fun',LINE,{function,Rep(Module),Rep(Name),Rep(Arity)}}."
%
% Since R15, fun M:F/A can be obtained through variables.
%
transform_expression( _E={ 'fun', Line, _F={ 'function', ModuleName,
											FunctionName, FunctionArity } },
					  Transforms ) ->

	%ast_utils:display_debug( "Intercepting remote fun expression ~p...",
	%						 [ E ] ),

	NewModuleName = transform_expression( ModuleName, Transforms ),

	NewFunctionName = transform_expression( FunctionName, Transforms ),

	NewFunctionArity = transform_expression( FunctionArity, Transforms ),

	Res = { 'fun', Line,
			{ 'function', NewModuleName, NewFunctionName, NewFunctionArity } },

	%ast_utils:display_debug( "... returning remote fun expression ~p",
	%						 [ Res ] ),

	Res;


% "If E is a variable V, then Rep(E) = {var,LINE,A}, where A is an atom with a
% printname consisting of the same characters as V."
%
transform_expression( E={ 'var', _Line, _VarAtomName }, _Transforms ) ->

	%ast_utils:display_debug( "Intercepting var expression with clauses ~p...",
	%						 [ E ] ),

	% Currently names not transformed:
	%NewVarAtomName = VarAtomName,

	%Res = { 'var', Line, NewVarAtomName },
	Res = E,

	%ast_utils:display_debug( "... returning var expression with clauses ~p",
	%						 [ Res ] ),

	Res;


% "If E is nil, [], then Rep(E) = {nil,LINE}."
%
transform_expression( E={ 'nil', _Line }, _Transforms ) ->

	%ast_utils:display_debug( "Intercepting nil expression with clauses ~p...",
	%						 [ E ] ),

	% Currently not transformed:
	Res = E,

	%ast_utils:display_debug( "... returning nil expression with clauses ~p",
	%						 [ Res ] ),

	Res;



% "If E is a fun expression fun Name Fc_1 ; ... ; Name Fc_k end, where Name is a
% variable and each Fc_i is a function clause, then Rep(E) =
% {named_fun,LINE,Name,[Rep(Fc_1), ..., Rep(Fc_k)]}."
%
transform_expression( _E={ 'named_fun', Line, Name, FunctionClauses },
					  Transforms ) ->

	%ast_utils:display_debug( "Intercepting named fun expression ~p...",
	%						 [ E ] ),

	NewFunctionClauses = ast_clause:transform_function_clauses(
						   FunctionClauses, Transforms ),

	Res = { 'named_fun', Line, Name, NewFunctionClauses },

	%ast_utils:display_debug( "... returning named fun expression ~p",
	%						 [ Res ] ),

	Res;


% "If E is an atomic literal L, then Rep(E) = Rep(L)."
%
% Wish type_utils:get_immediate_types/0 could be used in a guard.
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



% Transforms specificied qualifiers.
%
% Allows filters to be both guard tests and general expressions.
%
% See also: lc_bc_quals/1 in erl_id_trans
%
-spec transform_qualifiers( [ ast_qualifier() ], ast_transforms() ) ->
								  [ ast_qualifier() ].
transform_qualifiers( Qualifiers, Transforms ) ->
	[ transform_qualifier( Q, Transforms ) || Q <- Qualifiers ].


% Transforms specificied qualifier.
-spec transform_qualifier( ast_qualifier(), ast_transforms() ) ->
								 ast_qualifier().

% "If Q is a (lc) generator P <- E, where P is a pattern and E is an expression,
% then Rep(Q) = {generate,LINE,Rep(P),Rep(E)}."
%
transform_qualifier( _Qualifier={ 'generate', Line, Pattern, Expression },
					 Transforms ) ->

	NewPattern = ast_pattern:transform_pattern( Pattern, Transforms ),

	NewExpression = transform_expression( Expression, Transforms ),

	{ 'generate', Line, NewPattern, NewExpression };


% "If Q is a bitstring generator P <= E, where P is a pattern and E is an
% expression, then Rep(Q) = {b_generate,LINE,Rep(P),Rep(E)}."
%
transform_qualifier( _Qualifier={ 'b_generate', Line, Pattern, Expression },
					 Transforms ) ->

	NewPattern = ast_pattern:transform_pattern( Pattern, Transforms ),

	NewExpression = transform_expression( Expression, Transforms ),

	{ 'b_generate', Line, NewPattern, NewExpression };


% "If Q is a filter E, where E is an expression, then Rep(Q) = Rep(E)."
%
transform_qualifier( _Qualifier=Expression, Transforms ) ->
	transform_expression( Expression, Transforms ).




% (corresponds to record_inits/1 in erl_id_trans)
%
% Field names are full expressions here, but only atoms are allowed by the
% linter.
%
% (helper)
%
transform_record_field_inits( RecordFieldInits, Transforms ) ->
	[ transform_record_field_init( RFI, Transforms )
	  || RFI <- RecordFieldInits ].


% Includes the case where FieldName is '_':
transform_record_field_init( { 'record_field', LineField,
		   FieldNameASTAtom={ atom, _LineAtom, _FieldName }, FieldValue },
							 Transforms ) ->

	NewFieldValue = transform_expression( FieldValue, Transforms ),

	{ 'record_field', LineField, FieldNameASTAtom, NewFieldValue }.



% (corresponds to record_updates/1 in erl_id_trans)
%
% Field names are full expressions here, but only atoms are allowed by the
% linter.
%
% (helper)
%
transform_record_field_updates( RecordFieldUpdates, Transforms ) ->
	[ transform_record_field_update( RFU, Transforms )
	  || RFU <- RecordFieldUpdates ].


transform_record_field_update( { 'record_field', LineField,
		   FieldNameASTAtom={ atom, _LineAtom, _FieldName }, FieldValue },
							   Transforms ) ->

	NewFieldValue = transform_expression( FieldValue, Transforms ),

	{ record_field, LineField, FieldNameASTAtom, NewFieldValue }.





% Remote call expression found:
%
% "If E is a function call E_m:E_0(E_1, ..., E_k), then Rep(E) =
% {call,LINE,{remote,LINE,Rep(E_m),Rep(E_0)},[Rep(E_1), ..., Rep(E_k)]}.
%
% Remote call expression found, with an immediate name for both the module and
% the function:
%
% (parameters already transformed)
%
transform_call_expression( { 'remote', LineRemote,
		M={ atom, LineMod, ModuleName },
		F={ atom, LineFun, FunctionName } },
						   Arity, Transforms ) ->

	%ast_utils:display_debug( "Intercepting remote call expression "
	%						 "to ~s:~s/~B...",
	%						 [ ModuleName, FunctionName, Arity ] ),

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
			Res = { 'remote', LineRemote, M, F },
			%ast_utils:display_debug( "... returning remote call expression "
			%						 "(case R1) ~p", [ Res ] ),
			Res;

		{ SetModuleName, SetFunctionName } ->

			Res = { 'remote', LineRemote, { atom, LineMod, SetModuleName },
					{ atom, LineFun, SetFunctionName } },

			%ast_utils:display_debug( "... returning remote call expression "
			%						 "(case R2) ~p", [ Res ] ),
			Res

	end;


% Here, at least one name (module and/or function) is not immediate in that
% remote call expression:
%
% (note: we do not manage yet the case where for example the function name
% results from an expression yet a wildcard has been defined for it)
%
transform_call_expression( _FunctionRef={ 'remote', LineRemote, ModuleExpr,
										FunctionExpr },
						   _Arity, Transforms ) ->

	%ast_utils:display_debug( "Intercepting non-immediate remote call "
	%						 "expression ~p...", [ FunctionRef ] ),

	NewModuleExpr = transform_expression( ModuleExpr, Transforms ),

	NewFunctionExpr = transform_expression( FunctionExpr, Transforms ),

	Res = { 'remote', LineRemote, NewModuleExpr, NewFunctionExpr },

	%ast_utils:display_debug( "... returning non-immediate remote call "
	%						 "expression (case R3) ~p", [ Res ] ),

	Res;


% Local call expression found:
%
% "If E is a function call E_0(E_1, ..., E_k), then Rep(E) =
% {call,LINE,Rep(E_0),[Rep(E_1), ..., Rep(E_k)]}."
%
transform_call_expression( CallExpr={ 'atom', LineFun, FunName }, Arity,
						   Transforms ) ->

	%ast_utils:display_debug( "Intercepting local call expression ~p...",
	%						 [ CallExpr ] ),

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
						%   { NewModuleName, FunName };

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
			Res = CallExpr,
			%ast_utils:display_debug( "... returning local call expression ~p",
			%						 [ Res ] ),
			Res;

		{ SetModuleName, SetFunctionName } ->
			Res = { 'remote', LineFun, SetModuleName, SetFunctionName },
			%ast_utils:display_debug( "... returning remote call expression ~p",
			%						 [ Res ] ),
			Res

	end;


% Ex: happens with a line like: 'MyNode = MyContentFun( Content, "hello" )'.
%
transform_call_expression( CallExpr, _Arity, Transforms ) ->

	NewCallExpr = transform_expression( CallExpr, Transforms ),

	Res = NewCallExpr,

	Res.
