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



% Module in charge of handling guards defined with an AST.
%
% See the "7.6 Guards" section of http://erlang.org/doc/apps/erts/absform.html
% for more information.
%
-module(ast_guard).



% The description of a sequence of guards in an AST.
%
% "A guard sequence Gs is a sequence of guards G_1; ...; G_k, and Rep(Gs) =
% [Rep(G_1), ..., Rep(G_k)]. If the guard sequence is empty, then Rep(Gs) = []."
%
% Ex: {call,102, {atom,102,is_integer}, [{var,102,'X'}]}
%
-type ast_guard_sequence() :: [ ast_guard() ].



% The description of a guard in an AST.
%
% "A guard G is a non-empty sequence of guard tests Gt_1, ..., Gt_k, and Rep(G)
% = [Rep(Gt_1), ..., Rep(Gt_k)]."
%
%
-type ast_guard() :: nonempty_list( ast_guard_test() ).



% The description of a guard test in an AST.
%
-type ast_guard_test() ::
		ast_bitstring:constructor( ast_guard_test() )
	  | { 'cons', line(), ast_guard_test(), ast_guard_test() }
	  | { 'call', line(),
		  { 'remote', line(), ast_base:ast_element(), ast_base:ast_atom() }
			| ast_base:ast_atom(),
		  [ ast_guard_test() ] }
	  | ast_map:ast_map_form( ast_guard_test() ).



% "If Gt is a bitstring constructor <<Gt_1:Size_1/TSL_1, ...,
% Gt_k:Size_k/TSL_k>>, where each Size_i is a guard test and each TSL_i is a
% type specificer list, then Rep(Gt) =
% {bin,LINE,[{bin_element,LINE,Rep(Gt_1),Rep(Size_1),Rep(TSL_1)}, ...,
% {bin_element,LINE,Rep(Gt_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see
% above.
%
% An omitted Size_i is represented by default. An omitted TSL_i is
% represented by default."
%
% So apparently a guard test is a recursive type:
%
-type ast_bitstring_constructor() ::
		ast_bitstring:constructor( ast_guard_test() ).


% Defined in the context of a guard:
-type ast_bitstring_bin_element() :: ast_bitstring:bin_element( ast_guard_test() ).



-export_type([ ast_guard_sequence/0, ast_guard/0, ast_guard_test/0,
			   ast_bitstring_constructor/0, ast_bitstring_bin_element/0 ]).


-export([ transform_guard_test/2, transform_guard/2, transform_guard_sequence/2 ]).


% Shorthands:

-type line() :: ast_base:line().
-type ast_transforms() :: ast_transform:ast_transforms().



% Transforms specified guard test, operating relevant AST transformations.
%
% (see section 7.6 for complete detail)
%
% Note that we should not consider that guard tests are simply AST expressions
% (they are only a very specific subset thereof, with guard-specific rules -
% hence not plugging here to any expression-generic code).
%
% If the guard test is a bitstring constructor:
%
% "If Gt is a bitstring constructor <<Gt_1:Size_1/TSL_1, ...,
% Gt_k:Size_k/TSL_k>>, where each Size_i is a guard test and each TSL_i is a
% type specificer list, then Rep(Gt) =
% {bin,LINE,[{bin_element,LINE,Rep(Gt_1),Rep(Size_1),Rep(TSL_1)}, ...,
% {bin_element,LINE,Rep(Gt_k),Rep(Size_k),Rep(TSL_k)}]}. For Rep(TSL), see
% above. An omitted Size_i is represented by default. An omitted TSL_i is
% represented by default."
%
-spec transform_guard_test( ast_guard_test(), ast_transforms() ) ->
								  ast_guard_test().
transform_guard_test( GuardTest={ bin, Line, BinElements }, Transforms ) ->

	ast_utils:display_debug( "Intercepting guard test bitstring "
							 "constructor ~p...", [ GuardTest ] ),

	% Actually we are not sure Gt_1 is a guard test (maybe is more globally an
	% expression):
	%
	NewBinElements = ast_bitstring:transform_bin_elements( BinElements,
						   Transforms, fun transform_guard_test/2 ),

	Res = { bin, Line, NewBinElements },

	ast_utils:display_debug( "... returning guard test bitstring "
							 "constructor ~p", [ Res ] ),

	Res;


% "If Gt is a cons skeleton [Gt_h | Gt_t], then Rep(Gt) =
% {cons,LINE,Rep(Gt_h),Rep(Gt_t)}."
%
transform_guard_test( GuardTest={ cons, Line, HeadGuardTest, TailGuardTest },
					  Transforms ) ->

	ast_utils:display_debug( "Intercepting guard test cons skeleton ~p...",
							 [ GuardTest ] ),

	% We do not try to generalise this code (ex: by defining and using a
	% ast_transform:transform_cons( H, T, Transforms)) as, depending on the
	% context (ex: for guards), different AST structures are expected.

	% So, not ast_expression:transform_expression/2 here:
	NewHeadGuardTest = transform_guard_test( HeadGuardTest, Transforms ),

	% Expecting a list for tail?
	NewTailGuardTest = transform_guard_test( TailGuardTest, Transforms ),

	Res = { cons, Line, NewHeadGuardTest, NewTailGuardTest },

	ast_utils:display_debug( "... returning guard test cons skeleton ~p",
							 [ Res ] ),

	Res;


% Remote call (only to the 'erlang' module):
%
% "If Gt is a function call A_m:A(Gt_1, ..., Gt_k), where A_m is the atom erlang
% and A is an atom or an operator, then Rep(Gt) =
% {call,LINE,{remote,LINE,Rep(A_m),Rep(A)},[Rep(Gt_1), ..., Rep(Gt_k)]}.
%
transform_guard_test( GuardTest={ call, Line1,
			R={ remote, Line2, { atom, _Line3, _Module }, FunctionDesignator },
			SubGuardTests },
					  Transforms ) ->

	% Here, Module can only be 'erlang', and FunctionDesignator could be
	% checked, yet we are not (re)implementing the compiler.

	ast_utils:display_debug( "Intercepting guard test remote call "
							 " ~p...", [ GuardTest ] ),

	% Waiting for an operator to be met:
	ast_type:check_ast_atom( FunctionDesignator, Line2 ),

	NewSubGuardTests = [ transform_guard_test( GT, Transforms )
						 || GT <- SubGuardTests ],

	NewGuardTest = { call, Line1, R, NewSubGuardTests },

	ast_utils:display_debug( "... returning guard test remote call ~p",
							 [ NewGuardTest ] ),

	NewGuardTest;


% Local call (to a builtin-only):
%
% "If Gt is a function call A(Gt_1, ..., Gt_k), where A is an atom, then Rep(Gt)
% = {call,LINE,Rep(A),[Rep(Gt_1), ..., Rep(Gt_k)]}."
%
% Ex: {call,102, {atom,102,is_integer}, [{var,102,'X'}]}
%
transform_guard_test( GuardTest={ call, Line, FunctionName, GuardTests },
					  Transforms ) ->

	ast_utils:display_debug( "Intercepting guard test call ~p...",
							 [ GuardTest ] ),

	ast_utils:check_ast_atom( FunctionName, Line ),

	NewGuardTests = [ transform_guard_test( GT, Transforms )
						 || GT <- GuardTests ],

	NewGuardTest = { call, Line, FunctionName, NewGuardTests },

	ast_utils:display_debug( "... returning guard test call ~p",
							 [ NewGuardTest ] ),

	NewGuardTest;


% "If Gt is a map creation #{A_1, ..., A_k}, where each A_i is an association
% Gt_i_1 => Gt_i_2 or Gt_i_1 := Gt_i_2, then Rep(Gt) = {map,LINE,[Rep(A_1), ...,
% Rep(A_k)]}."
%
transform_guard_test( GuardTest={ map, Line, MapAssociations }, Transforms ) ->

	ast_utils:display_debug( "Intercepting guard test map creation ~p...",
							 [ GuardTest ] ),

	NewMapAssociations = ast_map:transform_map_associations( MapAssociations,
							Transforms, fun transform_guard_test/2 ),

	NewGuardTest = { map, Line, NewMapAssociations },

	ast_utils:display_debug( "... returning guard test map creation ~p",
							 [ NewGuardTest ] ),

	NewGuardTest;


% "If Gt is a map update Gt_0#{A_1, ..., A_k}, where each A_i is an association
% Gt_i_1 => Gt_i_2 or Gt_i_1 := Gt_i_2, then Rep(Gt) =
% {map,LINE,Rep(Gt_0),[Rep(A_1), ..., Rep(A_k)]}."
%
transform_guard_test( GuardTest={ map, Line, BaseMap, MapAssociations }, Transforms ) ->

	ast_utils:display_debug( "Intercepting guard test map update ~p...",
							 [ GuardTest ] ),

	NewBaseMap = transform_guard_test( BaseMap, Transforms ),

	NewMapAssociations = ast_map:transform_map_associations( MapAssociations,
							Transforms, fun transform_guard_test/2 ),

	NewGuardTest = { map, Line, NewBaseMap, NewMapAssociations },

	ast_utils:display_debug( "... returning guard test map update ~p",
							 [ NewGuardTest ] ),

	NewGuardTest;


% "If Gt is nil, [], then Rep(Gt) = {nil,LINE}."
%
transform_guard_test( GuardTest={ nil, _Line }, _Transforms ) ->
	GuardTest;


% "If Gt is an operator guard test Gt_1 Op Gt_2, where Op is a binary operator
% other than match operator =, then Rep(Gt) = {op,LINE,Op,Rep(Gt_1),Rep(Gt_2)}.
%
transform_guard_test( GuardTest={ op, Line, Op, LeftOperand, RightOperand },
					  Transforms ) when Op =/= '=' ->

	ast_utils:display_debug( "Intercepting guard test non-equal binary "
							 "operator ~p...", [ GuardTest ] ),

	NewLeftOperand = transform_guard_test( LeftOperand, Transforms ),

	NewRightOperand = transform_guard_test( RightOperand, Transforms ),

	NewGuardTest = { op, Line, Op, NewLeftOperand, NewRightOperand },

	ast_utils:display_debug( "... returning guard test non-equal binary "
							 "operator ~p", [ NewGuardTest ] ),

	NewGuardTest;


% "If Gt is an operator guard test Op Gt_0, where Op is a unary operator, then
% Rep(Gt) = {op,LINE,Op,Rep(Gt_0)}.
%
transform_guard_test( GuardTest={ op, Line, Op, Operand }, Transforms ) ->

	ast_utils:display_debug( "Intercepting guard test unary operator ~p...",
							 [ GuardTest ] ),

	NewOperand = transform_guard_test( Operand, Transforms ),

	NewGuardTest = { op, Line, Op, NewOperand },

	ast_utils:display_debug( "... returning guard test unary operator ~p",
							 [ NewGuardTest ] ),

	NewGuardTest;


% "If Gt is a parenthesized guard test ( Gt_0 ), then Rep(Gt) = Rep(Gt_0), that
% is, parenthesized guard tests cannot be distinguished from their bodies."


% "If Gt is a record creation #Name{Field_1=Gt_1, ..., Field_k=Gt_k}, where each
% Field_i is an atom or _, then Rep(Gt) =
% {record,LINE,Name,[{record_field,LINE,Rep(Field_1),Rep(Gt_1)}, ...,
% {record_field,LINE,Rep(Field_k),Rep(Gt_k)}]}.
%
transform_guard_test( GuardTest={ record, Line, RecordName, RecordFields },
					  Transforms ) ->

	ast_utils:display_debug( "Intercepting guard test record creation ~p...",
							 [ GuardTest ] ),

	ast_type:check_ast_atom( RecordName, Line ),

	NewRecordFields = ast_record:transform_record_field_definitions( 
						RecordFields, Transforms ),

	NewGuardTest = { record, Line, RecordName, NewRecordFields },

	ast_utils:display_debug( "... returning guard test record creation ~p",
							 [ NewGuardTest ] ),

	NewGuardTest;


% "If Gt is a record field access Gt_0#Name.Field, where Field is an atom, then
% Rep(Gt) = {record_field,LINE,Rep(Gt_0),Name,Rep(Field)}."
%
transform_guard_test( GuardTest={ record_field, Line, RecordGuardTest,
								  RecordName, FieldName },
					  Transforms ) ->

	ast_utils:display_debug( "Intercepting guard test record field access "
							 "~p...", [ GuardTest ] ),

	NewRecordGuardTest = transform_guard_test( RecordGuardTest, Transforms ),

	ast_type:check_ast_atom( RecordName, Line ),

	ast_type:check_ast_atom( FieldName, Line ),

	NewGuardTest = { record_field, Line, NewRecordGuardTest, RecordName,
					 FieldName },

	ast_utils:display_debug( "... returning guard test record field "
							 "access ~p", [ NewGuardTest ] ),

	NewGuardTest;


% "If Gt is a record field index #Name.Field, where Field is an atom, then
% Rep(Gt) = {record_index,LINE,Name,Rep(Field)}."
%
transform_guard_test( GuardTest={ record_index, Line, RecordName, FieldName },
					  _Transforms ) ->

	ast_utils:display_debug( "Intercepting guard test record field index "
							 "~p...", [ GuardTest ] ),

	ast_type:check_ast_atom( RecordName, Line ),

	ast_type:check_ast_atom( FieldName, Line ),

	NewGuardTest = GuardTest,

	ast_utils:display_debug( "... returning guard test record field "
							 "index ~p", [ NewGuardTest ] ),

	NewGuardTest;


% "If Gt is a tuple skeleton {Gt_1, ..., Gt_k}, then Rep(Gt) =
% {tuple,LINE,[Rep(Gt_1), ..., Rep(Gt_k)]}.
%
transform_guard_test( GuardTest={ tuple, Line, GuardTests }, Transforms ) ->

	ast_utils:display_debug( "Intercepting guard test tuple skeleton "
							 "~p...", [ GuardTest ] ),

	NewGuardTests = [ transform_guard_test( GT, Transforms )
					  || GT <- GuardTests ],

	NewGuardTest = { tuple, Line, NewGuardTests },

	ast_utils:display_debug( "... returning guard test tuple skeleton "
							 "~p", [ NewGuardTests ] ),

	NewGuardTest;


% "If Gt is a variable pattern V, then Rep(Gt) = {var,LINE,A}, where A is an
% atom with a printname consisting of the same characters as V."
%
transform_guard_test( GuardTest={ var, _Line, VarName }, _Transforms ) ->

	type_utils:check_atom( VarName ),

	GuardTest;


% Default, catch-all error clause:
transform_guard_test( Other, _Transforms ) ->
	ast_utils:raise_error( [ invalid_guard_test, Other ] ).





% Transforms specified guard, operating relevant AST transformations.
%
% "A guard G is a non-empty sequence of guard tests Gt_1, ..., Gt_k, and Rep(G)
% = [Rep(Gt_1), ..., Rep(Gt_k)]."
%
-spec transform_guard( ast_guard(), ast_transforms() ) -> ast_guard().
transform_guard( _GuardTests=[], _Transforms ) ->
	throw( invalid_empty_guard );

transform_guard( GuardTests, Transforms ) when is_list( GuardTests ) ->
	[ transform_guard_test( GT, Transforms ) || GT <- GuardTests ];

transform_guard( Other, _Transforms ) ->
	throw( { invalid_guard, Other } ).





% Transforms specified guard sequence, operating relevant AST transformations.
%
% "A guard sequence Gs is a sequence of guards G_1; ...; G_k, and Rep(Gs) =
% [Rep(G_1), ..., Rep(G_k)]. If the guard sequence is empty, then Rep(Gs) = []."
%
% Note: the cases where the sequence is empty is managed here as well.
%
-spec transform_guard_sequence( ast_guard_sequence(), ast_transforms() ) ->
									  ast_guard_sequence().
transform_guard_sequence( Guards, Transforms ) ->
	[ transform_guard( G, Transforms ) || G <- Guards ].
