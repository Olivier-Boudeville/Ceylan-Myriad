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



% Module in charge of handling records defined or used within an AST.
%
% See http://erlang.org/doc/apps/erts/absform.html for more information.
%
-module(ast_record).



% Note that records have to be managed differently, depending on various
% contexts, i.e. if being:
%
% - defined (then in a top-level form)
% - or found in:
%    * a pattern
%    * a guard test
%    * an expression
%    * a type
%
% To manage these last four cases, we used to rely on higher-functions for
% transformations, in which a transform fun (ex:
% ast_expression:transform_expression/2) was provided as a context-dependent
% parameter (see transform_record_field_definitions/2 and all).
%
% Now, we prefer to rely on a more flexible per-context version of the
% transformation, located in the module corresponding to the context (ex:
% ast_expression), and only the transformation of the record definition is
% managed here.


% Shorthands:

-type line() :: ast_base:line().
-type form() :: ast_base:form().
-type located_form() :: ast_info:located_form().

-type ast_element() :: ast_base:ast_element().
-type ast_transforms() :: ast_transform:ast_transforms().

-type record_name() :: basic_utils:record_name().
-type field_name() :: basic_utils:field_name().

-type ast_type() :: ast_type:ast_type().
-type maybe_ast_type() :: ast_type:maybe_ast_type().

-type maybe_ast_immediate_value() :: ast_value:maybe_ast_immediate_value().

-type record_table() :: ast_info:record_table().
-type record_definition() :: ast_info:record_definition().
-type field_table() :: ast_info:field_table().



% Defines the field of a type of record (type and default value, if
% specified).
%
-type field_definition() :: { maybe_ast_type(), maybe_ast_immediate_value() }.


% Field identifier, possibly '_':
-type field_id() :: field_name() | '_'.


-type ast_field_id() :: ast_base:ast_atom().


% AST definition for a field of a record, when creating it.
%
-type ast_record_field_definition() ::
		ast_record_field_definition( ast_base:ast_element() ).



% AST definition for a field of a record, when creating it.
%
% {record_field,LINE,Rep(Field_1),Rep(ValueType)}
%
-type ast_untyped_record_field_definition( ValueType ) ::
		{ 'record_field', line(), ast_field_id(), ValueType }.

-type ast_typed_record_field_definition( ValueType ) ::
		{ 'typed_record_field', line(),
		  ast_untyped_record_field_definition( ValueType ),
		  ast_type() }.


-type ast_record_field_definition( ValueType ) ::
		ast_untyped_record_field_definition( ValueType )
	  | ast_typed_record_field_definition( ValueType ).


% Typically used in ast_pattern:
-type ast_pattern_field() :: { 'record_field', line(),
							   ast_field_id() | { 'var', line(), '_' },
							   ast_pattern:ast_pattern() }.



-export_type([ field_definition/0, field_id/0, ast_field_id/0,
			   ast_untyped_record_field_definition/1,
			   ast_typed_record_field_definition/1,
			   ast_record_field_definition/0, ast_record_field_definition/1,
			   ast_pattern_field/0 ]).


-export([ transform_record_definitions/2,
		  transform_record_field_definitions/2,
		  transform_record_field_definition/2,
		  get_located_forms_for/1 ]).


% For the table macro:
-include("meta_utils.hrl").


% Often names (ex: of a record, or a field) are transmitted as parameters
% whereas they are usually not necessary, yet it is useful at least for error
% reporting.



% Transforms the specified record definitions (ex: coming for the 'records'
% field of a module_info record), according to specified transforms.
%
-spec transform_record_definitions( ast_info:record_table(),
				   ast_transforms() ) -> ast_info:record_table().
transform_record_definitions( RecordTable, Transforms ) ->

	% { RecordName, RecordDefinition } pairs:
	RecordPairs = ?table:enumerate( RecordTable ),

	NewRecordPairs = [ transform_record_definition( RecordName,
									RecordDefinition, Transforms )
					   || { RecordName, RecordDefinition } <- RecordPairs ],

	?table:new( NewRecordPairs ).



% Transforms the specified record definition.
%
-spec transform_record_definition( record_name(), record_definition(),
			   ast_transforms() ) -> { record_name(), record_definition() }.
transform_record_definition( RecordName,
				  _RecordDefinition={ FieldTable, Loc, Line }, Transforms ) ->

	% { FieldName, FieldDefinition } pairs:
	FieldPairs = ?table:enumerate( FieldTable ),

	NewFieldPairs = [ transform_field_definition( FieldName, FieldDefinition,
												  Transforms )
					  || { FieldName, FieldDefinition } <- FieldPairs ],

	NewFieldTable = ?table:new( NewFieldPairs ),

	NewRecordDefinition = { NewFieldTable, Loc, Line },

	{ RecordName, NewRecordDefinition }.



% Transforms the specified field definition.
%
-spec transform_field_definition( field_name(), field_definition(),
		ast_transforms() ) -> { field_name(), field_definition() }.
transform_field_definition( FieldName,
							_FieldDescription={ FieldType, FieldDefaultValue },
							Transforms ) ->

	NewFieldType = transform_field_definition_type( FieldType, Transforms ),

	NewFieldDefaultValue= transform_field_definition_default_value(
							FieldDefaultValue, Transforms ),

	NewFieldDescription = { NewFieldType, NewFieldDefaultValue },

	{ FieldName, NewFieldDescription }.



% Transforms the specified field type.
%
-spec transform_field_definition_type( maybe_ast_type(), ast_transforms() ) ->
											 maybe_ast_type().
transform_field_definition_type( _FieldType=undefined, _Transforms ) ->
	undefined;

transform_field_definition_type( FieldType, Transforms ) ->
	ast_type:transform_type( FieldType, Transforms ).



% Transforms the specified field default value.
%
-spec transform_field_definition_default_value( maybe_ast_immediate_value(),
							  ast_transforms() ) -> maybe_ast_immediate_value().
transform_field_definition_default_value( _FieldDefaultValue=undefined,
										  _Transforms ) ->
	undefined;

transform_field_definition_default_value( FieldDefaultValue, Transforms ) ->
	ast_value:transform_value( FieldDefaultValue, Transforms ).



% Transforms specified record fields, at creation, applying to each record field
% the specified function to perform the relevant transformations (that depends
% on the context; ex: if being in a guard, in an expression, etc.).
%
% (counterpart of record_inits/1 in erl_id_trans)
%
-spec transform_record_field_definitions( [ ast_record_field_definition() ],
			ast_transforms() ) -> [ ast_record_field_definition() ].
transform_record_field_definitions( RecordFields, Transforms ) ->
	[ transform_record_field_definition( RF, Transforms )
	  || RF <- RecordFields ].



% Transforms specified record field definition.
%
% Ex: {record_field,LINE,Rep(Field_k),Rep(Gt_k)}.
%
-spec transform_record_field_definition( ast_record_field_definition(),
			   ast_transforms() ) -> ast_record_field_definition().
% With a value and no type specified here:
transform_record_field_definition(
  _RF={ 'record_field', Line, ASTFieldName, ASTValue }, Transforms ) ->

	NewASTFieldName = transform_record_field_name( ASTFieldName, Transforms ),

	NewASTValue = ast_value:transform_value( ASTValue, Transforms ),

	{ record_field, Line, NewASTFieldName, NewASTValue };


% With no value and no type specified here:
transform_record_field_definition( _RF={ 'record_field', Line, ASTFieldName },
								   Transforms ) ->

	NewASTFieldName = transform_record_field_name( ASTFieldName, Transforms ),

	{ record_field, Line, NewASTFieldName };


% With a value and a type specified here:
transform_record_field_definition(
  _RF={ 'typed_record_field', { 'record_field', Line, ASTFieldName, ASTValue },
		ASTType }, Transforms ) ->

	NewASTFieldName = transform_record_field_name( ASTFieldName, Transforms ),

	NewASTValue = ast_value:transform_value( ASTValue, Transforms ),

	NewASTType = ast_type:transform_type( ASTType, Transforms ),

	{ typed_record_field, { record_field, Line, NewASTFieldName, NewASTValue },
	  NewASTType };


% With no value and a type specified here:
transform_record_field_definition(
  _RF={ 'typed_record_field', { 'record_field', Line, ASTFieldName }, ASTType },
  Transforms ) ->

	NewASTFieldName = transform_record_field_name( ASTFieldName, Transforms ),

	NewASTType = ast_type:transform_type( ASTType, Transforms ),

	{ typed_record_field, { record_field, Line, NewASTFieldName }, NewASTType }.





% Transforms the name of the specified field.
%
-spec transform_record_field_name( ast_element(), ast_transforms() ) ->
										 ast_element().
transform_record_field_name( ASTFieldName, Transforms ) ->

	% Note: field names are full expressions here, but only atoms are allowed
	% by the parser (dixit the id parse transform).
	% So we could expect to have ASTFieldName={atom,Line,Value} here.

	%ast_type:check_ast_atom( ASTFieldName, Line ),
	%NewASTFieldName = TransformFun( ASTFieldName, Transforms ),

	ast_expression:transform_expression( ASTFieldName, Transforms ).




% Returns located forms corresponding to specified record table.
%
-spec get_located_forms_for( record_table() ) -> [ located_form() ].
get_located_forms_for( RecordTable ) ->

	RecordPairs = ?table:enumerate( RecordTable ),

	lists:foldl( fun( { RecordName, RecordDef }, Acc ) ->
						 [ get_located_form_for_record( RecordName, RecordDef )
						   | Acc ]
				 end,
				 _Acc0=[],
				 _List=RecordPairs ).



% Returns a located form corresponding to specified record.
%
-spec get_located_form_for_record( record_name(), record_definition() ) ->
										 located_form().
get_located_form_for_record( RecordName,
							 _RecordDef={ FieldTable, Loc, Line } ) ->

	FieldDefs = recompose_field_definitions( FieldTable, Line ),

	Form = { attribute, Line, record, { RecordName, FieldDefs } },

	{ Loc, Form }.



% Recomposes the forms corresponding to the specified record fields.
%
-spec recompose_field_definitions( field_table(), line() ) -> [ form() ].
recompose_field_definitions( FieldTable, Line ) ->

	FieldPairs = ?table:enumerate( FieldTable ),

	[ recompose_field_definition( FieldName, FieldDef, Line )
	  || { FieldName, FieldDef } <- FieldPairs ].



% Recomposes the form corresponding to the specified record field.
%
-spec recompose_field_definition( field_name(), field_definition(), line() ) ->
										form().
recompose_field_definition( FieldName,
		_FieldDef={ _MaybeASTType=undefined, _MaybeASTDefaultValue=undefined },
		Line ) ->
	{ record_field, Line, { atom, Line, FieldName } };

recompose_field_definition( FieldName,
		_FieldDef={ _MaybeASTType=undefined, ASTDefaultValue },
		Line ) ->
	{ record_field, Line, { atom, Line, FieldName }, ASTDefaultValue };

recompose_field_definition( FieldName,
		_FieldDef={ ASTType, _MaybeASTDefaultValue=undefined },
		Line ) ->
	{ typed_record_field, { record_field, Line, { atom, Line, FieldName } },
	  ASTType };

recompose_field_definition( FieldName, _FieldDef={ ASTType, ASTDefaultValue },
							Line ) ->
	{ typed_record_field,
	  { record_field, Line, { atom, Line, FieldName }, ASTDefaultValue },
	  ASTType }.
