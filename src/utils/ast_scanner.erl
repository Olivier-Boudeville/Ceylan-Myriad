

-module(ast_scanner).



-type form() :: erl_parse:abstract_form() | erl_parse:form_info().

-type ast() :: [ form() ].


-type action_trigger() :: atom().

-type transformer() :: fun().


% Describes the various actions (ex: transformations) that are to be applied to
% an AST through callbacks.
%
-type action_table() :: table:table( action_trigger(),
									 basic_utils:maybe( transformer() ) ).

-export_type([ ast/0 ]).

-export([ scan_forms/2 ]).


% Shorthands:

-type function_name() :: meta_utils:function_name().
-type function_arity() :: meta_utils:function_arity().
-type function_id() :: meta_utils:function_id().
-type function_clause() :: form().



% Implementation notes:
%
% The integral scanning of ASTs got inspirated from erl_id_trans.erl.



% Scans the specified forms.
%
-spec scan_forms( [ form() ], action_table() ) -> [ form() ].
scan_forms( Forms, ActionTable ) ->
	[ scan_form( F, ActionTable ) || F <- Forms ].



% Scans the specified form.
%
-spec scan_form( form(), action_table() ) -> form().
scan_form( F={ attribute, Line, module, Module }, _ActionTable ) ->
	F;

scan_form( F={ attribute, Line, file, { File, Line } }, _ActionTable ) ->
	F;

scan_form( _F={ attribute, Line, export, FunIds }, ActionTable ) ->
	ScannedFunIds = scan_function_ids( FunIds, ActionTable ),
	{ attribute, Line, export, ScannedFunIds };

scan_form( _F={ attribute, Line, import, { Module, FunIds }, ActionTable } ) ->
	ScannedFunIds = scan_function_ids( FunIds, ActionTable ),
	{ attribute, Line, import, { Module, ScannedFunIds } };

scan_form( _F={ attribute, Line, export_type, TypeIds }, ActionTable ) ->
	ScannedTypeIds = scan_type_ids( TypeIds, ActionTable ),
	{ attribute, Line, export_type, ScannedTypeIds };

scan_form( _F={ attribute, Line, optional_callbacks, FunIds }, ActionTable ) ->
	% Apparently not always scannable as is (try/catch could be added):
	ScannedFunIds = scan_function_ids( FunIds, ActionTable ),
	{ attribute, Line, optional_callbacks, ScannedFunIds };

scan_form( F={ attribute, Line, compile, C }, ActionTable ) ->
	F;

scan_form( _F={ attribute, Line, record, { RecordName, RecordDefs } },
		   ActionTable ) ->
	NewRecordDefs = scan_record_defs( RecordDefs, ActionTable ),
	{ attribute, Line, record, { RecordName, NewRecordDefs } };

scan_form( F={ attribute, Line, asm, { function, N, A, Code } },
		   _ActionTable ) ->
	F;

scan_form( _F={ attribute, Line, type, { TypeName, TypeDef, TypeVars } },
		   ActionTable ) ->
	ScannedTypeDef = scan_type_def( TypeDef, ActionTable ),
	ScannedTypeVars = scan_variables( TypeVars, ActionTable ),
	{ attribute, Line, type, { TypeName, ScannedTypeDef, ScannedTypeVars } };

scan_form( _F={ attribute, Line, opaque, { TypeName, TypeDef, TypeVars } },
		   ActionTable ) ->
	ScannedTypeDef = scan_type_def( TypeDef, ActionTable ),
	ScannedTypeVars = scan_variables( TypeVars, ActionTable ),
	{ attribute, Line, opaque, { TypeName, ScannedTypeDef, ScannedTypeVars } };


scan_form( _F={ attribute, Line, spec, { Fun={ FunctionName, Params },
										 FunctionTypes } }, ActionTable ) ->
	NewFunctionTypes = scan_function_types( FunctionTypes, ActionTable ),
	{ attribute, Line, spec, { Fun, NewFunctionTypes } };

scan_form( _F={ attribute, Line, spec,
				{ Fun={ ModuleName, FunctionName, Params }, FunctionTypes } },
		   ActionTable ) ->
	NewFunctionTypes = scan_function_types( FunctionTypes, ActionTable ),
	{ attribute, Line, spec, { Fun, NewFunctionTypes } };

scan_form( _F={ attribute, Line, callback, { Fun={ FunctionName, Params },
											 FunctionTypes } }, ActionTable ) ->
	NewFunctionTypes = scan_function_types( FunctionTypes, ActionTable ),
	{ attribute, Line, callback, { Fun, NewFunctionTypes } };


% Any other, general attribute:
scan_form( F={ attribute, Line, AttributeName, AttributeValue },
		   _ActionTable ) ->
	F;

% Functions:
scan_form( { function, Line, Name, Arity, Clauses }, ActionTable ) ->
	{ ScannedName, ScannedArity, ScannedClauses } = scan_function( Name, Arity,
													   Clauses, ActionTable ),
	{ function, Line, ScannedName, ScannedArity, ScannedClauses };


% Extra forms from the parser:
scan_form( F={ error, E }, _ActionTable ) ->
	F;

scan_form( F={ warning, W }, _ActionTable ) ->
	F;

scan_form( F={ eof, Line }, _ActionTable ) ->
	F.




% Function section.


-spec scan_function_ids( [ function_id() ], action_table() ) ->
							   [ function_id() ].
scan_function_ids( FunIds, ActionTable ) when is_list( FunIds ) ->
	[ scan_function_id( FunId, ActionTable ) || FunId <- FunIds ];

scan_function_ids( InvalidFunIds, _ActionTable ) ->
	throw( { invalid_function_identifiers, InvalidFunIds } ).


scan_function_id( FunId={ FunctionName, FunctionArity }, ActionTable ) ->
	check_function_name( FunctionName ),
	check_function_arity( FunctionArity ),
	FunId;

scan_function_id( InvalidFunId, _ActionTable ) ->
	throw( { invalid_function_id, InvalidFunId } ).



check_function_name( FunctionName ) when is_atom( FunctionName ) ->
	ok;

check_function_name( InvalidFunctionName ) ->
	throw( { invalid_function_name, InvalidFunctionName } ).



check_function_arity( FunctionArity ) when is_integer( FunctionArity )
										   andalso FunctionArity >= 0 ->
	FunctionArity;

check_function_arity( InvalidFunctionArity ) ->
	throw( { invalid_function_arity, InvalidFunctionArity } ).


check_function_name( FunctionName ) when is_atom( FunctionName ) ->
	ok;

check_function_name( InvalidFunctionName ) ->
	throw( { invalid_function_name, InvalidFunctionName } ).




% Type section.


-spec scan_type_ids( [ type_id() ], action_table() ) -> [ type_id() ].
scan_type_ids( FunIds, ActionTable ) when is_list( FunIds ) ->
	[ scan_type_id( FunId, ActionTable ) || FunId <- FunIds ];

scan_type_ids( InvalidFunIds, _ActionTable ) ->
	throw( { invalid_type_identifiers, InvalidFunIds } ).


scan_type_id( FunId={ TypeName, TypeArity }, ActionTable ) ->
	check_type_name( TypeName ),
	check_type_arity( TypeArity ),
	FunId;

scan_type_id( InvalidFunId, _ActionTable ) ->
	throw( { invalid_type_id, InvalidFunId } ).



check_type_name( TypeName ) when is_atom( TypeName ) ->
	ok;

check_type_name( InvalidTypeName ) ->
	throw( { invalid_type_name, InvalidTypeName } ).


check_type_arity( TypeArity ) when is_integer( TypeArity )
								   andalso TypeArity >= 0 ->
	TypeArity;

check_type_arity( InvalidTypeArity ) ->
	throw( { invalid_type_arity, InvalidTypeArity } ).



% Variable section.

-spec scan_variables( [ variable_ref() ], action_table() ) ->
							[ variable_ref() ].
scan_variables( VariableRefs, ActionTable ) ->
	[ scan_variable( VarRef, ActionTable ) || VarRef <- VariableRefs ];


-spec scan_variables( variable_ref(), action_table() ) -> variable_ref().
scan_variable( VarRef={ var, Line, VarName }, ActionTable )
  when is_atom( VarName ) ->
	F;

scan_variable( InvalidVarRef, _ActionTable ) ->
	throw( { invalid_variable_reference, InvalidVarRef } ).



% Record section.

-spec scan_record_defs( [ record_def() ], action_table() ) -> [ record_def() ].
scan_record_defs( RecordDefs, ActionTable ) ->
	[ scan_record_def( RecordDef, ActionTable ) || RecordDef <- RecordDefs ].



-spec scan_record_def( record_def(), action_table() ) -> record_def().
% With value and no type:
scan_record_def( { record_field, Line, Attr={ atom, AttrLine, AttrName },
				   AttrValue }, ActionTable ) ->
	ScannedAttrValue = scan_expression( AttrValue, ActionTable ),
	{ record_field, Line, Line, ScannedAttrValue };

% No value, no type:
scan_record_def( Def={ record_field, Line,
						_Attr={ atom, AttrLine, AttrName } }, ActionTable ) ->
	Def;

% With or without value and with type:
scan_record_def( { typed_record_field, RecordField, TypeDef }, ActionTable ) ->
	NewRecordField = scan_record_def( RecordField, ActionTable ),
	NewTypeDef = scan_type_def( TypeDef, ActionTable ),
	{ typed_record_field, NewRecordField, NewTypeDef }.



% Function section.


-spec scan_function( function_name(), function_arity(), [ function_clause() ],
					 action_table() ) ->
			  { function_name(), function_arity(), [ function_clause() ] }.
scan_function( Name, Arity, Clauses, ActionTable ) ->
	check_function_name( Name ),
	check_function_arity( Arity ),
	NewClauses = [ scan_function_clause( C, ActionTable ) || C <- Clauses ],
	{ Name, Arity, NewClauses }.


% Returns the function clause resulting from the processing of the input one.
%
-spec scan_function_clause( function_clause(), action_table() ) -> 
								  function_clause().
scan_function_clause( { clause, Line, H0,G0,B0}, ActionTable ) ->
	
