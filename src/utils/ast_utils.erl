% Copyright (C) 2014-2018 Olivier Boudeville
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
% Creation date: Monday, January 1, 2018.



% Gathering of various convenient facilities to manage ASTs (Abstract Syntax
% Trees): direct bridge towards plain Erlang AST.
%
% Convenient to isolate processings from the current Erlang AST syntax, which
% could change over time (a bit like the erl_syntax standard module, albeit with
% a different set of conventions).
%
% See also:
%
% - the meta_utils module, for meta primitives less directly linked with syntax
%
% - the ast_scan module, to perform a full, strict traversal of an AST
%
-module(ast_utils).



% Design notes:
%
% When using the forge_*_type/N functions, type variables are expected to be
% already forged.


% Shorthands:

-type type_name()  :: type_utils:type_name().
%-type type_arity() :: type_utils:type_arity().
%-type type_id()    :: type_utils:type_id().

-type module_name() :: basic_utils:module_name().

-type function_name() :: basic_utils:function_name().


% Includes '_':
-type variable_name() :: atom().




% General element of an AST.
%
-type ast_element() :: tuple().


% Name of any parse attribute:
%
% (typically in Form={ attribute, Line, AttributeName, AttributeValue })
%
-type parse_attribute_name() :: atom().


% Line location (i.e. line number) of a form in a source file:
-type line() :: erl_anno:line().


% Line-related location in a source file (either line() or {line(), column()}):
%
-type file_loc() :: erl_anno:location().


% Context of a form:
-type form_context() :: basic_utils:maybe( line() | file_loc() ).


% Abstract form, part of an AST (ex: {attribute,40,file,{"foo.erl",40}}):
%
-type form() :: erl_parse:abstract_form() | erl_parse:form_info().


-type ast_variable() :: { 'var', line(), variable_name() }.




% Abstract Syntax Tree, standard representation of parse trees for Erlang
% programs as Erlang terms. This representation is known as the abstract format.
%
% Defined as erl_parse_tree().
%
% See also:
%
% - for the type: http://erlang.org/doc/man/erl_parse.html#type-erl_parse_tree
%
% - for the overall logic and structure:
% http://erlang.org/doc/apps/erts/absform.html
%
-type ast() :: [ form() ].



% Reference to a built-in type, in an AST.
%
% Ex:
% - {type,45,atom,[]}                       -- for atom()
% - {type,44,list,[{type,44,boolean,[]}]}   -- for [ boolean() ]
%
% Note: the order of fields matters (not arbitrary, to correspond to the actual
% AST terms)
%
-record( type, {

		   % Line of this form in the current source file:
		   line = 0 :: line(),

		   % Name of the target type:
		   name :: type_name(),

		   % Type variables, i.e. types on which this type depends:
		   type_vars = [] :: [ ast_type() ]

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
		   type_vars = [] :: [ ast_type() ]

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


% May be constrained or not (see http://erlang.org/doc/apps/erts/absform.html):
%
-type function_type().


% The description of a field of a record.
%
% Ex : {typed_record_field, {record_field,76, {atom,76,my_index}},
%               {remote_type,76, [{atom,76,linear}, {atom,76,coordinate}, []]}},
%
-type ast_field_description() :: tuple().


% The description of an immediate value in an AST, with line information.
%
% Ex: nil, in {nil,33} for [] at line #33.
%
% Note: to complete.
%
-type ast_immediate_value() :: { 'nil', line() }
							 | { 'atom', line(), atom() }
							 | { 'integer', line(), integer() }
							 | { 'float', line(), float() }.



% The description of an expression in an AST, with line information.
%
% Ex: {integer,97,2} or {match,117, {var,117,'A'}, {atom,117,foobar}}, etc.
%
-type ast_expression() :: ast_element().


-export_type([ ast_element/0, line/0, file_loc/0, form_context/0,
			   ast_builtin_type/0, ast_user_type/0, ast_remote_type/0,
			   ast_type/0, ast_field_description/0, ast_immediate_value/0 ]).


% Checking:
%
-export([ check_line/2,

		  check_parse_attribute_name/1, check_parse_attribute_name/2,

		  check_module_name/1, check_module_name/2,

		  check_inline_options/1, check_inline_options/2,

		  check_function_name/1, check_function_name/2,

		  check_type_name/1, check_type_name/2,

		  check_record_name/1, check_record_name/2,

		  check_arity/1, check_arity/2,

		  check_type_id/1, check_type_id/2,
		  check_type_ids/1, check_type_ids/2,

		  check_function_id/1, check_function_id/2,
		  check_function_ids/1, check_function_ids/2,

		  check_function_type/2, check_function_type/3,
		  check_function_types/2, check_function_types/3,

		  check_function_clauses/2, check_function_clauses/3,

		  check_variable/1, check_variable/2,
		  check_variables/1, check_variables/2,

		]).



% Designating values:
%
-export([ forge_boolean_value/1, forge_boolean_value/2,
		  forge_atom_value/1, forge_atom_value/2
		  %forge_pid_value/1, forge_pid_value/2,
		  %forge_integer_value/1, forge_integer_value/2,
		  %forge_float_value/1, forge_float_value/2,
		  %forge_tuple_value/1, forge_tuple_value/2,
		  %forge_list_value/1, forge_list_value/2
		]).


% Designating types:
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
		  forge_remote_type/4, forge_remote_type/6
		]).


% Designating calls:
%
-export([ forge_local_call/3, forge_local_call/4,
		  forge_remote_call/4, forge_remote_call/5 ]).



% Displaying:
%
-export([ display_debug/1, display_debug/2,
		  display_trace/1, display_trace/2,
		  display_info/1, display_info/2,
		  display_warning/1, display_warning/2,
		  display_error/1, display_error/2,
		  display_fatal/1, display_fatal/2 ]).


% Other:
%
-export([ raise_error/2, notify_warning/2 ]).


% Checking section.



% Checks that specified line reference is legit.
%
check_line( term(), form_context() ) ->
check_line( Line, _Context ) when is_integer( Line ) andalso Line >= 0 ->
	Line;

check_line( Other, Context ) ->
	% Not raise_error/3:
	throw( { invalid_line, Other, Context } ).



% Checks that specified module name is legit.
%
-spec check_module_name( term() ) -> basic_utils:module_name().
check_module_name( Name ) ->
	check_module_name( Name, _Context=undefined ).



% Checks that specified parse attribute name is legit.
%
-spec check_parse_attribute_name( term(), form_context() ) ->
										parse_attribute_name().
check_parse_attribute_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_parse_attribute_name( Other, Context ) ->
	raise_error( [ invalid_parse_attribute_name, Other ], Context ).


% Checks that specified parse attribute name is legit.
%
-spec check_parse_attribute_name( term() ) -> basic_utils:parse_attribute_name().
check_parse_attribute_name( Name ) ->
	check_parse_attribute_name( Name, _Context=undefined ).



% Checks that specified module name is legit.
%
-spec check_module_name( term(), form_context() ) -> basic_utils:module_name().
check_module_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_module_name( Other, Context ) ->
	raise_error( [ invalid_module_name, Other ], Context ).



% Checks that specified inline options are legit.
%
-spec check_inline_options( term() ) -> [ meta_utils:function_id() ].
check_inline_options( FunIds ) ->
	check_inline_options( FunIds, _Context=undefined ).


% Checks that specified inline options are legit.
%
-spec check_inline_options( term(), form_context() ) ->
								  [ meta_utils:function_id() ].
check_inline_options( FunIds, Context ) when is_list( FunIds ) ->
	check_function_ids( FunIds, Context );

check_inline_options( Other, Context ) ->
	raise_error( [ invalid_inline_options, Other ], Context ).




% Checks that specified function name is legit.
%
-spec check_function_name( term() ) -> basic_utils:function_name().
check_function_name( Name ) ->
	check_function_name( Name, _Context=undefined ).



% Checks that specified function name is legit.
%
-spec check_function_name( term(), form_context() ) -> basic_utils:function_name().
check_function_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_function_name( Other, Context ) ->
	raise_error( [ invalid_function_name, Other ], Context ).



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
	raise_error( [ invalid_type_name, Other ], Context ).




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
	raise_error( [ invalid_record_name, Other ], Context ).




% Checks that specified (function or type) arity is legit.
%
-spec check_arity( term() ) -> arity().
check_arity( Arity ) ->
	check_arity( Arity, _Context=undefined ).


% Checks that specified (function or type) arity is legit.
%
-spec check_arity( term(), form_context() ) -> arity().
check_arity( Arity, _Context ) when is_integer( Arity ) andalso Arity >= 0 ->
	Arity;

check_arity( Other, Context ) ->
	raise_error( [ invalid_arity, Other ], Context ).



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
	check_arity( TypeArity, Context );

check_type_id( Other, Context ) ->
	raise_error( [ invalid_type_identifier, Other ], Context ).



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
	raise_error( [ invalid_type_identifier_list, Other ], Context ).



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
	check_arity( FunctionArity, Context ),
	FunctionId;

check_function_id( Other, Context ) ->
	raise_error( [ invalid_function_identifier, Other ], Context ).



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
	raise_error( [ invalid_function_identifier_list, Other ], Context ).



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
check_function_type( _FunctionType, _FunctionArity, Context ) ->
	raise_error( [ fixme_function_type ], Context ).

check_function_type( Other, _FunctionArity, Context ) ->
	raise_error( [ invalid_function_type, Other ], Context ).



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
	raise_error( [ invalid_function_type_list, Other ], Context ).





% Checks that specified function clauses are legit.
%
-spec check_function_clauses( term(), function_arity() ) ->
									[ meta_utils:function_clause() ].
check_function_clauses( Clauses, FunctionArity ) ->
	check_function_clauses( Clauses, _Context=undefined ).


% Checks that specified function clauses are legit.
%
-spec check_function_clauses( term(), function_arity(), form_context() ) ->
									[ meta_utils:function_clause() ].
check_function_clauses( Clauses, FunctionArity, Context )
  when is_list( List ) ->
	check_arity( FunctionArity, Context ),
	Clauses;

check_function_clauses( Other, _FunctionArity, Context ) ->
	raise_error( [ invalid_function_clauses, Other ], Context ).



% Checks that specified variable is legit.
%
-spec check_variable( term() ) -> ast_variable().
check_variable( ASTVariable ) ->
	check_variable( ASTVariable, _Context=undefined ).


% Checks that specified variable is legit.
%
-spec check_variable( term(), form_context() ) ->
								 ast_variable().
check_variable( ASTVariable={ var, Line, VariableName }, Context )
  when is_atom( VariableName ) ->
	check_line( Line, Context ),
	ASTVariable;

check_variable( Other, Context ) ->
	raise_error( [ invalid_variable, Other ], Context ).



% Checks that specified variables are legit.
%
-spec check_variables( term() ) -> [ ast_variable() ].
check_variables( ASTVariables ) ->
	check_variables( ASTVariables, _Context=undefined ).


% Checks that specified variables are legit.
%
-spec check_variables( term(), form_context() ) ->
								[ ast_variable() ].
check_variables( List, Context ) when is_list( List ) ->
	[ check_variable( ASTVariable, Context ) || ASTVariable <- List ];

check_variables( Other, Context ) ->
	raise_error( [ invalid_variable_list, Other ], Context ).





% Value section.



% Returns an AST-compliant value designating specified boolean, defined at line
% #0 of the current source file.
%
% Ex: forge_boolean_value( true ) returns: {boolean,0,true}.
%
-spec forge_boolean_value( boolean() ) -> ast_element().
forge_boolean_value( BooleanValue ) ->
	forge_boolean_value( BooleanValue, _Line=0 ).


% Returns an AST-compliant value designating specified boolean, defined at
% specified line of the current source file.
%
% Ex: forge_boolean_value( false, 43 ) returns: {boolean,43,false}.
%
-spec forge_boolean_value( boolean(), line() ) -> ast_element().
forge_boolean_value( BooleanValue, Line ) ->
	{ boolean, Line, BooleanValue }.



% Returns an AST-compliant value designating specified atom, defined at line #0
% of the current source file.
%
% Ex: forge_atom_value( basic_utils ) returns: {atom,0,basic_utils}.
%
-spec forge_atom_value( atom() ) -> ast_element().
forge_atom_value( AtomValue ) ->
	forge_atom_value( AtomValue, _Line=0 ).


% Returns an AST-compliant value designating specified atom, defined at
% specified line of the current source file.
%
% Ex: forge_atom_value( basic_utils, 43 ) returns: {atom,43,basic_utils}.
%
-spec forge_atom_value( atom(), line() ) -> ast_element().
forge_atom_value( AtomValue, Line ) ->
	{ atom, Line, AtomValue }.




% Type section.


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



% Returns an AST-compliant type description for an integer, defined at line #0 of the
% current source file.
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
% Ex: to represent the following type defined at line 39: { integer(), float() },
% forge_tuple_type( 39, [ forge_integer_type(39), forge_float_type(39) ] ) returns:
% {type,39,tuple,[{type,39,integer,[]},{type,39,float,[]}]}.
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
% forge_union_type( [ forge_integer_type(39), forge_float_type(39) ], 39 ) returns:
% {type,39,union,[{type,39,integer,[]},{type,39,float,[]}]}.
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
	#type{ line=Line, name=TypeName, type_vars=TypeVars }.







% Returns an AST-compliant representation of specified local, user-defined type
% definition.
%
% Ex: to designate my_type() at line 40, forge_local_type( my_type, 40 )
% returns: {user_type,40,my_type,[]}.
%
-spec forge_local_type( type_name(), [ ast_type() ], line() ) ->
							  ast_user_type().
forge_local_type( TypeName, TypeVars, Line ) ->
	#user_type{ line=Line, name=TypeName, type_vars=TypeVars }.


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
% forge_remote_type( basic_utils, some_type, [], { 43, 44, 45 } ) - which returns:
% {remote_type,43,[{atom,44,basic_utils},{atom,45,some_type},
% [{type,43,float,[]}]]}.
%
-spec forge_remote_type( module_name(), type_name(), [ ast_type() ],
						 line(), line(), line() ) -> ast_remote_type().
forge_remote_type( ModuleName, TypeName, TypeVars, Line1, Line2, Line3 ) ->

	Spec = [ forge_atom_value( ModuleName, Line2 ),
			 forge_atom_value( TypeName, Line3 ), TypeVars ],

	#remote_type{ line=Line1, spec=Spec }.




% Returns an AST-compliant representation of specified local call.
%
% Ex: to designate 'some_fun( a, b )' at line 102, use;
% forge_local_call( some_fun, ParamDefs, 102 ) - which returns:
% {call,102,{atom,102,some_fun},[{atom,102,a},{atom,102,b}]}.
%
-spec forge_local_call( function_name(), [ ast_expression() ], line() ) ->
							  ast_expression().
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
	{ call, Line1, forge_atom_value( FunctionName, Line2 ), Params }.




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
	{ call, Line1, { remote, Line2, forge_atom_value( ModuleName, Line2 ),
					 forge_atom_value( FunctionName, Line2 ) },
					 Params }.




% Subsection for trace outputs that are specific to parse-transforms.


% Displays specified text as debug.
%
-spec display_debug( text_utils:string() ) -> basic_utils:void().
display_debug( String ) ->
	io:format( "[debug] ~s~n", [ String ] ).


% Displays specified text as debug.
%
-spec display_debug( text_utils:format_string(), [ term() ] ) ->
						  basic_utils:void().
display_debug( FormatString, Values ) ->
	display_debug( io_lib:format( FormatString, Values ) ).



% Displays specified text as trace.
%
-spec display_trace( text_utils:string() ) -> basic_utils:void().
display_trace( String ) ->
	io:format( "[trace] ~s~n", [ String ] ).


% Displays specified text as trace.
%
-spec display_trace( text_utils:format_string(), [ term() ] ) ->
						  basic_utils:void().
display_trace( FormatString, Values ) ->
	display_trace( io_lib:format( FormatString, Values ) ).



% Displays specified text as info.
%
-spec display_info( text_utils:string() ) -> basic_utils:void().
display_info( String ) ->
	io:format( "[info] ~s~n", [ String ] ).


% Displays specified text as info.
%
-spec display_info( text_utils:format_string(), [ term() ] ) ->
						  basic_utils:void().
display_info( FormatString, Values ) ->
	display_info( io_lib:format( FormatString, Values ) ).


% Displays specified text as warning.
%
-spec display_warning( text_utils:string() ) -> basic_utils:void().
display_warning( String ) ->
	io:format( "[warning] ~s~n", [ String ] ).


% Displays specified text as warning.
%
-spec display_warning( text_utils:format_string(), [ term() ] ) ->
						  basic_utils:void().
display_warning( FormatString, Values ) ->
	display_warning( io_lib:format( FormatString, Values ) ).



% Displays specified text as error.
%
-spec display_error( text_utils:string() ) -> basic_utils:void().
display_error( String ) ->
	io:format( "[error] ~s~n", [ String ] ).


% Displays specified text as error.
%
-spec display_error( text_utils:format_string(), [ term() ] ) ->
						  basic_utils:void().
display_error( FormatString, Values ) ->
	display_error( io_lib:format( FormatString, Values ) ).



% Displays specified text as fatal.
%
-spec display_fatal( text_utils:string() ) -> basic_utils:void().
display_fatal( String ) ->
	io:format( "[fatal] ~s~n", [ String ] ).


% Displays specified text as fatal.
%
-spec display_fatal( text_utils:format_string(), [ term() ] ) ->
						  basic_utils:void().
display_fatal( FormatString, Values ) ->
	display_fatal( io_lib:format( FormatString, Values ) ).






% Raises an error, with specified context.
%
% Ex: raise_error( [ invalid_module_name, Other ], _Context=112 ) shall
% result in throwing { invalid_module_name, Other, { line, 112 } }.
%
-spec raise_error( [ term() ], form_context() ) -> basic_utils:void().
raise_error( Elements, Context ) ->

	AllElements = get_elements_with_context( Elements, Context ),

	throw( list_to_tuple( AllElements ) ).



% Notifies a warning, with specified context.
%
-spec notify_warning( [ term() ], form_context() ) -> basic_utils:void().
notify_warning( Elements, Context ) ->

	AllElements = get_elements_with_context( Elements, Context ),

	display_warning( "~p", [ AllElements ] ).



% Returns error/warning elements including specified context.
%
% (helper)
%
-spec get_elements_with_context( [ term() ], form_context() ) -> [ term() ].
get_elements_with_context( Elements, _Context=undefined ) ->
	Elements;

get_elements_with_context( Elements, _Context={ FilePath, Line } )
  when is_list( FilePath ) andalso is_integer( Line ) ->
	Elements ++ [ { file, FilePath }, { line, Line } ];

get_elements_with_context( Elements, _Context=Line ) when is_integer( Line ) ->
	Elements ++ [ { line, Line } ];

get_elements_with_context( Elements, _Context=FilePath )
  when is_list( FilePath ) ->
			Elements ++ [ { file, FilePath } ];

get_elements_with_context( Elements, Context ) ->
	% No list_utils module used from this module:
	Elements ++ [ Context ].
