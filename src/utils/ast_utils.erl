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
% Creation date: Monday, January 1, 2018



% Gathering of various convenient facilities to manage ASTs (Abstract Syntax Trees).
%
% Convenient to isolate processings from the current Erlang AST syntax, which
% could change over time (a bit like the erl_syntax standard module, albeit with
% a different set of conventions).
%
% See also: the meta_utils module, for meta primitives less directly linked with syntax.
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



% General element of an AST.
%
-type ast_element() :: tuple().


% Line location (i.e. line number) of a form in a source file:
-type line() :: erl_anno:line().


% Line-related location in a source file (either line() or {line(), column()}):
%
-type file_loc() :: erl_anno:location().




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
% Ex: {remote_type,43,[{atom,43,basic_utils},{atom,43,maybe},[{type,43,float,[]}]]}
%            -- for basic_utils:maybe( float() )
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


-export_type([ ast_element/0, line/0, file_loc/0,
			   ast_builtin_type/0, ast_user_type/0, ast_remote_type/0,
			   ast_type/0, ast_immediate_value/0 ]).


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
% forge_remote_type( basic_utils, some_type, [], { 43, 44, 45 } ); returns:
% {remote_type,43,[{atom,44,basic_utils},{atom,45,some_type},
% [{type,43,float,[]}]]}
%
-spec forge_remote_type( module_name(), type_name(), [ ast_type() ],
						 line(), line(), line() ) -> ast_remote_type().
forge_remote_type( ModuleName, TypeName, TypeVars, Line1, Line2, Line3 ) ->

	Spec = [ forge_atom_value( ModuleName, Line2 ),
			 forge_atom_value( TypeName, Line3 ), TypeVars ],

	#remote_type{ line=Line1, spec=Spec }.
