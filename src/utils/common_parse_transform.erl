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
% Creation date: Friday, December 19, 2014



% Overall parse transform for the Common (a.k.a. Ceylan-Myriad) layer.
%
% See meta_utils.erl and meta_utils_test.erl.
%
-module(common_parse_transform).



% PRELIMINARY IMPORTANT NOTES REGARDING THE ART OF WRITING PARSE TRANSFORMS
%
%
% - they are by nature difficult to debug; instead of running them "as are"
% (then with little information returned in case of error), run them explicitly,
% typically from a test case; for that, one may refer to:
%
%    * src/utils/common_parse_transform_test.erl for that test
%
%    * src/data-management/simple_parse_transform_target.erl for the test module
%    the previous test is to apply to
%
% So the typical recommended workflow is to run repeatedly 'make
% common_parse_transform_run' (provided the compilation of the bootstrap modules
% and of common_parse_transform_test.beam still succeeds!) and perform
% modifications onto simple_parse_transform_target.erl.
%
% - this particular parse transform applies at the level of the Common Layer
% (a.k.a. Ceylan-Myriad), and as such *cannot use any module of that layer
% except the very few bootstrapped modules* - which are typically
% {meta,text}_utils and map_hashtable (see BOOTSTRAP_MODULES in GNUmakevars.inc
% for their actual list)
%
%     Indeed, the other modules (of Ceylan-Myriad) are not bootstrapped, so they
%     can enjoy the services offered by this parse transform, but of course they
%     thus require this transform to be compiled in order to be themselves
%     compiled; as a consequence, that parse transform may not use them at
%     execution-time (by design they cannot be compiled yet), so many facilities
%     are out of reach for this very particular, base parse transform
%
%
% - when a parse transform fails, one will not get much more than: '{"init
% terminating in do_boot",error_reported}' (even when using
% crashdump_viewer:start/0 on the resulting erl_crash.dump file); your best
% friend will thus be io:format/2 (as more advanced solutions usually cannot be
% used, as explained in the previous point)


% For the module_info record:
-include("meta_utils.hrl").



% Shorthands:

-type ast() :: meta_utils:ast().
-type module_info() :: meta_utils:module_info().
-type located_form() :: meta_utils:located_form().



% Implementation notes:
%
% Currently, the 'common' parse transform is in charge of:
%
% - replacing all calls and type specifications referring to the pseudo-module
% 'table' (a module that does not exist) into counterparts referring to the
% default, actual type of associative table that we currently use instead (ex:
% we have hashtable, lazy_hashtable, tracked_hashtable, map_hashtable, etc.) -
% unless the table type to be used is explicitly specified in the target,
% transformed module
%
%     As a result, one's code source may include 'MyTable = table:new(), ...' or
%     '-type my_type() :: [ { float(), table() } ].' and have them correctly
%     translated
%
%
% - replacing, in type specifications, any mention to a pseudo-builtin,
% pseudo-type void() by its actual definition, which is basic_utils:void()
% (ultimately: any(), simply)
%
%     As a result, one's code source may include '-spec f( boolean() ) ->
%     void().' and have it accepted by the compiler (and void() is now a
%     reserved, "builtin" type)



% This module, being the parse transformer, is not parse-transformed, as a
% consequence it cannot use the 'table' pseudo-module.
%
% Module name for the tables used in this module:
%
-define( table, map_hashtable ).


% The default actual implementation to which 'table' will be wired (versatile,
% quite efficient, etc. - i.e. a good defaults):
%
-define( default_table_type, map_hashtable ).


-export([ run_standalone/1, parse_transform/2 ]).


% Obsolete counterpart functions that operate directly on ASTs:
%-export([ replace_table_ast/1 ]).

% Silencing:
-export([ switch_table/2, lookup_table_select_attribute/1, replace_table/1,
		  expand_void_type/1 ]).


% Runs the parse transform defined here in a standalone way (i.e. without being
% triggered by the usual, integrated compile process).
%
% This allows to benefit from all compilation error and warning messages,
% whereas they are seldom available from a code run as a parse transform (ex:
% 'undefined parse transform 'foobar'' as soon as a function or a module is not
% found).
%
-spec run_standalone( file_utils:file_name() ) -> ast().
run_standalone( FileToTransform ) ->

	AST = meta_utils:erl_to_ast( FileToTransform ),

	% Options like : [ report_warnings, {d,debug_mode_is_enabled}, beam,
	% report_errors, {cwd,"X"}, {outdir,Y"}, {i,"A"},{i,"B"}, debug_info, etc.
	% are probably not all set, but it is unlikely to be a problem here.
	%
	% (anyway, for example defining a non-exported function in the target module
	% leads to a "unused function" warning)
	%
	parse_transform( AST, _Options=[] ).



% The parse transform itself, transforming the specified Abstract Format code
% into another one.
%
-spec parse_transform( ast(), meta_utils:parse_transform_options() ) -> ast().
parse_transform( AST, _Options ) ->

	%io:format( "  (applying parse transform '~p')~n", [ ?MODULE ] ),

	%io:format( "Options: ~p~n", [ Options ] ),

	% We will be replacing here all calls to the 'table' pseudo-module by calls
	% to the actual module designated by the default_table_type local macro.

	% This is just a matter of replacing 'table' by its counterpart in elements
	% like:
	%
	% {call,Line1,
	%             {remote,Line2,
	%                               {atom,Line3,table},
	%                               {atom,Line4,FunctionName}},
	%              ListArgs}

	% The same kind of conversion for the type specifications (ex: function
	% specs, type definitions, etc.) is done.


	% We also translate void() into basic_utils:void(), for example:
	%
	% {attribute,Line1,spec,
	%       { {FunctionName,Arity},
	%         [ {type,Line2,'fun',
	%                [{type,Line3,product,[]},
	%                 {user_type,Line4,void,[]}]}]}},
	%
	% into:
	%
	% {attribute,Line1,spec,
	%       { {FunctionName,Arity},
	%         [ {type,Line2,'fun',
	%                [{type,Line3,product,[]},
	%                 {remote_type,Line4,
	%                              [{atom,Line4,basic_utils},
	%                               {atom,Line4,void},
	%                               []]}]}]}},
	%
	% which means that, in a spec, any term in the form of
	% '{user_type,Line,void,[]}' shall be replaced with:
	% '{remote_type,Line, [{atom,Line,basic_utils}, {atom,Line,void}, [] ] }'


	%io:format( "~n## INPUT ############################################~n" ),
	%io:format( "Input AST:~n~p~n~n", [ AST ] ),
	%meta_utils:write_ast_to_file( AST, "Input-AST.txt" ),

	% Will fail (a bit silently) if the AST cannot be successfully linted:
	BaseModuleInfo = meta_utils:extract_module_info_from_ast( AST ),

	%meta_utils:write_module_info_to_file( BaseModuleInfo,
	%									  "Input-module_info.txt" ),

	%io:format( "Input module info: ~s~n",
	%		   [ meta_utils:module_info_to_string( BaseModuleInfo ) ] ),


	DesiredTableType = get_actual_table_type(
						 BaseModuleInfo#module_info.parse_attributes ),

	% Regarding local types, we want to replace:
	%  - void() with basic_utils:void() (i.e. prefix with basic_utils)
	%  - maybe(T) with basic_utils:maybe(T)
	%  - table/N (ex: table() or table(K,V)) with DesiredTableType/N (ex:
	%  DesiredTableType:DesiredTableType() or
	%  DesiredTableType:DesiredTableType(K,V))
	%
	% Just for specified arities:
	%
	LocalTypeReplacements = meta_utils:get_local_type_replacement_table( [
				{ { void,  0 }, basic_utils },
				{ { maybe, 1 }, basic_utils },
				% First clause as we do not want DesiredTableType:table/N, but
				% DesiredTableType:DesiredTableType/N:
				{ { table, '_' },
				  fun( _TypeName=table, _TypeArity ) ->
						  { DesiredTableType, DesiredTableType };
					 ( TypeName, _TypeArity ) ->
						  { DesiredTableType, TypeName }
				  end } ] ),


	% Regarding remote types, we want to replace:
	%      * table:table/N with DesiredTableType:DesiredTableType/N (N=0 or N=2)
	%      * table:T with DesiredTableType:T (ex: table:value() )
	% (as these substitutions overlap, a lambda function is provided)
	RemoteTypeReplacements = meta_utils:get_remote_type_replacement_table( [
				{ { table, '_', '_' },
				  fun( _ModuleName, _TypeName=table, _TypeArity ) ->
						  { DesiredTableType, DesiredTableType  };

					 ( _ModuleName, TypeName, _TypeArity ) ->
						  { DesiredTableType, TypeName }

				  end } ] ),

	% First update the type definitions accordingly:
	[ NewTypeDefs, NewRecordDefs ]  =
		[ meta_utils:replace_types_in( Defs, LocalTypeReplacements,
									   RemoteTypeReplacements )
		  || Defs <- [ BaseModuleInfo#module_info.type_definition_defs,
					   BaseModuleInfo#module_info.record_defs ] ],

	TypedFunctionTable = meta_utils:update_types_in_functions(
						 BaseModuleInfo#module_info.functions,
						 LocalTypeReplacements, RemoteTypeReplacements ),

	% None currently used here:
	LocalCallReplacements = meta_utils:get_local_call_replacement_table( [] ),

	RemoteCallReplacements = meta_utils:get_remote_call_replacement_table( [
				% For all function name and arities, the 'table' module shall be
				% replaced in remote calls by the desired table type:
				%
				{ { table, '_', '_' }, DesiredTableType } ] ),

	CallFunctionTable = meta_utils:update_calls_in_functions(
						  TypedFunctionTable, LocalCallReplacements,
						  RemoteCallReplacements ),

	%TableModuleInfo = replace_table( BaseModuleInfo ),
	%VoidModuleInfo = expand_void_type( TableModuleInfo ),
	% Other transformations may take place here.

	% Apply transformations:
	OutputModuleInfo = BaseModuleInfo#module_info{
						 type_definition_defs=NewTypeDefs,
						 record_defs=NewRecordDefs,
						 functions=CallFunctionTable },


	%meta_utils:write_module_info_to_file( OutputModuleInfo,
	%									  "Output-module_info.txt" ),

	%io:format( "~n## OUTPUT ############################################ ~n" ),
	%io:format( "Output module info: ~s~n",
	%		   [ meta_utils:module_info_to_string( OutputModuleInfo ) ] ),

	OutputAST = meta_utils:recompose_ast_from_module_info( OutputModuleInfo ),

	io:format( "~n~nOutput AST:~n~p~n", [ OutputAST ] ),
	%meta_utils:write_ast_to_file( OutputAST, "Output-AST.txt" ),

	OutputAST.



% Returns the name of the actual module to use for tables.
%
-spec get_actual_table_type( meta_utils:attribute_table() ) ->
								   basic_utils:module_name().
get_actual_table_type( ParseAttributeTable ) ->

	% Let's see whether a specific table_type has been specified:
	DesiredTableType = case ?table:lookupEntry( table_type,
												ParseAttributeTable ) of

		{ value, TableType } ->
			meta_utils:display_info( "Default table type overridden to ~p.~n",
									 [ TableType ] ),
			TableType;

		key_not_found ->
			TableType = ?default_table_type,
			%meta_utils:display_trace( "Using default table ~p.~n",
			%				   [ TableType ] ),
			TableType

	end,

	%meta_utils:display_debug( "Will replace references to the 'table' module "
	%						  "and datatypes by references to '~s'.",
	%						  [ DesiredTableType ] ),

	DesiredTableType.



% Replaces the pseudo-type table and its related elements by its actual
% implementation, as specified in DesiredTableType.
%
% Was previously an anonymous function with a closure; might be now more
% efficient.
%
% Returns { NewTerm, DesiredTableType }.
%
% In this clause we rewrite the calls to the table module itself:
%
switch_table( _Term={ call, L1, { remote, L2,
									{ atom, Line3, table },
									{ atom, Line4, FunctionName } },
					  ListArgs },
			  DesiredTableType ) ->

	%io:format( " - transforming table:~s call at line ~B~n",
	%			  [ FunctionName, Line3 ] ),

	NewTerm = { call, L1, { remote, L2,
							  { atom, Line3, DesiredTableType },
							  { atom, Line4, FunctionName } },
				ListArgs },

	{ NewTerm, DesiredTableType };


% We want to convert the type 'table:X' where X can be 'table', but also 'key',
% 'value', etc. into 'DesiredTableType:X':
%
% (here, a simple, not polymorphic type is referenced)
%
switch_table( { remote_type, L1, [
						{ atom, L2, table },
						{ atom, L3, TypeName }, DependentTypeList ] },
			  DesiredTableType ) ->

	%io:format( " - transforming table spec at line ~B~n", [ L2 ] ),

	% We have in this context to translate a table type (i.e. a type apparently
	% defined in the table pseudo-module) into the actual type of interest,
	% while the other types (ex: table:key/0) must only have their origin module
	% changed:
	%
	ActualTypeName = case TypeName of

		table ->
			DesiredTableType;

		_ ->
			TypeName

	end,

	% Here polymorphic types may be referenced, ex:
	%
	% DependentTypeList = [
	%    {type,162,arity,[]},
	%    {remote_type,162,[{atom,162,meta_utils},{atom,162,function_info},[]]} ]
	%
	% Parametric types may use 'table' as well:
	NewDependentTypeList = [
		begin
			{ NewTermType, _DesiredTableType } = switch_table( Type,
														DesiredTableType ),
			NewTermType

		end || Type <- DependentTypeList ],

	NewTerm = { remote_type, L1, [
					{ atom, L2, DesiredTableType },
					{ atom, L3, ActualTypeName }, NewDependentTypeList ] },

	{ NewTerm, DesiredTableType };


% Other terms are unchanged:
switch_table( Term, DesiredTableType ) ->
	{ Term, DesiredTableType }.




% Returns any module-level explicit replacement for the table pseudo-type,
% specified thanks to a '-table_type( my_type ).' attribute, with, for example:
% '-table_type( list_table ).'.
%
% Only searches through top-level attributes as intended, and checks that the
% table type is defined up to once only.
%
lookup_table_select_attribute( AST ) ->
	lookup_table_select_attribute( AST, _Found=undefined ).


lookup_table_select_attribute( _AST=[], Found ) ->
	Found;

lookup_table_select_attribute( _AST=[ { attribute, _L, table_type, Type } | T ],
							   _Found=undefined ) ->
	lookup_table_select_attribute( T, Type );

% Here Found has already been defined:
lookup_table_select_attribute(
  _AST=[ { attribute, L, table_type, AType } | _T ], Found ) ->
	meta_utils:raise_error( { table_type_defined_more_than_once, { line, L },
							  Found, AType } );

% AST element not of interest here:
lookup_table_select_attribute( _AST=[ _H | T ], Found ) ->
	lookup_table_select_attribute( T, Found ).



% Replaces calls to the 'table' pseudo-module by actual calls to the
% default_table_type one, and do so also for the type specifications.
%
% We preserve element order.
%
% (module-info version)
%
-spec replace_table( module_info() ) -> module_info().
replace_table( ModuleInfo=#module_info{ parse_attributes=ParseAttributeTable,
										type_definition_defs=TypeActualDefs,
										functions=FunctionTable } ) ->

	% We have here to possibly update all type specifications and all function
	% declarations.

	% Let's see whether a specific table_type has been specified:
	DesiredTableType = case ?table:lookupEntry( table_type,
												ParseAttributeTable ) of

		{ value, TableType } ->
			io:format( "Default table type overridden to ~p.~n",
						[ TableType ] ),
			TableType;

		key_not_found ->
			TableType = ?default_table_type,
			io:format( "Using default table ~p.~n", [ TableType ] ),
			TableType

	end,

	io:format( "Replacing calls to 'table' by calls to '~s':~n",
			   [ DesiredTableType ] ),

	% Let's first update the actual type definitions, which are available here
	% as a list of located forms:
	%
	NewTypeActualDefs = process_type_defs_for_table( TypeActualDefs ),

	% Now updating in turn the functions (specs and definitions):
	NewFunctionTable = FunctionTable,

	ModuleInfo#module_info{ type_definition_defs=NewTypeActualDefs,
							functions=NewFunctionTable }.



% Processes the type definitions regarding the 'table' pseudotype.
%
-spec process_type_defs_for_table( [ located_form() ] ) -> [ located_form() ].
process_type_defs_for_table( LocatedForms ) ->
	process_type_defs_for_table( LocatedForms, _Acc=[] ).


process_type_defs_for_table( _LocatedForms=[], Acc ) ->
	% Located forms, hence their order does not matter:
	Acc;

process_type_defs_for_table( _LocatedForms=[ { Loc, Form } | T ], Acc ) ->
	io:format( "Processing form in ~p:~n~p~n", [ Loc, Form ] ),
	NewForm = Form,
	process_type_defs_for_table( T, [ { Loc, NewForm } | Acc ] ).







% Translates void() into basic_utils:void(), in function specifications.
%
-spec expand_void_type( ast() ) -> ast().
expand_void_type( AST ) ->
	%io:format( "Expanding the void() pseudo-datatype.~n" ),

	% '{user_type,Line,void,[]}' shall be replaced with:
	%
	%  {remote_type,Line, [{atom,Line,basic_utils}, {atom,Line,void}, [] ] }

	AST.
