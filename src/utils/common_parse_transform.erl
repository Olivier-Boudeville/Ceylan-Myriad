% Copyright (C) 2014-2017 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
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



% Overall parse transform for the common layer.
%
% See meta_utils.erl and meta_utils_test.erl.
%
-module(common_parse_transform).


-type ast() :: meta_utils:ast().



% Implementation notes:
%
% Currently, the 'common' parse transform is in charge of replacing any call to
% the pseudo-module 'table' (a module that does not exist) into a call to the
% default hashtable type we currently use (ex: we have hashtable,
% lazy_hashtable, tracked_hashtable, map_hashtable, etc.) - unless it is
% specifically overridden in the transformed module.


% The default actual implementation that 'table' will be wired to:
-define( default_hashtable_type, map_hashtable ).


-export([ run_standalone/1, parse_transform/2 ]).



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
	% are probably not all set, but it is unlikely to be a problem.
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
	% to the actual module designated by the default_hashtable_type local macro.

	% This is just a matter of replacing 'table' by its counterpart in elements
	% like:
	%
	% {call,Line1,
	%             {remote,Line2,
	%                               {atom,Line3,table},
	%                               {atom,Line4,FunctionName}},
	%              ListArgs }

	% The same kind of conversion for the type specifications (ex: function
	% specs, type definitions, etc.) is done.

	%io:format( "######################################################~n" ),
	%io:format( "Input AST:~n~p~n~n", [ AST ] ),

	TableAST = replace_table( AST ),

	ModuleInfo = meta_utils:extract_module_info_from_ast( TableAST ),

	% Other transformations may take place here.

	% Uncomment with care: must ultimately depend only on non-bootstrapped
	% modules (like {meta,text}_utils):
	%
	%io:format( meta_utils:module_info_to_string( ModuleInfo ) ),

	%io:format( "Module info: ~p~n", [ ModuleInfo ] ),

	OutputAST = meta_utils:recompose_ast_from_module_info( ModuleInfo ),

	%io:format( "~n######################################################~n" ),
	%io:format( "~n~nOutput AST:~n~p~n", [ OutputAST ] ),

	OutputAST.



% Replaces calls to the table pseudo-module by actual calls to the
% default_hashtable_type one.
%
% We preserve element order.
%
-spec replace_table( ast() ) -> ast().
replace_table( AST ) ->

	% The Ln variables designate line numbers.

	DesiredTableType = case lookup_table_select_attribute( AST ) of

		undefined ->
			?default_hashtable_type;

		TableType ->
			%io:format( "Default hashtable type overridden "
			%		  "to ~p.~n", [ TableType ] ),
			TableType

	end,

	%io:format( "Replacing calls to 'table' by calls to '~s':~n",
	%		   [ DesiredTableType ] ),

	% This is why the meta_utils module must be bootstrapped:
	%
	{ NewAST, _DesiredTableType } = meta_utils:traverse_term(
									  _TargetTerm=AST,
									  _TypeDescription=tuple,
									  _TermTransformer=fun switch_table/2,
									  _UserData=DesiredTableType ),

	NewAST.



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
% '-table_type( list_hashtable ).'.
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

lookup_table_select_attribute( _AST=[ _H | T ], Found ) ->
	lookup_table_select_attribute( T, Found ).
