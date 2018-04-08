% Copyright (C) 2014-2018 Olivier Boudeville (olivier.boudeville@esperide.com)
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
% Creation date: Friday, December 19, 2014.



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
% except the very few bootstrapped modules* (see BOOTSTRAP_MODULES in
% GNUmakevars.inc for their actual list)
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
% used, as explained in the previous point); this is why testing a parse
% transform as a mere function called from a test case is strongly recommended


% For the module_info record:
-include("ast_info.hrl").


% For the table type:
-include("meta_utils.hrl").

% For the ast_transforms record:
-include("ast_transform.hrl").


% Local shorthands:

-type ast() :: ast_utils:ast().
-type module_info() :: ast_info:module_info().



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



% The default actual implementation to which 'table' will be wired:
%
% (a map is versatile, quite efficient, etc. - hence a good defaults):
%
-define( default_table_type, map_hashtable ).


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

	AST = ast_utils:erl_to_ast( FileToTransform ),

	%io:format( "Input AST:~n~p~n~n", [ AST ] ),

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
parse_transform( InputAST, _Options ) ->

	%io:format( "  (applying parse transform '~p')~n", [ ?MODULE ] ),

	%io:format( "Options: ~p~n", [ Options ] ),

	%io:format( "~n## INPUT ############################################~n" ),
	%io:format( "Input AST:~n~p~n~n", [ InputAST ] ),
	%ast_utils:write_ast_to_file( InputAST, "Input-AST.txt" ),

	BaseModuleInfo = ast_info:extract_module_info_from_ast( InputAST ),

	%ast_info:write_module_info_to_file( BaseModuleInfo,
	%									  "Input-module_info.txt" ),

	io:format( "Input module info: ~s~n",
			   [ ast_info:module_info_to_string( BaseModuleInfo ) ] ),

	Transforms = get_myriad_ast_transforms_for( BaseModuleInfo ),

	io:format( "~nApplying following ~s~n",
			   [ ast_transform:ast_transforms_to_string( Transforms ) ] ),

	TransformedModuleInfo = meta_utils:apply_ast_transforms( Transforms,
															 BaseModuleInfo ),

	OutputModuleInfo = TransformedModuleInfo,

	%ast_info:write_module_info_to_file( OutputModuleInfo,
	%									  "Output-module_info.txt" ),

	%io:format( "~n## OUTPUT ############################################ ~n" ),
	%io:format( "Output module info: ~s~n",
	%		   [ ast_info:module_info_to_string( OutputModuleInfo ) ] ),

	OutputAST = ast_info:recompose_ast_from_module_info( OutputModuleInfo ),

	%io:format( "~n~nOutput AST:~n~p~n", [ OutputAST ] ),

	%OutputASTFilename = text_utils:format( "Output-AST-for-module-~s.txt",
	%							   [ OutputModuleInfo#module_info.module ] ),

	%ast_utils:write_ast_to_file( OutputAST, OutputASTFilename ),

	OutputAST.




% Returns a transforms record describing the AST changes defined by this Myriad
% layer.
%
% (helper)
%
-spec get_myriad_ast_transforms_for( module_info() ) ->
										   meta_utils:ast_transforms().
get_myriad_ast_transforms_for(
  #module_info{ parse_attributes=ParseAttributes } ) ->

	% We will be replacing here all calls to the 'table' pseudo-module by calls
	% to the actual module that may be designated by a specific parse attribute,
	% otherwise by the default_table_type local macro.

	% This is just a matter of replacing 'table' (which does not exist as a
	% module) by its counterpart in elements like:
	%
	% {call,Line1,
	%             {remote,Line2,
	%                               {atom,Line3,table},
	%                               {atom,Line4,FunctionName}},
	%              ListArgs}

	% The same kind of conversion for the type specifications (ex: function
	% specs, type definitions, etc.) is done.


	% We also translate void() (which is not a builtin type) into
	% basic_utils:void(), for example:
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
	% '{remote_type,Line, [ {atom,Line,basic_utils}, {atom,Line,void}, [] ] }'

	% We also manage maybe/1 here: if used as 'maybe(T)', translated as
	% 'basic_utils:maybe(T)'.

	% Determines the target table type that we want to rely on ultimately:
	DesiredTableType = get_actual_table_type( ParseAttributes ),

	% So, regarding local types, we want to replace:
	%  - void() with basic_utils:void() (i.e. prefixed with basic_utils)
	%  - maybe(T) with basic_utils:maybe(T)
	%  - table/N (ex: table() or table(K,V)) with DesiredTableType/N (ex:
	%  DesiredTableType:DesiredTableType() or
	%  DesiredTableType:DesiredTableType(K,V))
	%  (as if table() was a local, hence builtin, type)
	%
	% Replacements to be done only for specified arities:
	% (as these substitutions overlap, a lambda function is provided)
	%
	LocalTypeTransforms = ast_transform:get_local_type_transform_table( [
				{ { void,  0 }, basic_utils },
				{ { maybe, 1 }, basic_utils },
				% First clause defined as we do not want to obtain
				% DesiredTableType:table/N, but
				% DesiredTableType:DesiredTableType/N instead:
				%
				{ { _ModuleName=table, '_' },
				  fun( _TypeName=table, _TypeArity ) ->
						  { _Module=DesiredTableType, DesiredTableType };
					 ( OtherTypeName, _TypeArity ) ->
						  { DesiredTableType, OtherTypeName }
				  end } ] ),


	% Regarding remote types, we want to replace:
	%  - table:table/N with DesiredTableType:DesiredTableType/N (N=0 or N=2)
	%  - table:T with DesiredTableType:T (ex: table:value() )
	% (as these substitutions overlap, a lambda function is provided)
	%
	RemoteTypeTransforms = ast_transform:get_remote_type_transform_table( [
				{ { table, '_', '_' },
				  fun( _ModName, _TypeName=table, _TypeArity ) ->
						  { DesiredTableType, DesiredTableType  };

					 ( _ModName, TypeName, _TypeArity ) ->
						  { DesiredTableType, TypeName }

				  end } ] ),

	% None currently used here:
	%LocalCallTransforms = meta_utils:get_local_call_transform_table( [] ),
	LocalCallTransforms = undefined,

	RemoteCallTransforms = ast_transform:get_remote_call_transform_table( [
				% For all function names and arities, the 'table' module shall
				% be replaced in remote calls by the desired table type:
				%
				{ { table, '_', '_' }, DesiredTableType } ] ),

	% Returns an overall description of these requested AST transformations:
	#ast_transforms{ local_types=LocalTypeTransforms,
					 remote_types=RemoteTypeTransforms,
					 local_calls=LocalCallTransforms,
					 remote_calls=RemoteCallTransforms }.



% Returns the name of the actual module to use for tables.
%
-spec get_actual_table_type( meta_utils:attribute_table() ) ->
								   basic_utils:module_name().
get_actual_table_type( ParseAttributeTable ) ->

	% Let's see whether a specific table_type has been specified:
	DesiredTableType = case ?table:lookupEntry( table_type,
												ParseAttributeTable ) of

		{ value, TableType } when is_atom( TableType ) ->
			ast_utils:display_info( "Default table type ('~s') overridden "
									"for this module to '~s'.~n",
									[ ?default_table_type, TableType ] ),
			TableType;

		{ value, InvalidTableType } ->
			ast_utils:raise_error( { invalid_table_type_override,
									  InvalidTableType } );

		key_not_found ->
			TableType = ?default_table_type,
			%ast_utils:display_trace( "Using default table ~p.~n",
			%				   [ TableType ] ),
			TableType

	end,

	%ast_utils:display_debug( "Will replace references to the 'table' module "
	%						  "and datatypes by references to '~s'.",
	%						  [ DesiredTableType ] ),

	DesiredTableType.
