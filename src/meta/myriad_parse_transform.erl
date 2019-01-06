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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Friday, December 19, 2014.



% Overall parse transform for the 'Ceylan-Myriad' layer.
%
% See meta_utils.erl and meta_utils_test.erl.
%
-module(myriad_parse_transform).



% PRELIMINARY IMPORTANT NOTES REGARDING THE ART OF WRITING PARSE TRANSFORMS
%
%
% - they are by nature difficult to debug; instead of running them "as are"
% (then with little information returned in case of error), run them explicitly,
% typically from a test case; for that, one may refer to:
%
%    * src/utils/myriad_parse_transform_test.erl for that test
%
%    * src/data-management/simple_parse_transform_target.erl for the test module
%    the previous test is to apply to
%
% So the typical recommended workflow is to run repeatedly 'make
% myriad_parse_transform_run' (provided the compilation of the bootstrap modules
% and of myriad_parse_transform_test.beam still succeeds!) and perform
% modifications onto simple_parse_transform_target.erl.
%
% - this particular parse transform applies at the level of the Myriad Layer,
% and as such *cannot use any module of that layer except the very few
% bootstrapped modules* (see BOOTSTRAP_MODULES in GNUmakevars.inc for their
% actual list)
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

% To ease troubleshooting: enable the writing to file of the input and output
% ASTs, and compare them (ex: 'meld Myriad-input-AST-sorted.txt
% Myriad-output-AST-sorted.txt').


% For the module_info record:
-include("ast_info.hrl").


% For the table type:
-include("meta_utils.hrl").

% For the ast_transforms record:
-include("ast_transform.hrl").


% Local shorthands:

-type ast() :: ast_base:ast().
-type module_info() :: ast_info:module_info().



% Implementation notes:
%
% Currently, the 'myriad' parse transform is in charge of:
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


-export([ run_standalone/1, parse_transform/2, apply_myriad_transform/2,
		  transform_module_info/1 ]).



% Runs the Myriad parse transform defined here in a standalone way (i.e. without
% being triggered by the usual, integrated compile process).
%
% This allows to benefit from all compilation error and warning messages,
% whereas they are seldom available from a code directly run as a parse
% transform (ex: 'undefined parse transform 'foobar'' as soon as a function or a
% module is not found).
%
-spec run_standalone( file_utils:file_name() ) -> { ast(), module_info() }.
run_standalone( FileToTransform ) ->

	AST = ast_utils:erl_to_ast( FileToTransform ),

	% Options like : [ report_warnings, {d,debug_mode}, beam,
	% report_errors, {cwd,"X"}, {outdir,Y"}, {i,"A"},{i,"B"}, debug_info, etc.
	% are probably not all set, but it is unlikely to be a problem here.
	%
	% (anyway, for example defining a non-exported function in the target module
	% leads to a "unused function" warning)
	%
	apply_myriad_transform( AST, _Options=[] ).



% The parse transform itself, transforming the specified (Myriad-based) Abstract
% Format code into another (Erlang-compliant) one.
%
% Note: the (compile) Options variable is currently ignored, as we do not know
% what we could do with it. There is nevertheless valuable information in it,
% like in:
%
% Options = [report_warnings, {d,debug_mode}, beam, report_errors,
%			{cwd,"[...]/foo"}, {outdir,"[...]/foo"}, {i,"[...]/foo/../bar"},
%           [...]
%			{parse_transform,myriad_parse_transform}, debug_info,
%           warnings_as_errors, warn_unused_import, warn_obsolete_guards,
%           warn_shadow_vars, warn_export_vars, warn_export_all,
%           encrypt_debug_info, {debug_info_key,"Ceylan-Myriad"} ]
%
% Notably, short of managing specifically 'debug_info' et al., apparently in the
% resulting BEAM files there is no Core Erlang code (see the output of 'make
% generate-local-plt' for more information, like: 'Could not get Core Erlang
% code for: foo/baz.beam; Recompile with +debug_info or analyze starting from
% source code').
%
-spec parse_transform( ast(), meta_utils:parse_transform_options() ) -> ast().
parse_transform( InputAST, Options ) ->

	%ast_utils:display_debug( "Options: ~p~n", [ Options ] ),

	% In the context of this direct parse transform, the module_info is of no
	% use afterwards and thus can be dropped:
	%
	{ MyriadAST, _MyriadModuleInfo } =
		apply_myriad_transform( InputAST, Options ),

	MyriadAST.



% Defined to be reused in multiple contexts.
%
-spec apply_myriad_transform( ast(), meta_utils:parse_transform_options() ) ->
									{ ast(), module_info() }.
apply_myriad_transform( InputAST, Options ) ->

	%ast_utils:display_debug( "applying parse transform '~p'",
	%                         [ ?MODULE ] ),

	%ast_utils:display_debug(
	%           "~n## INPUT ####################################" ),

	%ast_utils:display_debug( "Myriad input AST:~n~p~n~n", [ InputAST ] ),

	%ast_utils:write_ast_to_file( InputAST, "Myriad-input-AST.txt" ),

	% This allows to compare input and output ASTs more easily:
	%ast_utils:write_ast_to_file( lists:sort( InputAST ),
	%							 "Myriad-input-AST-sorted.txt" ),

	BaseModuleInfo = ast_info:extract_module_info_from_ast( InputAST ),

	WithOptsModuleInfo = ast_info:interpret_options( Options, BaseModuleInfo ),

	%ast_info:write_module_info_to_file( WithOptsModuleInfo,
	%									  "Input-module_info.txt" ),

	%ast_utils:display_debug( "Input module info: ~s~n~n",
	%		   [ ast_info:module_info_to_string( WithOptsModuleInfo ) ] ),

	% Currently the resulting transforms are not kept:
	{ TransformedModuleInfo, _ModuleTransforms } =
		transform_module_info( WithOptsModuleInfo ),


	%ast_info:write_module_info_to_file( TransformedModuleInfo,
	%									"Output-module_info.txt" ),

	%trace_utils:debug( "~n## OUTPUT ###################################" ),
	%ast_utils:display_debug( "Output module info: ~s",
	%		   [ ast_info:module_info_to_string( TransformedModuleInfo ) ] ),
	%ast_utils:display_debug( "Output module info: ~s~n~n",
	%		   [ ast_info:module_info_to_string( TransformedModuleInfo ) ] ),

	OutputAST = ast_info:recompose_ast_from_module_info(
				  TransformedModuleInfo ),

	%ast_utils:display_debug( "~n~nMyriad output AST:~n~p~n", [ OutputAST ] ),
	%ast_utils:display_debug( "Myriad output AST:~n~p~n~n", [ OutputAST ] ),

	%OutputASTFilename = text_utils:format(
	%			"Myriad-output-AST-for-module-~s.txt",
	%			[ element( 1, TransformedModuleInfo#module_info.module ) ] ),

	%ast_utils:write_ast_to_file( OutputAST, OutputASTFilename ),

	%ast_utils:write_ast_to_file( lists:sort( OutputAST ),
	%							 "Myriad-output-AST-sorted.txt" ),

	{ OutputAST, TransformedModuleInfo }.



% Transforms (at the Myriad level) specified module information.
%
-spec transform_module_info( module_info() ) ->
					   { module_info(), ast_transform:ast_transforms() }.
transform_module_info( ModuleInfo ) when is_record( ModuleInfo, module_info ) ->

	% First determines the right transforms:
	Transforms = get_myriad_ast_transforms_for( ModuleInfo ),

	% Then apply them:

	%ast_utils:display_debug( "~nApplying following ~s",
	%		   [ ast_transform:ast_transforms_to_string( Transforms ) ] ),

	% Returns updated transforms and module information:
	meta_utils:apply_ast_transforms( ModuleInfo, Transforms ).



% Returns a transforms record describing the AST changes defined by this Myriad
% layer.
%
% (helper)
%
-spec get_myriad_ast_transforms_for( module_info() ) ->
										   ast_transform:ast_transforms().
get_myriad_ast_transforms_for(
  #module_info{ compilation_options=CompileOptTable,
				parse_attributes=ParseAttributes } ) ->

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
				  fun( _TypeName=table, _TypeArity, TransfoState ) ->
						  TypeId = { _Module=DesiredTableType,
									 DesiredTableType },
						  { TypeId, TransfoState };

					 ( OtherTypeName, _TypeArity, TransfoState ) ->
						  TypeId = { DesiredTableType, OtherTypeName },
						  { TypeId, TransfoState }

				  end
				} ] ),


	% Regarding remote types, we want to replace:
	%  - table:table/N with DesiredTableType:DesiredTableType/N (N=0 or N=2)
	%  - table:T with DesiredTableType:T (ex: table:value() )
	% (as these substitutions overlap, a lambda function is provided)
	%
	RemoteTypeTransforms = ast_transform:get_remote_type_transform_table( [
				{ { table, '_', '_' },
				  fun( _ModName, _TypeName=table, _TypeArity, TransfoState ) ->
						  TypeId = { DesiredTableType, DesiredTableType  },
						  { TypeId, TransfoState };

					 ( _ModName, TypeName, _TypeArity, TransfoState ) ->
						  TypeId = { DesiredTableType, TypeName },
						  { TypeId, TransfoState }

				  end } ] ),

	% None currently used here:
	%LocalCallTransforms = meta_utils:get_local_call_transform_table( [] ),
	LocalCallTransforms = undefined,

	% We used to define a simple, direct transformation from 'table' to
	% DesiredTableType, however the addition of the cond_utils support led to
	% have to define a full-blown call transform fun (to perform a more radical
	% transformation), instead of a mere mapping and also instead of a
	% remote_call_replacement fun/4, which would not be able to take into the
	% value of arguments (ex: the specified token), since being just being
	% parametrised by an arity.
	%
	% So (anonymous mute variables corresponding to line numbers):

	RemoteCallTransformFun = fun

		% Calls to cond_utils:if_defined( Token, Exprs ) shall be replaced
		% either by specified expressions or by nothing:
		%
		( _LineCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
						 {atom,_,if_defined} },
		  _Params=[ {atom,_,Token}, ExprFormList ],
		  Transforms=#ast_transforms{
			transformation_state=TokenTable } ) ->

			case ?table:lookupEntry( Token, TokenTable ) of

				% Any value associated to this token will do, as we want just to
				% detect whether it is defined at all:
				%
				{ value, _Any } ->
					% The corresponding, specified code (expressions) is thus
					% enabled; the AST sees them as the elements of a list
					% ({cond,_,E1, {cond,_,E2,...}), whereas we need a direct list
					% here (i.e. [E1, E2, ...]), so:
					%
					Exprs = ast_generation:form_to_list( ExprFormList ),

					% So that the call as a whole is replaced with a series of
					% expressions:
					%
					{ Exprs, Transforms };

				key_not_found ->
					% Token not defined, so expressions are skipped:
					{ _Exprs=[], Transforms }

			end;


		% For all function names and arities, the 'table' module shall be
		% replaced in remote calls by the desired table type:
		%
		( LineCall,
		  _FunctionRef={ remote, Line1, {atom,Line2,table}, FunNameForm },
		  Params,
		  Transforms ) ->
			  % Just swap the 'table' module with the desired one:
			  NewFunctionRef = { remote, Line1, {atom,Line2,DesiredTableType},
								 FunNameForm },
			  NewExpr = { call, LineCall, NewFunctionRef, Params },

			 { [ NewExpr ], Transforms };


		% Other calls shall go through:
		( LineCall, FunctionRef, Params, Transforms ) ->
			OriginalExpr = { call, LineCall, FunctionRef, Params },
			{ [ OriginalExpr ], Transforms }


	end,

	TransformTable = ?table:new(
						[ { _Trigger=call, RemoteCallTransformFun } ] ),

	% Finally, we want to read any tokens specified by the user in order to
	% drive the activation of conditional code:
	%
	TokenTable = cond_utils:get_token_table_from( CompileOptTable ),

	%trace_utils:debug_fmt( "Token table:~n~p", [ TokenTable ] ),

	% Returns an overall description of these requested AST transformations:
	#ast_transforms{ local_types=LocalTypeTransforms,
					 remote_types=RemoteTypeTransforms,
					 local_calls=LocalCallTransforms,
					 %Not useful anymore: remote_calls=RemoteCallTransforms,
					 transform_table=TransformTable,
					 transformation_state=TokenTable } .



% Returns the name of the actual module to use for tables.
%
-spec get_actual_table_type( ast_info:attribute_table() ) ->
								   basic_utils:module_name().
get_actual_table_type( ParseAttributeTable ) ->

	% Let's see whether a specific table_type has been specified:
	DesiredTableType = case ?table:lookupEntry( table_type,
												ParseAttributeTable ) of

		{ value, { TableType, _LocForm } } when is_atom( TableType ) ->
			ast_utils:display_info( "Default table type ('~s') overridden "
									"for this module to '~s'.~n",
									[ ?default_table_type, TableType ] ),
			TableType;

		{ value, { InvalidTableType, _LocForm } } ->
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
