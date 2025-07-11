% Copyright (C) 2014-2025 Olivier Boudeville
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

-module(myriad_parse_transform).

-moduledoc """
Overall **parse transform for the `Ceylan-Myriad` layer**.

See `meta_utils.erl` and `meta_utils_test.erl`.
""".



% PRELIMINARY IMPORTANT NOTES REGARDING THE ART OF WRITING PARSE TRANSFORMS
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
%   Indeed, the other modules (of Ceylan-Myriad) are not bootstrapped, so they
%   can enjoy the services offered by this parse transform, but of course they
%   thus require this transform to be compiled in order to be themselves
%   compiled; as a consequence, that parse transform may not use them at
%   execution-time (by design they cannot be compiled yet), so many facilities
%   are out of reach for this very particular, base parse transform
%
%
% - when a parse transform fails, one will not get much more than: '{"init
% terminating in do_boot",error_reported}' (even when using
% crashdump_viewer:start/0 on the resulting erl_crash.dump file); your best
% friend will thus be io:format/2 (as more advanced solutions usually cannot be
% used, as explained in the previous point); this is why testing a parse
% transform as a mere function called from a test case is strongly recommended

% To ease troubleshooting: enable the writing to file of the input and output
% ASTs, and compare them (e.g. 'meld Myriad-input-AST-sorted.txt
% Myriad-output-AST-sorted.txt').


% For the module_info record:
-include("ast_info.hrl").


% For the table type:
-include("meta_utils.hrl").

% For the ast_transforms record:
-include("ast_transform.hrl").


% Local type shorthands:

-type format_string() :: text_utils:format_string().

-type file_name() :: file_utils:file_name().

-type ast() :: ast_base:ast().
-type file_loc() :: ast_base:file_loc().
-type form() :: ast_base:form().

-type module_info() :: ast_info:module_info().
-type module_name() :: basic_utils:module_name().
-type ast_expression() :: ast_expression:ast_expression().
-type ast_clause() :: ast_clause:ast_clause().

-type ast_transforms() :: ast_transform:ast_transforms().

-type ast_transform_table() :: ast_transform:ast_transform_table().

-type local_call_transform_table() ::
		ast_transform:local_call_transform_table().

-type remote_call_transform_table() ::
		ast_transform:remote_call_transform_table().

-type parse_transform_options() :: meta_utils:parse_transform_options().



% Implementation notes:
%
% Currently, the 'myriad' parse transform is in charge of:
%
% - replacing all calls and type specifications referring to the 'table'
% pseudo-type (either itself prefixed with the 'table' module - that does not
% exist, or as a local pseudo-type) into counterparts referring to the default,
% actual type of associative table that we currently use instead (e.g. we have
% hashtable, lazy_hashtable, tracked_hashtable, map_hashtable, etc.) - unless
% the table type to be used is explicitly specified in the target, transformed
% module
%
%     As a result, one's code source may include 'MyTable = table:new(), ...' or
%     '-type my_type() :: [{float(), table()}].' and have them correctly
%     translated
%
% - replacing, in type specifications, any mention to a pseudo-builtin,
% pseudo-types like void() by their actual definition (e.g. type_utils:void())
% (ultimately: any(), simply)
%
%     As a result, one's code source may include '-spec f(boolean( ) -> void().'
%     and have it accepted by the compiler (and void() is now a reserved,
%     "builtin" type)


% Errors raised by a failing compilation are apparently reported (1) one by one
% (i.e. only one at each compilation) and (2) from the last one in the sources
% to the first one; this does not seem related to Myriad but to the (current)
% Erlang compiler (tested with 24.0), yet using directly erlc does not exhibit
% that (possibly that this relates to its support of parse transform then).


% The default actual implementation to which 'table' will be wired:
%
% (a map is versatile, quite efficient, etc. - hence a good default):
%
-define( default_table_type, map_hashtable ).


-export([ run_standalone/1, run_standalone/2,
		  parse_transform/2, apply_myriad_transform/2,
		  transform_module_info/1 ]).


% Silencing:
-export([ handle_formatting_call_3p/5 ]).



-doc """
Runs the Myriad parse transform defined here in a standalone way (that is
without being triggered by the usual, integrated compile process), with no
specific option.

This allows to benefit from all compilation error and warning messages, whereas
they are seldom available from a code directly run as a parse transform
(e.g. `undefined parse transform 'foobar'` as soon as a function or a module is
not found).
""".
-spec run_standalone( file_name() ) -> { ast(), module_info() }.
run_standalone( FileToTransform ) ->
	run_standalone( FileToTransform, _Options=[] ).



-doc """
Runs the Myriad parse transform defined here in a standalone way (that is
without being triggered by the usual, integrated compile process), with the
specified options.

This allows to benefit from all compilation error and warning messages, whereas
they are seldom available from a code directly run as a parse transform
(e.g. 'undefined parse transform 'foobar'' as soon as a function or a module is
not found).
""".
-spec run_standalone( file_name(), parse_transform_options() ) ->
										{ ast(), module_info() }.
run_standalone( FileToTransform, Options ) ->
	AST = ast_utils:erl_to_ast( FileToTransform ),

	% Options like : report_warnings, {d,myriad_debug_mode}, beam,
	% report_errors, {cwd,"X"}, {outdir,Y"}, {i,"A"},{i,"B"}, debug_info, etc.
	% may probably not all set, but it is unlikely to be a problem here.
	%
	% (anyway, for example defining a non-exported function in the target module
	% leads to a "unused function" warning)
	%
	apply_myriad_transform( AST, Options ).



-doc """
The parse transform itself, transforming the specified (Myriad-based) Abstract
Format code into another (Erlang-compliant) one.

Note: the (compile) Options variable is currently ignored, as we do not know
what we could do with it. There is nevertheless valuable information in it, like
in:
```
Options = [report_warnings, {d,myriad_debug_mode}, beam, report_errors,
		  {cwd,"[...]/foo"}, {outdir,"[...]/foo"}, {i,"[...]/foo/../bar"},
		  [...]
		  {parse_transform,myriad_parse_transform}, debug_info,
		  warnings_as_errors, warn_unused_import, warn_obsolete_guards,
		  warn_shadow_vars, warn_export_vars, warn_export_all,
		  encrypt_debug_info, {debug_info_key,"Ceylan-Myriad"} ]
```

Notably, short of managing specifically `debug_info` et al., apparently in the
resulting BEAM files there is no Core Erlang code (see the output of `make
generate-local-plt` for more information, like: `Could not get Core Erlang code
for: foo/baz.beam; Recompile with +debug_info or analyze starting from source
code`).
""".
-spec parse_transform( ast(), parse_transform_options() ) -> ast().
parse_transform( InputAST, Options ) ->

	%ast_utils:display_debug( "Options: ~p~n", [ Options ] ),

	% In the context of this direct parse transform, the module_info is of no
	% use afterwards and thus can be dropped:
	%
	{ MyriadAST, _MyriadModuleInfo } =
		apply_myriad_transform( InputAST, Options ),

	%ast_utils:display_debug( "MyriadAST: ~p~n", [ MyriadAST ] ),
	%ast_utils:display_debug( "MyriadModuleInfo: ~p~n", [ MyriadModuleInfo ] ),

	MyriadAST.



-doc """
Applies the Myriad parse-transform.

Defined to be reused in multiple contexts.
""".
-spec apply_myriad_transform( ast(), parse_transform_options() ) ->
									{ ast(), module_info() }.
apply_myriad_transform( InputAST, Options ) ->

	% If uncommenting this trace and not seeing it in the console, check that a
	% myriad_parse_transform.beam file is not eclipsing from ebin any proper
	% one:
	%
	%ast_utils:display_debug( "  (applying parse transform '~p')",
	%                         [ ?MODULE ] ),

	%ast_utils:display_debug(
	%           "~n## INPUT ####################################" ),

	%ast_utils:display_debug( "Myriad input AST:~n~p~n~n", [ InputAST ] ),

	%ast_utils:display_debug( "Myriad options:~n~p~n", [ Options ] ),

	%ast_utils:write_ast_to_file( InputAST, "Myriad-input-AST.txt" ),

	% This allows to compare input and output ASTs more easily:
	% (most useful input option)
	%ast_utils:write_ast_to_file( lists:sort( InputAST ),
	%                             "Myriad-input-AST-sorted.txt" ),

	%ast_utils:display_debug( "Code path: ~p", [ code:get_path() ] ),

	BaseModuleInfo = ast_info:extract_module_info_from_ast( InputAST ),

	WithOptsModuleInfo = ast_info:interpret_options( Options, BaseModuleInfo ),

	%ast_utils:display_debug( "Compilation options are: ~ts.",
	%   [ ast_info:compilation_options_to_string(
	%       _CompileTable=WithOptsModuleInfo#module_info.compilation_options,
	%       _CompOptDefs=WithOptsModuleInfo#module_info.compilation_option_defs,
	%     _DoIncludeForms=false ) ] ),

	%ast_info:write_module_info_to_file( WithOptsModuleInfo,
	%                                    "Input-module_info.txt" ),

	%ast_utils:display_debug( "Input module info: ~ts~n~n",
	%      [ ast_info:module_info_to_string( WithOptsModuleInfo ) ] ),

	% Currently the resulting transforms are not kept:
	{ TransformedModuleInfo, _ModuleTransforms } =
		transform_module_info( WithOptsModuleInfo ),


	%ast_info:write_module_info_to_file( TransformedModuleInfo,
	%                                    "Output-module_info.txt" ),

	%ast_utils:display_debug( "~n## OUTPUT #################################" ),
	%ast_utils:display_debug( "Output module info: ~ts",
	%       [ ast_info:module_info_to_string( TransformedModuleInfo ) ] ),
	%ast_utils:display_debug( "Output module info: ~ts~n~n",
	%       [ ast_info:module_info_to_string( TransformedModuleInfo ) ] ),

	OutputAST =
		ast_info:recompose_ast_from_module_info( TransformedModuleInfo ),

	%ast_utils:display_debug( "~n~nMyriad output AST:~n~p~n", [ OutputAST ] ),

	%OutputASTFilename = io_lib:format(
	%    "Myriad-output-AST-for-module-~ts.txt",
	%    [ element( 1, TransformedModuleInfo#module_info.module ) ] ),

	%ast_utils:write_ast_to_file( OutputAST, OutputASTFilename ),

	%ast_utils:write_ast_to_file( lists:sort( OutputAST ),
	%                             "Myriad-output-AST-sorted.txt" ),

	{ OutputAST, TransformedModuleInfo }.



-doc "Transforms (at the Myriad level) the specified module information.".
-spec transform_module_info( module_info() ) ->
								{ module_info(), ast_transforms() }.
transform_module_info( ModuleInfo ) when is_record( ModuleInfo, module_info ) ->

	?display_debug( "[Myriad] Transforming module information." ),

	% First determines the right transforms:
	Transforms = get_myriad_ast_transforms_for( ModuleInfo ),

	% Then apply them:

	%ast_utils:display_debug( "~nApplying following ~ts",
	%      [ ast_transform:ast_transforms_to_string( Transforms ) ] ),

	% Returns updated transforms and module information:
	meta_utils:apply_ast_transforms( ModuleInfo, Transforms ).



-doc """
Returns a transforms record describing the AST changes defined by this Myriad
layer.
""".
-spec get_myriad_ast_transforms_for( module_info() ) -> ast_transforms().
get_myriad_ast_transforms_for( #module_info{
									module=ModuleEntry,
									compilation_options=CompileOptTable,
									parse_attributes=ParseAttributes } ) ->

	% We will be replacing here all calls to the 'table' pseudo-module by calls
	% to the actual module that may be designated by a specific parse attribute,
	% otherwise by the default_table_type local macro.

	% This is just a matter of replacing 'table' (which does not exist as a
	% module) by its counterpart in elements like:
	%
	% {call,FileLoc1,
	%             {remote,FileLoc2,
	%                               {atom,FileLoc3,table},
	%                               {atom,FileLoc4,FunctionName}},
	%              ListArgs}

	% The same kind of conversion for the type specifications (e.g. function
	% specs, type definitions, etc.) is done.


	% We also translate types like void() (which are not builtin types) into
	% for example type_utils:void():
	%
	% {attribute,FileLoc1,spec,
	%       { {FunctionName,Arity},
	%         [ {type,FileLoc2,'fun',
	%                [{type,FileLoc3,product,[]},
	%                 {user_type,FileLoc4,void,[]}]}]}},
	%
	% into:
	%
	% {attribute,FileLoc1,spec,
	%       { {FunctionName,Arity},
	%         [ {type,FileLoc2,'fun',
	%                [{type,FileLoc3,product,[]},
	%                 {remote_type,FileLoc4,
	%                              [{atom,FileLoc4,type_utils},
	%                               {atom,FileLoc4,void},
	%                               []]}]}]}},
	%
	% which means that, in a spec, any term in the form of
	% '{user_type,FileLoc,void,[]}' shall be replaced with:
	% '{remote_type,FileLoc, [ {atom,FileLoc,type_utils},
	%                          {atom,FileLoc,void}, [] ] }'

	% We also manage option/1 here: if used as 'option(T)', translated as
	% 'type_utils:option(T)'; the same applies to safe_option/1, fallible/{1,2}
	% and diagnosed_fallible/{1,2}.

	% Determines the target table type that we want to rely on ultimately:
	DesiredTableType = get_actual_table_type( ParseAttributes ),

	LocalTypeTransformTable = get_local_type_transforms( DesiredTableType ),
	RemoteTypeTransformTable = get_remote_type_transforms( DesiredTableType ),

	LocalCallTransformTable = get_local_call_transforms(),
	RemoteCallTransformTable = get_remote_call_transforms(),

	DisableLCO = shall_lco_be_disabled( CompileOptTable ),

	TargetModuleName = case ModuleEntry of

		undefined ->
			undefined;

		{ ModName, _ModLocForm } ->
			ModName

	end,

	% Too serious consequences not to be advertised; not using display_warning/2
	% anymore, as a warning may trigger error-management mechanisms (e.g. with
	% Emacs preventing a compilation buffer to be buried):
	%
	DisableLCO andalso ast_utils:display_info(
		"LCO disabled for this '~ts' module.", [ TargetModuleName ] ),

	ASTTransformTable = get_ast_global_transforms( DesiredTableType,
		_DisableLCO=shall_lco_be_disabled( CompileOptTable ) ),

	% Finally, we want to read any tokens specified by the user in order to
	% drive the activation of conditional code:
	%
	TokenTable = cond_utils:get_token_table_from( CompileOptTable ),

	% Uncomment to see all known tokens:
	%ast_utils:display_debug( "Token table:~n~ts",
	%                         [ ?table:to_string( TokenTable ) ] ),

	% Returns an overall description of these requested AST transformations:
	#ast_transforms{ local_types=LocalTypeTransformTable,
					 remote_types=RemoteTypeTransformTable,
					 local_calls=LocalCallTransformTable,
					 remote_calls=RemoteCallTransformTable,
					 transformed_module_name=TargetModuleName,
					 transform_table=ASTTransformTable,
					 transformation_state=TokenTable } .



-doc "Returns the name of the actual module to use for tables.".
-spec get_actual_table_type( ast_info:attribute_table() ) -> module_name().
get_actual_table_type( ParseAttributeTable ) ->

	% Let's see whether a specific table_type has been specified:
	DesiredTableType = case ?table:lookup_entry( table_type,
												 ParseAttributeTable ) of

		{ value, { TableType, _LocForm } } when is_atom( TableType ) ->
			ast_utils:display_info( "Default table type ('~ts') overridden "
				"for this module to '~ts'.~n",
				[ ?default_table_type, TableType ] ),
			TableType;

		{ value, { InvalidTableType, _LocForm } } ->
			ast_utils:raise_error(
				{ invalid_table_type_override, InvalidTableType } );

		key_not_found ->
			TableType = ?default_table_type,
			%?display_debug( "Using default table ~p.~n",
			%                [ TableType ] ),
			TableType

	end,

	%ast_utils:display_debug( "Will replace references to the 'table' module "
	%  "and datatypes by references to '~ts'.", [ DesiredTableType ] ),

	DesiredTableType.



-doc "Determines whether the disabling of Last Call Optimisation is requested.".
shall_lco_be_disabled( CompileOptTable ) ->

	DebugDefines = ?table:get_value_with_default( _DefinesK='d', _Default=[],
												  CompileOptTable ),

	lists:member( myriad_disable_lco, DebugDefines ).



-doc """
Returns the table specifying the transformation of the local types.

Regarding local types, we want to replace:

- `void()` with `type_utils:void()` (i.e. prefixed with `type_utils`)

- `option(T)` with `type_utils:option(T)`

- `safe_option(T)` with `type_utils:safe_option(T)`

- `fallible(T)` with `basic_utils:fallible(T)`

- `fallible(TSuccess, TFailure)` with `basic_utils:fallible(TSuccess, TFailure)`

- `diagnosed_fallible(TSuccess, TFailure)` with
  `basic_utils:diagnosed_fallible(TSuccess, TFailure)`

- `table/N` (e.g. `table()` or `table(K,V)`) with `DesiredTableType/N` (e.g.
`DesiredTableType:DesiredTableType()` or
`DesiredTableType:DesiredTableType(K,V)`) (as if `table()` was a builtin type)
""".
-spec get_local_type_transforms( module_name() ) ->
									ast_transform:local_type_transform_table().
get_local_type_transforms( DesiredTableType ) ->

	% Replacements to be done only for the specified arities, here to be found
	% in the basic_utils module:
	%
	BasicUtilsTypes = [ { fallible, 1 },
						{ fallible, 2 },
						{ diagnosed_fallible, 1 },
						{ diagnosed_fallible, 2 } ],

	% Same regarding the type_utils module:
	TypeUtilsTypes = [ { void, 0 },
					   { option, 1 },
					   { safe_option, 1 } ],


	BaseReplacements = [ { T, basic_utils } || T <- BasicUtilsTypes ]
		++ [ { T, type_utils } || T <- TypeUtilsTypes ],

	ast_transform:get_local_type_transform_table( BaseReplacements ++ [

		% A transformation function is needed to discriminate correctly between
		% the cases: the first clause is defined as we do not want to obtain
		% DesiredTableType:table/N, but DesiredTableType:DesiredTableType/N
		% instead.
		%
		{ { _ModuleName=table, '_' },
			fun( _TypeName=table, _TypeArity, TransfoState ) ->
				TypeId = { _Module=DesiredTableType, DesiredTableType },
				{ TypeId, TransfoState };

				( OtherTypeName, _TypeArity, TransfoState ) ->
				TypeId = { DesiredTableType, OtherTypeName },
				{ TypeId, TransfoState }

			end
		} ] ).



-doc """
Returns the table specifying the transformation of the remote types.

Regarding remote types, we want to replace:
 - `table:table/N` with `DesiredTableType:DesiredTableType/N` (N=0 or N=2)
 - `table:T` with `DesiredTableType:T` (e.g. `table:value()`)

(as these substitutions overlap, a lambda function is provided)
""".
-spec get_remote_type_transforms( module_name() ) ->
									ast_transform:remote_type_transform_table().
get_remote_type_transforms( DesiredTableType ) ->
	ast_transform:get_remote_type_transform_table( [
		{ { table, '_', '_' },
			fun( _ModName, _TypeName=table, _TypeArity, TransfoState ) ->
				TypeId = { DesiredTableType, DesiredTableType  },
				{ TypeId, TransfoState };

			   ( _ModName, TypeName, _TypeArity, TransfoState ) ->
				TypeId = { DesiredTableType, TypeName },
				{ TypeId, TransfoState }

			end
		} ] ).



-doc "Returns the table specifying the transformation of the local calls.".
% None currently used here:
-spec get_local_call_transforms() -> local_call_transform_table().
get_local_call_transforms() ->
	%meta_utils:get_local_call_transform_table( [] ),
	undefined.



-doc """
Returns the table specifying the transformation of the remote calls (see next
`get_ast_global_transforms/2`).
""".
% None used anymore, superseded by a more powerful AST transform table.
-spec get_remote_call_transforms() -> remote_call_transform_table().
get_remote_call_transforms() ->
	undefined.



-doc """
Returns the table specifying the global transformations to be done on an AST.

If LCO is requested to be disabled, a corresponding (function definition-level)
transformation will be registered.

We used to define a simple, direct transformation from 'table' to
DesiredTableType, however the addition of the cond_utils support led to have to
define a full-blown call transform fun (to perform a more radical
transformation), instead of a mere mapping, and also instead of a
`remote_call_replacement_fun/4` - which would not be able to take into account
the value of arguments (e.g. the specified token), since being just being
parametrised by an arity.
""".
-spec get_ast_global_transforms( module_name(), boolean() ) ->
										ast_transform_table().
get_ast_global_transforms( DesiredTableType, DisableLCO ) ->

	% Anonymous mute variables corresponding to in-file locations:
	RemoteCallTransformFun = fun

		%%%%%%% Section for cond_utils:if_debug/1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		% Calls to cond_utils:if_debug(Expr) shall be replaced either by the
		% corresponding specified expression or by nothing at all (not even
		% 'ok'):
		%
		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
							{atom,FileLocFun,if_debug} },
		  _Params=[ ExprForm ],
		  Transforms=#ast_transforms{ transformation_state=TokenTable } ) ->

			%ast_utils:display_debug( "Call to cond_utils:if_debug/1 found" ),

			% Implicit token here:
			Token = myriad_debug_mode,

			case ?table:lookup_entry( Token, TokenTable ) of

				% Any value associated to this token will do, as we want just to
				% detect whether it is defined at all:
				%
				{ value, _Any } ->
					% So we will (attempt to) inject this expression:
					inject_expression( ExprForm, Transforms, FileLocFun );

				key_not_found ->
					%ast_utils:display_debug( "Token '~p' not defined, hence "
					%   "skipping as a whole expression~n~p",
					%   [ Token, ExprForm ] ),
					{ _Expr=[], Transforms }

			end;


		%%%%%%% Section for cond_utils:if_defined %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%%%%%%% Subsection for cond_utils:if_defined/2 %%%%%%%%%%%%%%%%%%%%%%%%%

		% Calls to cond_utils:if_defined(Token, Expr) shall be replaced either
		% by the corresponding specified expression or by nothing at all (not
		% even 'ok'):
		%
		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils}, {atom,_,if_defined} },
		  _Params=[ {atom,FileLocToken,Token}, ExprForm ],
		  Transforms=#ast_transforms{ transformation_state=TokenTable } ) ->

			%ast_utils:display_debug( "Call to cond_utils:if_defined/2 found, "
			%                         "for token '~p'.", [ Token ] ),

			case ?table:lookup_entry( Token, TokenTable ) of

				% Any value associated to this token will do, as we want just to
				% detect whether it is defined at all:
				%
				{ value, _Any } ->
					% So we will (attempt to) inject this expression:
					inject_expression( ExprForm, Transforms, FileLocToken );

				key_not_found ->
					%ast_utils:display_debug( "Token '~p' not defined, hence "
					%   "skipping as a whole expression~n~p",
					%   [ Token, ExprForm ] ),
					{ _Expr=[], Transforms }

			end;

		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils}, {atom,_,if_defined} },
		  _Params=[ {var,FileLoc,VarName}, _ExprForm ],
		  _Transforms ) ->
			ast_utils:display_error(
				"A token used with cond_utils:if_defined/2 must be an "
				"immediate value (precisely an atom), not a (runtime) "
				"variable like '~ts' (at ~ts).",
				[ VarName, ast_utils:file_loc_to_string( FileLoc ) ] ),
			ast_utils:raise_error( { non_immediate_token, VarName,
				ast_utils:file_loc_to_explicative_term( FileLoc ) } );


		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
						 {atom,FileLoc,if_defined} },
		  _Params=[ _Other, _Expr ],
		  _Transforms ) ->
			ast_utils:display_error(
				"A token used with cond_utils:if_defined/2 must be an "
				"immediate value (precisely an atom), not a runtime "
				"construct like the one at ~ts.",
				[ ast_utils:file_loc_to_string( FileLoc ) ] ),
			ast_utils:raise_error( { non_immediate_token,
				ast_utils:file_loc_to_explicative_term( FileLoc ) } );


		%%%%%%% Subsection for cond_utils:if_defined/3 %%%%%%%%%%%%%%%%%%%%%%%%%

		% Calls to cond_utils:if_defined(Token, ExprFormIfDef, ExprFormIfNotDef)
		% shall be replaced by either of the corresponding specified
		% expressions, depending on whether the specified token has been
		% defined:
		%
		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils}, {atom,_,if_defined} },
		  _Params=[ {atom,FileLocToken,Token}, ExprFormIfDef,
					ExprFormIfNotDef ],
		  Transforms=#ast_transforms{ transformation_state=TokenTable } ) ->

			%ast_utils:display_debug( "Call to cond_utils:if_defined/3 found, "
			%                         "for token '~p'.", [ Token ] ),

			case ?table:lookup_entry( Token, TokenTable ) of

				% Any value associated to this token will do, as we want just to
				% detect whether it is defined at all:
				%
				{ value, _Any } ->
					%ast_utils:display_debug( "Token '~p' defined, hence "
					%    "injecting the expression ~p",
					%    [ Token, ExprFormIfDef ] ),
					inject_expression( ExprFormIfDef, Transforms,
									   FileLocToken );

				key_not_found ->
					%ast_utils:display_debug( "Token '~p' not defined, hence "
					%    "injecting the expression ~p",
					%    [ Token, ExprFormIfNotDef ] ),
					inject_expression( ExprFormIfNotDef, Transforms,
									   FileLocToken )

			end;

		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils}, {atom,_,if_defined} },
		  _Params=[ { var,FileLoc,VarName}, _ExprFormIfDef, _ExprFormIfNotDef ],
		  _Transforms ) ->
			ast_utils:display_error(
				"A token used with cond_utils:if_defined/3 must be an "
				"immediate value (precisely an atom), not a (runtime) variable "
				"like '~ts' (at ~ts).",
				[ VarName, ast_utils:file_loc_to_string( FileLoc ) ] ),
			ast_utils:raise_error( { non_immediate_token, VarName,
				ast_utils:file_loc_to_explicative_term( FileLoc ) } );


		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
							{atom,FileLoc,if_defined} },
		  _Params=[ _Other, _ExprFormIfDef, _ExprFormIfNotDef ],
		  _Transforms ) ->
			ast_utils:display_error(
				"A token used with cond_utils:if_defined/3 must be an "
				"immediate value (precisely an atom), not a runtime construct "
				"like the one at ~ts.",
				[ ast_utils:file_loc_to_string( FileLoc ) ] ),
			ast_utils:raise_error( { non_immediate_token,
				ast_utils:file_loc_to_explicative_term( FileLoc ) } );



		%%%%%%% Section for cond_utils:if_set_to %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


		%%%%%%% Subsection for cond_utils:if_set_to/3 %%%%%%%%%%%%%%%%%%%%%%%%%%

		% Calls to cond_utils:if_set_to(Token, Value, Expr) shall be replaced
		% either by the corresponding specified expression or by nothing at all
		% (not even 'ok'):
		%
		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils}, {atom,_,if_set_to} },
		  _Params=[ {atom,FileLocToken,Token}, ValueForm, ExprForm ],
		  Transforms=#ast_transforms{ transformation_state=TokenTable } ) ->

			%ast_utils:display_debug( "Call to cond_utils:if_set_to/3 found, "
			%   "for token '~p' and value '~p'.", [ Token, ValueForm ] ),

			RequestedValue = ast_value:get_immediate_value( ValueForm ),

			case ?table:lookup_entry( Token, TokenTable ) of

				% Right value found matching:
				{ value, RequestedValue } ->
					%ast_utils:display_debug( "Token '~p' defined and set to "
					%   "the right value ('~p'), hence "
					%   "injecting the expression ~p",
					%   [ Token, RequestedValue, ExprForm ] ),

					% So we will (attempt to) inject this expression:
					inject_expression( ExprForm, Transforms, FileLocToken );

				% Another value found:
				{ value, _OtherValue } ->
					%ast_utils:display_debug( "Token '~p' defined but not set "
					%   "to the right value (set to '~ts' instead of '~ts'), "
					%   "hence skipping as a whole expression~n~p",
					%   [ Token, OtherValue, RequestedValue, ExprForm ] ),
					{ _Expr=[], Transforms };

				key_not_found ->
					%ast_utils:display_debug( "Token '~p' not defined, hence "
					%   "skipping as a whole expression~n~p",
					%   [ Token, ExprForm ] ),
					{ _Expr=[], Transforms }

			end;


		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils}, {atom,_,if_set_to} },
		  _Params=[ { var,FileLoc,VarName}, _ValueForm, _Expr ],
		  _Transforms ) ->
			ast_utils:display_error(
				"A token used with cond_utils:if_set_to/3 must be an immediate "
				"value (precisely an atom), not a (runtime) variable like "
				"'~ts' (at ~ts).",
				[ VarName, ast_utils:file_loc_to_string( FileLoc ) ] ),
			ast_utils:raise_error( { non_immediate_token, VarName,
				ast_utils:file_loc_to_explicative_term( FileLoc ) } );


		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
							{atom,FileLoc,if_set_to} },
		  _Params=[ _Other, _ValueForm, _Expr ],
		  _Transforms ) ->
			ast_utils:display_error(
				"A token used with cond_utils:if_set_to/3 must be an immediate "
				"value (precisely an atom), not a runtime construct like the "
				"one at ~ts.", [ ast_utils:file_loc_to_string( FileLoc ) ] ),
			ast_utils:raise_error( { non_immediate_token,
				ast_utils:file_loc_to_explicative_term( FileLoc ) } );



		%%%%%%% Subsection for cond_utils:if_set_to/4 %%%%%%%%%%%%%%%%%%%%%%%%%%


		% Calls to cond_utils:if_set_to(Token, Value, ExprFormIfMatching,
		% ExprFormIfNotMatching) shall be replaced by either of the
		% corresponding specified expressions, depending on whether the
		% specified token has been set to the specified value:
		%
		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils}, {atom,_,if_set_to} },
		  _Params=[ {atom,FileLocToken,Token}, ValueForm, ExprFormIfMatching,
					ExprFormIfNotMatching ],
		  Transforms=#ast_transforms{ transformation_state=TokenTable } ) ->

			%ast_utils:display_debug( "Call to cond_utils:if_set_to/4 found, "
			%    "for token '~p' and value '~p'.", [ Token, ValueForm ] ),

			RequestedValue = ast_value:get_immediate_value( ValueForm ),

			case ?table:lookup_entry( Token, TokenTable ) of

				% Right value found matching:
				{ value, RequestedValue } ->
					%ast_utils:display_debug( "Token '~p' defined and set to "
					%   "the right value ('~p'), hence "
					%   "injecting the expression ~p",
					%   [ Token, RequestedValue, ExprFormIfMatching ] ),

					% So we will (attempt to) inject expression:
					inject_expression( ExprFormIfMatching, Transforms,
									   FileLocToken );

				% Another value found:
				{ value, _OtherValue } ->
					%ast_utils:display_debug( "Token '~p' defined but not set "
					%   "to the right value (set to '~ts' instead of '~ts'), "
					%   "hence injecting the expression~n~p",
					%   [ Token, OtherValue, RequestedValue,
					%     ExprFormIfNotMatching ] ),

					% So we will (attempt to) inject this expression:
					inject_expression( ExprFormIfNotMatching, Transforms,
									   FileLocToken );

				key_not_found ->
					%ast_utils:display_debug( "Token '~p' not defined, hence "
					%   "injecting the expression~n ~p",
					%   [ Token, ExprFormIfNotMatching ] ),
					inject_expression( ExprFormIfNotMatching, Transforms,
									   FileLocToken )

			end;

		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
						 {atom,_,if_set_to} },
		  _Params=[ { var,FileLoc,VarName}, _ValueForm,_ExprFormIfMatching,
					_ExprFormIfNotMatching ], _Transforms ) ->
			ast_utils:display_error(
				"A token used with cond_utils:if_set_to/4 must be an immediate "
				"value (precisely an atom), not a (runtime) variable like "
				"'~ts' (at ~ts).",
				[ VarName, ast_utils:file_loc_to_string( FileLoc ) ] ),
			ast_utils:raise_error( { non_immediate_token, VarName,
				ast_utils:file_loc_to_explicative_term( FileLoc ) } );


		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
							{atom,FileLoc,if_set_to} },
		  _Params=[ _Other, _ValueForm, _ExprFormIfMatching,
					_ExprFormIfNotMatching ], _Transforms ) ->
			ast_utils:display_error(
				"A token used with cond_utils:if_set_to/4 must be an immediate "
				"value (precisely an atom), not a runtime construct like the "
				"one at ~ts.", [ ast_utils:file_loc_to_string( FileLoc ) ] ),
			ast_utils:raise_error( { non_immediate_token,
				ast_utils:file_loc_to_explicative_term( FileLoc ) } );


		%%%%%%% Section for cond_utils:switch_execution_target/2 %%%%%%%%%%%%%%%

		% switch_execution_target( A, B) shall be equivalent to:
		% if_defined( _Token=exec_target_is_production, A, B ).

		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
							{atom,FileLocFun,switch_execution_target} },
		  _Params=[ ExprIfInDevMode, ExprIfProdMode ],
		  Transforms=#ast_transforms{ transformation_state=TokenTable } ) ->

			%ast_utils:display_debug(
			%   "Call to cond_utils:switch_execution_target/2 found." ),

			case ?table:lookup_entry( _Token=exec_target_is_production,
									  TokenTable ) of

				% Any value associated to this token will do, as we want just to
				% detect whether it is defined at all:
				%
				{ value, _Any } ->
					%ast_utils:display_debug( "Token '~p' defined, hence "
					%    "injecting the 'production' expression ~p",
					%    [ Token, ExprIfInDevMode ] ),
					inject_expression( ExprIfProdMode, Transforms, FileLocFun );

				key_not_found ->
					%ast_utils:display_debug( "Token '~p' not defined, hence "
					%    "injecting the 'developement' expression ~p",
					%    [ Token, ExprFormIfNotDef ] ),
					inject_expression( ExprIfInDevMode, Transforms, FileLocFun )

			end;

			% No token-related error case to handle here.


		%%%%%%% Section for cond_utils:switch_set_to %%%%%%%%%%%%%%%%%%%%%%%%%%%


		%%%%%%% Subsection for cond_utils:switch_set_to/2 %%%%%%%%%%%%%%%%%%%%%%

		% Calls to cond_utils:switch_set_to(Token, TokenExprTable) shall either
		% be replaced by the expression associated to the specified value for
		% that token, or shall trigger a compilation-time error.
		%
		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
							{atom,_,switch_set_to} },
		  _Params=[ {atom,FileLocToken,Token}, TokenExprTableAsForm ],
		  Transforms=#ast_transforms{ transformation_state=TokenTable } ) ->

			%ast_utils:display_debug( "Call to cond_utils:switch_set_to/2 "
			%   "found, for token '~p' and token-expression table "
			%   "(as form):~n  ~p.", [ Token, TokenExprTableAsForm ] ),

			TokenValue = case ?table:lookup_entry( Token, TokenTable ) of

				{ value, Value } ->
					Value;

				key_not_found ->
					ast_utils:display_error( "Token '~p' not set, whereas "
						"cond_utils:switch_set_to/2 (at ~ts) requires it.",
						[ Token,
						  ast_utils:file_loc_to_string( FileLocToken ) ] ),
					ast_utils:raise_error( { token_not_set, Token,
						ast_utils:file_loc_to_explicative_term(
							FileLocToken ) } )

			end,

			%ast_utils:display_debug( "Value associated to token '~p': ~p "
			%   "(type: ~ts).",
			%   [ Token, TokenValue, type_utils:get_type_of( TokenValue ) ] ),

			% We have to see whether TokenValue can be found among the keys of
			% the (proplist, in AST form) TokenExprTableAsForm:

			% Obtaining a list of {ValueForm, ExprForm} pairs:
			TokenExprTableAsList =
				ast_generation:form_to_list( TokenExprTableAsForm ),

			%ast_utils:display_debug( "Token table as list: ~p.",
			%                         [ TokenExprTableAsList ] ),

			ExprForm = find_expression_for( TokenValue, Token, FileLocToken,
											TokenExprTableAsList ),

			%ast_utils:display_debug( "Resulting expression:~n  ~p",
			%                         [ ExprForm ] ),

			% So we will (attempt to) inject this expression:
			inject_expression( ExprForm, Transforms, FileLocToken );


		%%%%%%% Subsection for cond_utils:switch_set_to/3 %%%%%%%%%%%%%%%%%%%%%%

		% Calls to cond_utils:switch_set_to(Token, TokenExprTable,
		% DefaultTokenValue) shall either be replaced by the expression
		% associated to the specified value for that token, or, if that token is
		% either not defined or set to a value that does not pertain to said
		% table, the call-specified default value will be used instead as the
		% token value is charge of selecting which expression shall be injected.
		%
		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
							{atom,_,switch_set_to} },
		  _Params=[ {atom,FileLocToken,Token}, TokenExprTableAsForm,
					DefaultValueForm ],
		  Transforms=#ast_transforms{ transformation_state=TokenTable } ) ->

			DefaultValue = ast_value:get_immediate_value( DefaultValueForm ),

			% Obtaining a list of {ValueForm, ExprForm}:
			TokenExprTableAsList =
				ast_generation:form_to_list( TokenExprTableAsForm ),

			%ast_utils:display_debug( "Call to cond_utils:switch_set_to/3 "
			%   "found, for token '~p', default value '~p' and "
			%   "token-expression table (as form):~n  ~p.",
			%   [ Token, DefaultValue, TokenExprTableAsForm ] ),

			ExprForm = case ?table:lookup_entry( Token, TokenTable ) of

				{ value, TokenValue } ->
					% This value may or may not be referenced:
					find_expression_for( TokenValue, DefaultValue, Token,
										 FileLocToken, TokenExprTableAsList );

				key_not_found ->
					% Like switch_set_to/2:
					find_expression_for( DefaultValue, Token, FileLocToken,
										 TokenExprTableAsList )

			end,

			%ast_utils:display_debug( "Resulting expression:~n  ~p",
			%                         [ ExprForm ] ),

			% So we will (attempt to) inject this expression:
			inject_expression( ExprForm, Transforms, FileLocToken );



		%%%%%%% Section for cond_utils:assert %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%%%%%%% Subsection for cond_utils:assert/1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
							{atom,FileLocAssert,assert} },
		  _Params=[ ExpressionForm ],
		  Transforms=#ast_transforms{ transformation_state=TokenTable } ) ->

			%ast_utils:display_debug( "Call to cond_utils:assert/1 found, "
			%   "with expression form ~p.", [ ExpressionForm ] ),

			% Implicit token here:
			Token = myriad_debug_mode,

			case ?table:lookup_entry( Token, TokenTable ) of

				% Any value associated to this token will do, as we want just to
				% detect whether it is defined at all:
				%
				{ value, _Any } ->
					% So we will (attempt to) inject a match expression:
					inject_match_expression( ExpressionForm, Transforms,
											 FileLocAssert );

				key_not_found ->
					%ast_utils:display_debug( "Token '~p' not defined, hence "
					%   "skipping as a whole expression ~n~p",
					%   [ Token, ExprForm ] ),
					{ _Expr=[], Transforms }

			end;


		%%%%%%% Subsection for cond_utils:assert/2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils}, {atom,_,assert} },
		  _Params=[ {atom,FileLocToken,Token}, ExpressionForm ],
		  Transforms=#ast_transforms{ transformation_state=TokenTable } ) ->

			%ast_utils:display_debug( "Call to cond_utils:assert/2 found, "
			%   "with token '~p' and expression form ~p.",
			%   [ Token, ExpressionForm ] ),

			case ?table:lookup_entry( Token, TokenTable ) of

				% Any value associated to this token will do, as we want just to
				% detect whether it is defined at all:
				%
				{ value, _Any } ->
					% So we will (attempt to) inject a match expression:
					inject_match_expression( ExpressionForm, Transforms,
											 FileLocToken );

				key_not_found ->
					%ast_utils:display_debug( "Token '~p' not defined, hence "
					%   "skipping as a whole expression ~n~p",
					%   [ Token, ExprForm ] ),
					{ _Expr=[], Transforms }

			end;


		%%%%%%% Subsection for cond_utils:assert/3 %%%%%%%%%%%%%%%%%%%%%%%


		( _FileLocCall,
		  _FunctionRef={ remote, _, {atom,_,cond_utils},
						 % Resist the temptation of naming it assert_equal: we
						 % are not comparing at runtime ValueForm and
						 % ExpressionForm, but at compile time the value
						 % associated to the token with ValueForm:
						 %
						 {atom,_,assert} },
		  _Params=[ {atom,FileLocToken,Token}, ValueForm, ExpressionForm ],
		  Transforms=#ast_transforms{ transformation_state=TokenTable } ) ->

			%ast_utils:display_debug( "Call to cond_utils:assert/3 "
			%   "found, with token '~p' and expression form ~p.",
			%   [ Token, ExpressionForm ] ),

			RequestedValue = ast_value:get_immediate_value( ValueForm ),

			case ?table:lookup_entry( Token, TokenTable ) of

				% Right value found matching:
				{ value, RequestedValue } ->
					%ast_utils:display_debug( "Token '~p' defined and set to "
					%    "the right value ('~p'), hence "
					%    "injecting the expression ~p",
					%    [ Token, RequestedValue, ExpressionForm ] ),

					% So we will (attempt to) inject a match expression:
					inject_match_expression( ExpressionForm, Transforms,
											 FileLocToken );

				% Another value found:
				{ value, _OtherValue } ->
					%ast_utils:display_debug( "Token '~p' defined but not set "
					%   "to the right value (set to '~ts' instead of '~ts'), "
					%   "hence skipping as a whole expression ~n~p",
					%   [ Token, OtherValue, RequestedValue, ExpressionForm ] ),
					{ _Expr=[], Transforms };

				key_not_found ->
					%ast_utils:display_debug( "Token '~p' not defined, hence "
					%   "skipping as a whole expression ~n~p",
					%   [ Token, ExprForm ] ),
					{ _Expr=[], Transforms }

			end;


		%%%%%%% Section for table %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		% For all function names and arities, the 'table' module shall be
		% replaced in remote calls by the desired table type:
		%
		( FileLocCall,
		  _FunctionRef={ remote, FileLoc1, {atom,FileLoc2,table}, FunNameForm },
		  Params,
		  Transforms ) ->
			%ast_utils:display_debug( "replacing call to 'table' by a call "
			%  "to '~p' at ~ts for parameters ~p",
			%  [ DesiredTableType,
			%    ast_utils:file_loc_to_string( FileLoc1 ), Params ] ),

			% Just swap the 'table' module with the desired one:
			NewFunctionRef = { remote, FileLoc1,
				{atom,FileLoc2,DesiredTableType}, FunNameForm },

			% We have to recurse as well in parameters, as they may themselves
			% contain calls to 'table' as well, like in:
			%
			% TargetTable = table:add_entry( a, 1, table:new() ),

			{ NewParams, NewTransforms } =
				ast_expression:transform_expressions( Params, Transforms ),

			NewExpr = { call, FileLocCall, NewFunctionRef, NewParams },

			{ [ NewExpr ], NewTransforms };


		%%%%%%% Section for text formatting %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		% Each call to such function (e.g. text_utils:format/2), provided that
		% the format string is directly a string literal, shall be:
		%
		% (1) checked so that the number of specified values is equal to the
		% number of elements of the control sequence in the format string (the
		% type of these values is not specifically checked here)
		%
		% (2) possibly replaced by another function call (here a direct - yet
		% potentially crashing, so it may not be a good idea - call to
		% io_lib:format/2)

        % The compiler already checks io:format/2 and io_lib:format/2 (with a
        % warning, e.g. 'the format string requires an argument list with 1
        % argument, but the argument list contains 2 arguments'), so we focus on
        % the formatting functions that this layer introduces.

        % Many calls should be caught (use 'ergrep format_values' to spot them,
        % for example in UI/GUI); at least for the moment we concentrate on the
        % key / most usual ones.

        % For text_utils:{,bin_,atom_}format/2 and others:
		( _FileLocCall,
		  _FunctionRef={ remote, _FileLoc1,
						 ModNameForm={atom,FileLoc2,_ModName=text_utils},
						 FunNameForm={atom,_FileLoc3,FunName} },
		  Params=[ {string, _FileLoc4, _FormatString}, _FormatValuesForm ],
		  Transforms ) when FunName =:= format orelse FunName =:= bin_format
                            orelse FunName =:= atom_format
                            orelse FunName =:= format_ellipsed
                            orelse FunName =:= ellipse_fmt ->

			%?display_debug( "Call to text_utils:format(~p, ~p) "
			%   "intercepted.", [ FormatString, FormatValuesForm ] ),

			% Safety preferred over performance, keeping text_utils:
			%NewModName = {atom,FileLoc2,io_lib},
            NewModNameForm = ModNameForm,

            handle_formatting_call_2p( FileLoc2, NewModNameForm, FunNameForm,
                                       Params, Transforms );


        % For basic_utils:display*/2, etc.:
		( _FileLocCall,
		  _FunctionRef={ remote, _FileLoc1,
						 ModNameForm={atom,FileLoc2,_ModName=basic_utils},
						 FunNameForm={atom,_FileLoc3,FunName} },
		  Params=[ {string, _FileLoc4, _FormatString}, _FormatValuesForm ],
		  Transforms ) when FunName =:= notify_user
                            orelse FunName =:= display
                            orelse FunName =:= display_timed
                            orelse FunName =:= display_error
                            orelse FunName =:= debug ->

            handle_formatting_call_2p( FileLoc2, ModNameForm, FunNameForm,
                                       Params, Transforms );


        % For test_facilities:display/2:
		( _FileLocCall,
		  _FunctionRef={ remote, _FileLoc1,
						 ModNameForm={atom,FileLoc2,_ModName=test_facilities},
						 FunNameForm={atom,_FileLoc3,_FunName=display} },
		  Params=[ {string, _FileLoc4, _FormatString}, _FormatValuesForm ],
		  Transforms ) ->

            handle_formatting_call_2p( FileLoc2, ModNameForm, FunNameForm,
                                       Params, Transforms );

        % For file_utils:write_ustring/2:
		( _FileLocCall,
		  _FunctionRef={ remote, _FileLoc1,
						 ModNameForm={atom,FileLoc2,_ModName=file_utils},
						 FunNameForm={atom,_FileLoc3,_FunName=write_ustring} },
		  Params=[ {string, _FileLoc4, _FormatString}, _FormatValuesForm ],
		  Transforms ) ->

            handle_formatting_call_2p( FileLoc2, ModNameForm, FunNameForm,
                                       Params, Transforms );


        % For app_facilities:display/2 and al:
		( _FileLocCall,
		  _FunctionRef={ remote, _FileLoc1,
						 ModNameForm={atom,FileLoc2,_ModName=app_facilities},
						 FunNameForm={atom,_FileLoc3,FunName} },
		  Params=[ {string, _FileLoc4, _FormatString}, _FormatValuesForm ],
		  Transforms ) when FunName =:= display orelse FunName =:= fail ->

            handle_formatting_call_2p( FileLoc2, ModNameForm, FunNameForm,
                                       Params, Transforms );


        % For trace_bridge:*_fmt:/2, just checking send/3, on which they all
        % rely, is not an option: we have to intercept the overall, user-level
        % calls.
        %
		( _FileLocCall,
		  _FunctionRef={ remote, _FileLoc1,
						 ModNameForm={atom,FileLoc2,_ModName=trace_bridge},
						 FunNameForm={atom,_FileLoc3,FunName} },
		  Params=[ {string, _FileLoc4, _FormatString}, _FormatValuesForm ],
		  Transforms ) when FunName =:= debug_fmt orelse FunName =:= info_fmt
                orelse FunName =:= notice_fmt orelse FunName =:= warning_fmt
                orelse FunName =:= error_fmt orelse FunName =:= critical_fmt
                orelse FunName =:= alert_fmt orelse FunName =:= emergency_fmt
                orelse FunName =:= void_fmt ->

            handle_formatting_call_2p( FileLoc2, ModNameForm, FunNameForm,
                                       Params, Transforms );


		% Other calls shall go through:
		( FileLocCall, FunctionRef, Params, Transforms ) ->

			% Of course very verbose:
			%?display_debug( "(not changing function referenced as ~p "
			%   "whose parameters are: ~n~p)", [ FunctionRef, Params ] ),

			{ NewParams, NewTransforms } =
				ast_expression:transform_expressions( Params, Transforms ),

			RecursedExpr = { call, FileLocCall, FunctionRef, NewParams },

			{ [ RecursedExpr ], NewTransforms }

	end,

	% Returning a corresponding remote call transformation table:
	BaseTable = ?table:singleton( _FirstTrigger=call, RemoteCallTransformFun ),

	case DisableLCO of

		true ->
			% We operate at the clause level (each ending with a call to an
			% identity function):
			%
			?table:add_new_entry( _K=clause,
				fun lco_disabling_clause_transform_fun/2, BaseTable );

		false ->
			BaseTable

	end.



-doc """
Centralises the checking and transformation of all calls akin to
`text_utils:format/2`, hence with 2 parameters, for Myriad and all the layers
deriving from it.
""".
-spec handle_formatting_call_2p( file_loc(), form(), form(),
    [ ast_expression() ], ast_transforms() ) ->
                                { [ ast_expression() ], ast_transforms() }.
handle_formatting_call_2p( FileLoc, ModNameForm, FunNameForm, ParamExprs,
                       Transforms ) ->

    NewFunctionRef = { remote, FileLoc, ModNameForm, FunNameForm },

    % We will check the transformed versions:
    { NewParamExprs=[ { string, _FileLoc, NewFormatString },
                  NewFormatValuesForm ], NewTransforms } =
        ast_expression:transform_expressions( ParamExprs, Transforms ),

    case NewFormatValuesForm of

        % Generally is directly a list, either empty (like here) or not:
		{ nil, _ } ->
			check_format_string( NewFormatString, NewFormatValuesForm,
				FileLoc, NewFunctionRef, NewParamExprs, NewTransforms );

        % Here a non-empty list:
        { cons, _, _, _ } ->
            check_format_string( NewFormatString, NewFormatValuesForm,
				FileLoc, NewFunctionRef, NewParamExprs, NewTransforms );

        % Not a direct list, for example '{call, ...'; then pass-through, no
        % build-time checking/transformation apply:
		%
		_ ->
			NewExpr = { call, FileLoc, NewFunctionRef, NewParamExprs },
			{ [ NewExpr ], NewTransforms }

    end.



-doc """
Centralises the checking and transformation of all calls akin to
`trace_bridge:send/3`, hence with 3 parameters (the first one being an extra one
compared to `handle_formatting_call_2p/5`), for Myriad and all the layers
deriving from it.
""".
-spec handle_formatting_call_3p( file_loc(), form(), form(),
    [ ast_expression() ], ast_transforms() ) ->
                                { [ ast_expression() ], ast_transforms() }.
handle_formatting_call_3p( FileLoc, ModNameForm, FunNameForm, ParamExprs,
                           Transforms ) ->

    NewFunctionRef = { remote, FileLoc, ModNameForm, FunNameForm },

    % We will check the transformed versions:
    { NewParamExprs=[ _FirstParamForm, { string, _FileLoc, NewFormatString },
                  NewFormatValuesForm ], NewTransforms } =
        ast_expression:transform_expressions( ParamExprs, Transforms ),

    case NewFormatValuesForm of

        % Generally is directly a list, either empty (like here) or not:
		{ nil, _ } ->
			check_format_string( NewFormatString, NewFormatValuesForm,
				FileLoc, NewFunctionRef, NewParamExprs, NewTransforms );

        % Here a non-empty list:
        { cons, _, _, _ } ->
            check_format_string( NewFormatString, NewFormatValuesForm,
				FileLoc, NewFunctionRef, NewParamExprs, NewTransforms );

        % Not a direct list, for example '{call, ...'; then pass-through, no
        % build-time checking/transformation apply:
		%
		_ ->
			NewExpr = { call, FileLoc, NewFunctionRef, NewParamExprs },
			{ [ NewExpr ], NewTransforms }

    end.




-spec check_format_string( format_string(), term(), term(), term(),
		list(), ast_transforms() ) -> { ast_clause(), ast_transforms() }.
check_format_string( FormatString, FormatValuesForm, FileLocCall,
					 FunctionRef, Params, Transforms ) ->

	%ast_utils:display_debug( "Checking format string '~p' against values ~p.",
	%                         [ FormatString, FormatValuesForm ] ),

	case text_utils:scan_format_string( FormatString ) of

		{ format_parsing_failed, ReasonStr } ->
			ast_utils:display_error( "Failed to scan format string '~ts' "
				"at ~ts: ~ts",
				[ FormatString, ast_utils:file_loc_to_string( FileLocCall ),
				  ReasonStr ] ),

			ast_utils:raise_error( { invalid_format_string, FormatString,
									 FileLocCall, ReasonStr } );

		ValueDescs ->
			% Here, at compile-time, we cannot make the finer study done at
			% runtime by text_utils:scan_format_string/1, we can just compare
			% counts:

			FmtParamCount = length( ValueDescs ),
			ParamCount = ast_generation:list_form_length( FormatValuesForm ),

			case FmtParamCount of

				ParamCount ->
					%ast_utils:display_debug( "(use of format string '~ts' "
					%   "validated)", [ FormatString ] ),

					NewExpr = { call, FileLocCall, FunctionRef, Params },
					{ [ NewExpr ], Transforms };

				_ ->
					FmtParamStr = case FmtParamCount of

						0 ->
							"no value";

						1 ->
							text_utils:format( "one value (of type ~ts)",
											   [ hd( ValueDescs ) ] );

						_ ->
							text_utils:format( "~B values (of types ~w)",
											   [ FmtParamCount, ValueDescs ] )

					end,

					ParamStr = case ParamCount of

						0 ->
							"no parameter is";

						1 ->
							"one parameter is";

						_ ->
							text_utils:format( "~B parameters are",
											   [ ParamCount ] )

					end,

					ast_utils:display_error( "The format string '~ts' (~ts) "
						"requires ~ts, but ~ts specified.",
						[ FormatString,
						  ast_utils:file_loc_to_string( FileLocCall ),
						  FmtParamStr, ParamStr ] ),

					ast_utils:raise_error( { inconsistent_format_string,
						FileLocCall, FormatString, FmtParamCount, ParamCount } )

			end

	end.




-doc """
The transformation function in charge of disabling LCO (Last Call Optimisation)
by ending each local function call with a remote one to an identity function
(namely `basic_utils:identity/1`).
""".
-spec lco_disabling_clause_transform_fun( ast_clause(), ast_transforms() ) ->
									{ ast_clause(), ast_transforms() }.
% Not expected to happen:
%lco_disabling_clause_transform_fun( _Clause={ 'clause', _FileLoc,
%       _HeadPatternSequence, _GuardSequence, _BodyExprs=[] }, _Transforms ) ->
%   throw( empty_body );

lco_disabling_clause_transform_fun( _Clause={ 'clause', FileLoc,
		HeadPatternSequence, GuardSequence, BodyExprs }, Transforms ) ->

	% No list_utils:extract_last_element/1 available from here, so:
	[ LastExpr | RevRestExprs ] = lists:reverse( BodyExprs ),

	% Mere '_' are file locations:
	NewBodyExprs = case LastExpr of

		{ call, _, {remote, _, _ModExpr, _FunExpr}, _ArgsExpr } ->
			%ast_utils:display_debug( "No change needed, clause already ends "
			%   "with a remote call:~n ~p.~n", [ LastExpr ] ),
			BodyExprs;

		OtherExpr ->
			% Not reusing FileLoc, as the beginning of that clause may be far
			% before:
			%
			LastFileLoc = element( _Index=2, OtherExpr ),

			NewLastExpr = { call, LastFileLoc, {remote, LastFileLoc,
								{atom, LastFileLoc, basic_utils},
								{atom, LastFileLoc, identity } },
							[ OtherExpr ] },

			%ast_utils:display_debug( "Clause not ending with a remote call, "
			%   "adding one to an identity function:~n ~p.~n",
			%   [ NewLastExpr ] ),

			RevExprs = [ NewLastExpr | RevRestExprs ],

			lists:reverse( RevExprs )

	end,

	NoLCOClause = { 'clause', FileLoc, HeadPatternSequence, GuardSequence,
					NewBodyExprs },

	% Now that LCO is disabled, let's apply the usual clause-level
	% transformations:
	%
	ast_clause:transform_clause_default( NoLCOClause, Transforms ).

% Not expected to happen:
%lco_disabling_clause_transform_fun( UnmatchedClause, _Transforms ) ->
%   throw( { unmatched_clause, UnmatchedClause } ).



-doc "Injects the specified expression in the AST.".
-spec inject_expression( ast_expression(), ast_transforms(), file_loc() ) ->
								{ [ ast_expression() ], ast_transforms() }.

% The two next clauses are not used anymore, as semantically ambiguous, see
% documentation:

% Nothing to inject here (empty conditional expression list):
%inject_expression( _ExprFormList={ nil, _ }, Transforms, _FileLoc ) ->
%   { _NewExprs=[], Transforms };

% A list of expressions shall be injected here:
%inject_expression( ExprFormList={ cons, _, _Head, _Tail }, Transforms,
%                   _FileLoc ) ->
%
%   %ast_utils:display_debug( "Token '~p' defined, hence injecting expressions"
%   %                         "corresponding to ~p", [ Token, ExprFormList ] ),
%
%   % The corresponding, specified code (expressions) is thus enabled; the AST
%   % expects them as the elements of a list ({cons,_,E1, {cons,_,E2,...}),
%   % whereas we need an actual, direct list here (ie [E1, E2, ...]), so:
%   %
%   Exprs = ast_generation:form_to_list( ExprFormList ),
%
%   % This injected code may need to be transformed (e.g. if referencing the
%   % table module), so:
%   %
%   ast_expression:transform_expressions( Exprs, Transforms );


% Other, non-list (e.g. call, match pattern, etc.) expression parameter, which
% used to be unsupported; the constraint of having a list of expressions has
% been relaxed, a single expression is accepted as well (actually now it is the
% only option):
%
inject_expression( ExprForm, Transforms, _FileLoc ) ->

	% ast_utils:display_error( "Unsupported expression specified at ~ts "
	%     "for a conditional injection (:~n~p", [
	%   ast_utils:file_loc_to_string( FileLoc ), OtherExprForm ] ),

	% ast_utils:raise_error( { unsupported_expression_for_conditional_injection,
	%   ast_utils:file_loc_to_explicative_term( FileLoc ) } ).

	ast_expression:transform_expression( ExprForm, Transforms ).



-doc """
Injects an expression checking whether once evaluated the corresponding form
matches the `true` atom.
""".
-spec inject_match_expression( ast_expression(), ast_transforms(),
					file_loc() ) -> { [ ast_expression() ], ast_transforms() }.
inject_match_expression( ExpressionForm, Transforms, FileLoc ) ->

	% Was initially:
	% NewExpr = { match, FileLoc, {atom,FileLoc,true}, ExpressionForm },
	% yet the error message was not sufficiently clear: {badmatch,false}.

	% Now corresponds roughly to:
	%
	% EXPR =:= true orelse throw({assertion_failed,Other})
	% (no need to do more as the stacktrace with line numbers shall be output)

	% We have to ensure that the name of the variable that we bind in the second
	% clause is reasonably unique, otherwise, should more than one assert be
	% found by the compiler in a given scope, it would deem that this variable
	% name (e.g. 'Other') would be unsafe in 'case' (and of course this name
	% should not clash with user-defined ones). So:
	%
	VarName = list_to_atom( lists:flatten(
		io_lib:format( "Myriad_assert_var_name-~ts",
					   [ ast_utils:format_file_loc_alt( FileLoc ) ] ) ) ),

	NewExpr = { 'case', FileLoc, ExpressionForm,
				[ {clause,FileLoc,[{atom,FileLoc,true}],[],[{atom,FileLoc,ok}]},
				  {clause,FileLoc,
					 [{var,FileLoc,VarName}], [],
					 [{call,FileLoc,
						  {atom,FileLoc,throw},
						  [ { tuple, FileLoc,[ {atom,FileLoc,assertion_failed},
											{var,FileLoc,VarName} ] } ] }]}] },

	ast_expression:transform_expression( NewExpr, Transforms ).



-doc """
Finds in the specified token-expression table the expression associated to the
specified token value, and returns it.
""".
find_expression_for( TokenValue, Token, FileLocToken,
					 _TokenExprTableAsList=[] ) ->
	ast_utils:display_error( "The current value '~p' of token '~p' could not "
		"be found in the switch_set_to/2 table specified at ~ts.",
		[ TokenValue, Token, ast_utils:file_loc_to_string( FileLocToken ) ] ),

	ast_utils:raise_error( { token_value_not_referenced, {value,TokenValue},
		{token,Token},
		ast_utils:file_loc_to_explicative_term( FileLocToken ) } );

% Target value found, regardless of its type in form:
find_expression_for( TokenValue, _Token, _FileLocToken,
		_TokenExprTableAsList=[ { tuple, _LTuple,
			[ {_ValueType,_L,TokenValue}, Expr ] } | _T ] ) ->
	Expr;

% Another value:
find_expression_for( TokenValue, Token, FileLocToken,
		_TokenExprTableAsList=[ { tuple, _TupleLoc,
			[ {_ValueType,_L,_OtherTokenValue}, _Expr ] } | T ] ) ->
	find_expression_for( TokenValue, Token, FileLocToken, T );

find_expression_for( _TokenValue, Token, _FileLocToken,
		_TokenExprTableAsList=[ { tuple, TupleLoc,
			[ UnexpectedValue, _Expr ] } | _T ] ) ->

	ast_utils:display_error( "Unexpected non-immediate value ('~p') "
		"for token '~p' in cond_utils:switch_set_to table/2 (at ~ts).",
		[ UnexpectedValue, Token, ast_utils:file_loc_to_string( TupleLoc ) ] ),

	ast_utils:raise_error( { non_immediate_token_value, {value,UnexpectedValue},
		{token,Token}, ast_utils:file_loc_to_explicative_term( TupleLoc ) } );

find_expression_for( _TokenValue, Token, FileLocToken,
		_TokenExprTableAsList=[ UnexpectedEntryForm | _T ] ) ->

	ast_utils:raise_error( { unexpected_entry_form, {form,UnexpectedEntryForm},
		{token,Token},
		ast_utils:file_loc_to_explicative_term( FileLocToken ) } ).



-doc """
Finds in the specified token-expression table the expression associated to the
specified token value (if referenced, otherwise tries with the specified default
value), and returns it.
""".
find_expression_for( TokenValue, DefaultValue, Token, FileLocToken,
					 TokenExprTableAsList ) ->
	find_expression_for( TokenValue, DefaultValue, Token, FileLocToken,
						 TokenExprTableAsList, _MaybeDefExpr=undefined ).


% (helper)
%
% Here we neither found the specified token value nor the default one in the
% table:
%
find_expression_for( TokenValue, DefaultValue, Token, FileLocToken,
					 _TokenExprTableAsList=[], _MaybeDefExpr=undefined ) ->

	ast_utils:display_error( "For token '~p' in cond_utils:switch_set_to/3 "
		"(~ts): neither its value (~p) nor the specified default one (~p) "
		"are referenced in specified table.",
		[ Token, ast_utils:file_loc_to_string( FileLocToken ), TokenValue,
		  DefaultValue ] ),

	ast_utils:raise_error( { unreferenced_values, {token_value,TokenValue},
		{default_value,DefaultValue}, {token,Token},
		ast_utils:file_loc_to_explicative_term( FileLocToken ) } );

% Here the token value was not found yet the default one was, so injecting the
% expression of this last one:
%
find_expression_for( _TokenValue, _DefaultValue, _Token, _FileLocToken,
					 _TokenExprTableAsList=[], DefExpr ) ->
	DefExpr;

% Here the token value is directly found:
find_expression_for( TokenValue, _DefaultValue, _Token, _FileLocToken,
		_TokenExprTableAsList=[ { tuple, _LTuple,
			[ {_ValueType,_L,TokenValue}, Expr ] } | _T ], _MaybeDefExpr ) ->
	Expr;

% Storing the expressions for this default value:
find_expression_for( TokenValue, DefaultValue, Token, FileLocToken,
		_TokenExprTableAsList=[ { tuple, _LTuple,
			[ {_ValueType,_L,DefaultValue}, Expr ] } | T ],
					 _MaybeDefExpr=undefined ) ->
	find_expression_for( TokenValue, DefaultValue, Token, FileLocToken, T,
						 Expr );

% Another value (ie not the token or default one):
find_expression_for( TokenValue, DefaultValue, Token, FileLocToken,
		_TokenExprTableAsList=[ { tuple, _LTuple,
			[ {_ValueType,_L,_OtherTokenValue}, _Expr ] } | T ],
		MaybeDefExpr ) ->
	find_expression_for( TokenValue, DefaultValue, Token, FileLocToken, T,
						 MaybeDefExpr );

find_expression_for( _TokenValue, _DefaultValue, Token, _FileLocToken,
		_TokenExprTableAsList=[ { tuple, TupleLoc,
			[ UnexpectedValue, _Expr ] } | _T ], _MaybeDefExpr ) ->

	ast_utils:display_error( "Unexpected non-immediate value ('~p') "
		"for token '~p' in cond_utils:switch_set_to table/3 (at ~ts).",
		[ UnexpectedValue, Token, ast_utils:file_loc_to_string( TupleLoc ) ] ),

	ast_utils:raise_error( { non_immediate_token_value, {value,UnexpectedValue},
		{token,Token}, ast_utils:file_loc_to_explicative_term( TupleLoc ) } );

find_expression_for( _TokenValue, _DefaultValue, Token, TokenLoc,
		_TokenExprTableAsList=[ UnexpectedEntryForm | _T ], _MaybeDefExpr ) ->

	ast_utils:raise_error( { unexpected_entry_form, {form,UnexpectedEntryForm},
		{token,Token}, ast_utils:file_loc_to_explicative_term( TokenLoc ) } ).
