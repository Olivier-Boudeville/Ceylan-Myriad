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
% Creation date: Friday, December 19, 2014.




% Gathering of various higher-level, convenient meta-related facilities, notably
% regarding metaprogramming, types and parse transforms.
%
% See meta_utils_test.erl for the corresponding test, and ast_info.erl for the
% more basic services used by this module.
%
% Note that this module is a prerequisite of at least most of our parse
% transforms, hence it must be bootstrapped *before* they are built, and cannot
% use them.
%
% So, to compile it, just go to the root of this layer and execute for example
% 'make all'.
%
% To determine the other bootstrapped modules (i.e. the subset of our modules
% that this module can use), see the BOOTSTRAP_MODULES variable in
% GNUmakevars.inc.

% See also: the type_utils module, about the management of datatypes themselves,
% and the ast* modules for lower-level operations.
%
-module(meta_utils).




% For table macro, etc.:
-include("meta_utils.hrl").


% For *_info records:
-include("ast_info.hrl").


% For ast_transforms record:
-include("ast_transform.hrl").




% Key implementation notes:
%
% - again: any exported function meant to be used by parse transforms shall rely
% exclusively (through all its code paths) on bootstrapped modules, as listed in
% the BOOTSTRAP_MODULES variable of GNUmakevars.inc
%
% - see type_utils about how to handle datatypes




% Implementation notes about parse transforms:

% Here are some resources to better understand parse transforms (PT, here):
%
% - generic information about PT: in http://www.erlang-factory.com/ :
% upload/presentations/521/yrashk_parse_transformations_sf12.pdf
%
% - Abstract Format: http://www.erlang.org/doc/apps/erts/absform.html (full spec
% of the AST format)
%
% - http://chlorophil.blogspot.fr/2007/04/erlang-macro-processor-v1-part-i.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-ii.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-iii.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-iv.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-v.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-vi.html
%   http://chlorophil.blogspot.fr/2007/04/atomiser-part-vii.html


% We consider here that an AST is an ordered list of forms.
%
% We often use located counterparts of the standard elements (ex: forms, ASTs)
% so that we can recreate and modify the order of (possibly transformed, added
% or removed) forms in an AST (that order matters, notably for its compilation),
% knowing that the embedded source-level line numbers are considerably less
% tractable and refer to another view onto the program at hand (and that the
% original forms are aggregated internally on a per-category basis rather than
% on a source-level one).
%
% See the definition of the location/0 type for further information.


% Standard modules of interest:
%
% - erl_scan ('The Erlang Token Scanner'): functions for tokenizing characters
% into Erlang tokens
%
% - epp ('An Erlang Code Preprocessor'): functions which are used by compile to
% preprocess macros and include files before the actual parsing
%
% - erl_parse ('The Erlang Parser'): basic Erlang parser
%
% - erl_eval ('The Erlang Meta Interpreter'): interpreter for Erlang
% expressions, in the abstract syntax
%
% - erl_pp ('The Erlang Pretty Printer'): to display abstract forms
%
% - erl_lint ('The Erlang Code Linter'): to check Erlang code for illegal
% syntax, bugs, unrecommended coding practices, etc.
%
% - compile ('The Erlang Compiler'): interface to the standard Erlang compiler

% Example of PT: http://www.erlang.org/doc/man/erl_id_trans.html


% Third-party libraries of interest:
%
% - https://github.com/uwiger/parse_trans
% - https://github.com/uwiger/toker


% Useful information about how to convert source code into actual code: on
% http://stackoverflow.com/questions/,
% 2160660/how-to-compile-erlang-code-loaded-into-a-string


% Use -P to see the code generated by a parse-transform; ex: 'erlc -P' or in the
% shell as 'c( "X.erl", [ 'P' ] )'.





% Options specified to a parse transform at runtime, like: report_warnings,
% beam,report_errors, {cwd,"X"}, {outdir,"Y"}, {i,"Z"}, {parse_transform,P},
% debug_info, warnings_as_errors, etc.
%
%
% (hence not a list_table, anyway not available here)
%
-type parse_transform_options() :: proplists:proplist().



%% Module subsection.

% The name of a module:
%
-type module_name() :: basic_utils:module_name().



%% Function subsection.


% The name of a function:
%
-type function_name() :: basic_utils:function_name().


% The arity of a function:
%
-type function_arity() :: arity().


% Declaration of a function based on a name with an arity (unique function
% signature within a module):
%
-type function_id() :: { function_name(), function_arity() }.


% The form corresponding to the definition of a clause of a function, typically
% { clause, LINE, Rep(Ps), Rep(Gs), Rep(B) } for '( Ps ) when Gs -> B':
%
-type clause_def() :: form().



% The full type specification (if any) of that function, as an abstract form;
% typically:
%
% { attribute, L, spec, { {foobar,Arity}, [{type,L,'fun', [{type,L,...
%
-type function_spec() :: form().



-export_type([ parse_transform_options/0,
			   module_name/0, function_name/0, function_id/0,
			   clause_def/0, function_spec/0,
			   function_info/0 ]).



% Local shorthands:

-type type_name() :: type_utils:type_name().
%-type type_arity() :: type_utils:type_arity().

-type form() :: ast_base:form().
-type ast_field_description() :: ast_utils:ast_field_description().


-type module_info() :: ast_info:module_info().
-type type_info() :: ast_info:type_info().
-type function_info() :: ast_info:function_info().



% Parse-transform related functions:
%
-export([ apply_ast_transforms/2,
		  add_function/2, remove_function/2,
		  add_type/2, remove_type/2,
		  update_types/2,
		  replace_types_in/2, update_types_in_functions/2,
		  update_calls_in_functions/2 ]).



% General functions, not operating an ASTs:
%
-export([ list_exported_functions/1, get_arities_for/2,
		  is_function_exported/3, check_potential_call/3 ]).




% Function addition/removal section.


% Registers specified function in specified module.
%
-spec add_function( function_info(), module_info() ) -> module_info().
add_function( FunInfo=#function_info{ exported=ExportLocs },
			  ModuleInfo=#module_info{ function_exports=ExportTable,
									   functions=FunTable } ) ->

	% Let's check first that the function is not already defined:
	FunId = { FunInfo#function_info.name, FunInfo#function_info.arity },

	case ?table:hasEntry( FunId, FunTable ) of

		true ->
			CurrentFunInfo = ?table:getEntry( FunId, FunTable ),
			CurrentFunString = ast_info:function_info_to_string(
								 CurrentFunInfo ),

			AddedFunString = ast_info:function_info_to_string( FunInfo ),

			ast_utils:display_error( "Function ~p already defined, as ~s, "
						   "whereas to be added, as ~s.",
						   [ FunId, CurrentFunString, AddedFunString ] ),

			throw( { function_already_defined, FunId } );

		false ->
			ok

	end,

	NewFunTable = ?table:addEntry( FunId, FunInfo, FunTable ),

	% Now updating the exports:
	NewExportTable = ast_info:ensure_function_exported( FunId, ExportLocs,
											  ModuleInfo, ExportTable ),

	ModuleInfo#module_info{ function_exports=NewExportTable,
							functions=NewFunTable }.




% Unregisters specified function from specified module.
%
-spec remove_function( function_info(), module_info() ) -> module_info().
remove_function( FunInfo=#function_info{ exported=ExportLocs },
				 ModuleInfo=#module_info{ function_exports=ExportTable,
										  functions=FunTable } ) ->

	FunId = { FunInfo#function_info.name, FunInfo#function_info.arity },

	% First forget its description:
	NewFunTable = case ?table:hasEntry( FunId, FunTable ) of

		true ->
			?table:removeEntry( FunId, FunTable );

		false ->
			throw( { non_existing_function_to_remove, FunId } )

	end,

	% Then its exports:
	NewExportTable = ast_info:ensure_function_not_exported( FunId, ExportLocs,
															ExportTable ),

	ModuleInfo#module_info{ function_exports=NewExportTable,
							functions=NewFunTable }.




% Type addition/removal section.



% Registers the specified, fully-described type in specified module.
%
-spec add_type( type_info(), module_info() ) -> module_info().
add_type( TypeInfo=#type_info{
					  variables=TypeVariables,
					  exported=ExportLocs },
		  ModuleInfo=#module_info{ type_exports=ExportTable,
								   types=TypeTable } ) ->

	Arity = length( TypeVariables ),

	% Let's check first that the type is not already defined:
	TypeId = { TypeInfo#type_info.name, Arity },

	case ?table:hasEntry( TypeId, TypeTable ) of

		true ->
			CurrentTypeInfo = ?table:getEntry( TypeId, TypeTable ),
			CurrentTypeString = ast_info:type_info_to_string( CurrentTypeInfo ),

			AddedTypeString = ast_info:type_info_to_string( TypeInfo ),

			ast_utils:display_error( "Type ~p already defined, as ~s, "
						   "whereas to be added, as ~s.",
						   [ TypeId, CurrentTypeString, AddedTypeString ] ),

			throw( { type_already_defined, TypeId } );

		false ->
			ok

	end,

	NewTypeTable = ?table:addEntry( TypeId, TypeInfo, TypeTable ),

	% Now updating the exports:
	NewExportTable = ast_info:ensure_type_exported( TypeId, ExportLocs,
													ModuleInfo, ExportTable ),

	ModuleInfo#module_info{ type_exports=NewExportTable,
							types=NewTypeTable }.




% Unregisters specified type from specified module.
%
-spec remove_type( type_info(), module_info() ) -> module_info().
remove_type( TypeInfo=#type_info{
						 variables=TypeVariables,
						 exported=ExportLocs },
			 ModuleInfo=#module_info{ type_exports=ExportTable,
									  types=TypeTable } ) ->

	Arity = length( TypeVariables ),

	TypeId = { TypeInfo#type_info.name, Arity },

	% First forget its description:
	NewTypeTable = case ?table:hasEntry( TypeId, TypeTable ) of

		true ->
			?table:removeEntry( TypeId, TypeTable );

		false ->
			throw( { non_existing_type_to_remove, TypeId } )

	end,

	% Then its exports:
	NewExportTable = ast_info:ensure_type_not_exported( TypeId, ExportLocs,
														ExportTable ),

	ModuleInfo#module_info{ type_exports=NewExportTable,
							types=NewTypeTable }.






% Section about type updating.



% Updates the type in specified type table, according to the specified
% transformation.
%
-spec update_types( ast_info:type_table(), ast_transform:ast_transforms() ) ->
						  ast_info:type_table().
update_types( TypeTable,
			  #ast_transforms{ local_types=MaybeLocalTypeTable,
							   remote_types=MaybeRemoteTypeTable } ) ->

	% Closure:
	UpdaterFun = fun( TypeInfo ) ->

						 update_types_in( TypeInfo, MaybeLocalTypeTable,
										  MaybeRemoteTypeTable )

				 end,

	% Returns NewTypeTable:
	?table:mapOnValues( UpdaterFun, TypeTable ).



% Updates the specified type information based on the two specified type
% transformation tables (if any).
%
% (helper)
%
update_types_in( TypeInfo, _MaybeLocalTypeTable=undefined,
				 _MaybeRemoteTypeTable=undefined ) ->
	TypeInfo;

update_types_in( TypeInfo=#type_info{ definition=TypeDef }, MaybeLocalTypeTable,
				 MaybeRemoteTypeTable ) ->
	% Ex: Def = {type,42,tuple,[{type,42,integer,[]},{type,42,float,[]}]}
	NewDef = ast_scan:scan_type( TypeDef, MaybeLocalTypeTable,
								 MaybeRemoteTypeTable ),
	TypeInfo#type_info{ definition=NewDef }.



% Replaces local and remote types in specified located AST according to the
% specified transformation information.
%
-spec replace_types_in( ast_info:located_ast(),
					ast_transform:ast_transforms() ) -> ast_info:located_ast().
replace_types_in( InputLocatedAST, #ast_transforms{
									  local_types=MaybeLocalTypeTable,
									  remote_types=MaybeRemoteTypeTable } ) ->

	%ast_utils:display_debug( "Local type replacement table: ~s",
	%	   [ ?table:toString( Replacements#ast_transforms.local_types ) ] ),

	%ast_utils:display_debug( "Remote type replacement table: ~s",
	%	   [ ?table:toString( Replacements#ast_transforms.remote_types ) ] ),

	OutputLocatedAST = replace_types_helper( InputLocatedAST,
		  MaybeLocalTypeTable, MaybeRemoteTypeTable, _Acc=[] ),

	%ast_utils:display_debug( "AST after type replacement:~n~s",
	%			   [ located_ast_to_string( OutputLocatedAST ) ] ),

	OutputLocatedAST.



% (helper)
replace_types_helper( _InputLocatedAST=[], _MaybeLocalTypeTable,
					  _MaybeRemoteTypeTable, Acc ) ->
	Acc;

replace_types_helper( _InputLocatedAST=[ { Loc, Form } | T ],
					  MaybeLocalTypeTable, MaybeRemoteTypeTable, Acc ) ->

	NewForm = replace_types_in_type_def( Form, MaybeLocalTypeTable,
										 MaybeRemoteTypeTable ),

	replace_types_helper( T, MaybeLocalTypeTable, MaybeRemoteTypeTable,
						  [ { Loc, NewForm } | Acc ] ).




% (helper)
replace_types_in_type_def( _Form={ attribute, Line, type,
								   { TypeName, TypeDef, TypeVars } },
						   MaybeLocalTypeTable, MaybeRemoteTypeTable ) ->

	NewTypeDef = ast_scan:scan_type( TypeDef, MaybeLocalTypeTable,
									 MaybeRemoteTypeTable ),

	NewTypeVars = [ ast_scan:scan_type( Elem, MaybeLocalTypeTable,
								MaybeRemoteTypeTable ) || Elem <- TypeVars ],

	%ast_utils:display_debug(
	%  "Translation of type definition:~n~p~nis:~n~p~nwith ~p.",
	%			   [ TypeDef, NewTypeDef, NewTypeVars ] ),

	{ attribute, Line, type, { TypeName, NewTypeDef, NewTypeVars } };


replace_types_in_type_def( _Form={ attribute, Line, opaque,
								   { TypeName, TypeDef, TypeVars } },
						   MaybeLocalTypeTable, MaybeRemoteTypeTable ) ->

	NewTypeDef = ast_scan:scan_type( TypeDef, MaybeLocalTypeTable,
									 MaybeRemoteTypeTable ),

	NewTypeVars = [ ast_scan:scan_type( Elem, MaybeLocalTypeTable,
								MaybeRemoteTypeTable ) || Elem <- TypeVars ],

	%ast_utils:display_debug(
	%  "Translation of opaque type definition:~n~p~nis:~n~p~n"
	%               "with ~p.", [ TypeDef, NewTypeDef, NewTypeVars ] ),

	{ attribute, Line, opaque, { TypeName, NewTypeDef, NewTypeVars } };


replace_types_in_type_def( _Form={ attribute, Line, record,
								   { TypeName, Fields } },
						   MaybeLocalTypeTable, MaybeRemoteTypeTable ) ->

	NewFields = update_types_in_fields( Fields, MaybeLocalTypeTable,
										MaybeRemoteTypeTable ),

	%ast_utils:display_debug(
	%  "Translation of record field definitions:~n~p~nis:~n~p~n.",
	%               [ Fields, NewFields ] ),

	{ attribute, Line, record, { TypeName, NewFields } };


replace_types_in_type_def( UnexpectedForm, _MaybeLocalTypeTable,
						   _MaybeRemoteTypeTable ) ->
	ast_utils:raise_error( [ unexpected_typedef_form, UnexpectedForm ] ).





% Section about function updating.



% Updates the types in known functions from specified function table, based on
% specified replacements.
%
-spec update_types_in_functions( ast_info:function_table(),
				ast_transforms:ast_transforms() ) -> ast_info:function_table().
update_types_in_functions( FunctionTable, #ast_transforms{
							  local_types=MaybeLocalTypeTable,
							  remote_types=MaybeRemoteTypeTable } ) ->

	FunIdInfoPairs = ?table:enumerate( FunctionTable ),

	NewFunIdInfoPairs = [ { FunId, update_fun_info_for_types( FunInfo,
								MaybeLocalTypeTable, MaybeRemoteTypeTable ) }
						  || { FunId, FunInfo } <- FunIdInfoPairs ],

	?table:new( NewFunIdInfoPairs ).



% Updates the types in the -spec fields, based on specified replacements.
%
update_fun_info_for_types( FunInfo=#function_info{ spec=undefined },
						   _MaybeLocalTypeTable, _MaybeRemoteTypeTable ) ->
	FunInfo;

update_fun_info_for_types( FunInfo=#function_info{ spec={ Loc, FunSpec } },
						   MaybeLocalTypeTable, MaybeRemoteTypeTable ) ->

	NewFunSpec = case FunSpec of

		% Ex for '-spec f( type_a() ) -> type_b().':
		% SpecList = [ {type,652,'fun',
		%     [{type,652,product,[{user_type,652,type_a,[]}]},
		%       {user_type,652,type_b,[]}]
		%   } ]
		{ attribute, Line, spec, { FunId, SpecList } } ->
			%ast_utils:display_trace( "SpecList = ~p", [ SpecList ] ),
			NewSpecList = [ update_spec( Spec, MaybeLocalTypeTable,
								 MaybeRemoteTypeTable ) || Spec <- SpecList ],
			{ attribute, Line, spec, { FunId, NewSpecList } };

		_ ->
			ast_utils:raise_error( [ unexpected_fun_spec, FunSpec ] )

	end,

	FunInfo#function_info{ spec={ Loc, NewFunSpec } };


update_fun_info_for_types( _FunInfo=#function_info{ spec=UnexpectedLocSpec },
				  _MaybeLocalTypeTable, _MaybeRemoteTypeTable ) ->
	ast_utils:raise_error( [ unexpected_located_fun_spec, UnexpectedLocSpec ] ).




% Updates the specified function specification.
%
update_spec( { type, Line, 'fun', ClausesSpecs }, MaybeLocalTypeTable,
			 MaybeRemoteTypeTable ) ->

	NewClausesSpecs = update_clause_spec( ClausesSpecs, MaybeLocalTypeTable,
										  MaybeRemoteTypeTable ),
	{ type, Line, 'fun', NewClausesSpecs };

update_spec( UnexpectedFunSpec, _MaybeLocalTypeTable, _MaybeRemoteTypeTable ) ->
	ast_utils:raise_error( [ unexpected_fun_spec, UnexpectedFunSpec ] ).



% (helper)
update_clause_spec( [ { type, Line, product, ParamTypes }, ResultType ],
					MaybeLocalTypeTable, MaybeRemoteTypeTable ) ->

	NewParamTypes = [ ast_scan:scan_type( ParamType, MaybeLocalTypeTable,
						  MaybeRemoteTypeTable ) || ParamType <- ParamTypes ],

	NewResultType = ast_scan:scan_type( ResultType, MaybeLocalTypeTable,
										MaybeRemoteTypeTable ),

	[ { type, Line, product, NewParamTypes }, NewResultType ];


update_clause_spec( UnexpectedClauseSpec, _MaybeLocalTypeTable,
					_MaybeRemoteTypeTable ) ->
	ast_utils:raise_error( [ unexpected_clause_spec, UnexpectedClauseSpec ] ).




% Updates the types in specified record fields, based on specified replacements.
%
-spec update_types_in_fields( [ ast_field_description() ],
			ast_transform:local_type_transform_table(),
			ast_transform:remote_type_transform_table()  ) ->
									[ ast_field_description() ].
update_types_in_fields( Fields, MaybeLocalTypeTable, MaybeRemoteTypeTable ) ->

	%ast_utils:display_debug( "Input fields: ~p.", [ Fields ] ),

	NewFields = [ update_types_in_field( F, MaybeLocalTypeTable,
							   MaybeRemoteTypeTable ) || F <- Fields ],

	%ast_utils:display_debug( "New fields: ~p.", [ NewFields ] ),

	NewFields.



% (helper)
%
-spec update_types_in_field( ast_field_description(),
		ast_transform:local_type_transform_table(),
		ast_transform:remote_type_transform_table() ) ->
								   ast_field_description().
% Type specified, without or with a default value:
update_types_in_field( _F={ typed_record_field,
		   % { record_field, _Line1, { atom, _Line2, _FieldName } },
		   %  - or -
		   % { record_field, _Line1, { atom, _Line2, _FieldName },
		   %		{ _ImmediateType, Line2, DefaultValue } }:
		   RecordField,
		   %{ type, Line3, TypeName, TypeVars } }:
		   TypeDef }, MaybeLocalTypeTable, MaybeRemoteTypeTable ) ->

	NewTypeDef = ast_scan:scan_type( TypeDef, MaybeLocalTypeTable,
									 MaybeRemoteTypeTable ),

	{ typed_record_field, RecordField, NewTypeDef };


% No type and no default value specified:
update_types_in_field( F={ record_field, _Line1,
						   % { atom, Line2, FieldName }:
						   _FieldNameDef },
					   _MaybeLocalTypeTable, _MaybeRemoteTypeTable ) ->
	F;

% No type specified, yet with a default value:
update_types_in_field( F={ record_field, _Line1,
						   % { atom, Line2, FieldName }:
						   _FieldNameDef,
						   % { _ImmediateType, Line2, DefaultValue }:
						   _DefaultValueDef },
					   _MaybeLocalTypeTable, _MaybeRemoteTypeTable ) ->
	F;


update_types_in_field( F, _MaybeLocalTypeTable, _MaybeRemoteTypeTable ) ->
	ast_utils:raise_error( [ unexpected_record_field, F ] ).





% Handling record-related types.


% Annotated type, for example found in a record field like:
%  pointDrag :: {X::integer(), Y::integer()}}
%
% Resulting then in:
% {typed_record_field,
%		   {record_field,342,{atom,342,pointDrag}},
%		   {type,342,tuple,
%			   [{ann_type,342,[{var,342,'X'},{type,342,integer,[]}]},
%				{ann_type,342,
%					[{var,342,'Y'},{type,342,integer,[]}]} ] }}
%
%% scan_type( _TypeDef={ ann_type, Line, [ Var, InternalTypeDef ] },
%%			   LocalTransformTable, RemoteTransformTable ) ->

%%	NewInternalTypeDef = scan_type( InternalTypeDef, LocalTransformTable,
%%										RemoteTransformTable ),

%%	{ ann_type, Line, [ Var, NewInternalTypeDef ] };



% Updates the calls in known functions from specified function table, based on
% specified replacements.
%
-spec update_calls_in_functions( ast_info:function_table(),
				ast_transform:ast_transforms() ) -> ast_info:function_table().
update_calls_in_functions( FunctionTable, Transforms ) ->

	FunIdInfoPairs = ?table:enumerate( FunctionTable ),

	NewFunIdInfoPairs = [ { FunId,
							update_fun_info_for_calls( FunInfo, Transforms ) }
						  || { FunId, FunInfo } <- FunIdInfoPairs ],

	?table:new( NewFunIdInfoPairs ).



% Updates the calls in the function definitions, based on specified
% replacements.
%
update_fun_info_for_calls( FunInfo=#function_info{ definition=ClauseDefs },
						   Transforms ) ->

	% Top-level function clauses are apparently the same as 'case', 'receive',
	% etc. clauses:
	%
	NewClauseDefs = [ ast_scan:scan_expression( ClauseDef, Transforms )
					  || ClauseDef <- ClauseDefs ],

	FunInfo#function_info{ definition=NewClauseDefs }.





% Applies specified AST transformations to specified module information.
%
% (helper)
%
-spec apply_ast_transforms( ast_transform:ast_transforms(), module_info() ) ->
								  module_info().
apply_ast_transforms( Transforms, ModuleInfo ) ->

	% First, update the type definitions accordingly (including in records):

	ast_utils:display_debug( "Transforming known types..." ),
	NewTypes = update_types( ModuleInfo#module_info.types, Transforms ),

	ast_utils:display_debug( "Transforming known types in records..." ),
	NewRecordDefs = replace_types_in( ModuleInfo#module_info.record_defs,
									  Transforms ),

	% Do the same for types in function (type) specifications:
	ast_utils:display_debug( "Transforming known types in function specs..." ),
	TypedFunctionTable = update_types_in_functions(
						   ModuleInfo#module_info.functions, Transforms ),

	% And then in related function definitions:
	ast_utils:display_debug( "Transforming function calls..." ),
	CallFunctionTable = update_calls_in_functions( TypedFunctionTable,
												   Transforms ),

	% Updated module_info returned:
	ModuleInfo#module_info{ types=NewTypes,
							record_defs=NewRecordDefs,
							functions=CallFunctionTable }.



% Lists (in the order of their definition) all the functions ({Name,Arity}) that
% are exported by the specified module, expected to be found in the code path.
%
-spec list_exported_functions( basic_utils:module_name() ) ->
									 [ function_id() ].
list_exported_functions( ModuleName ) ->

	% To avoid a unclear message like 'undefined function XXX:module_info/1':
	case code_utils:is_beam_in_path( ModuleName ) of

		not_found ->
			throw( { module_not_found_in_path, ModuleName } );

		_ ->
			ok

	end,

	ModuleName:module_info( exports ).



% Returns a list of the arities for which the specified function of the
% specified module is exported.
%
-spec get_arities_for( basic_utils:module_name(), function_name() ) ->
							 [ arity() ].
get_arities_for( ModuleName, FunctionName ) ->

	ExportedFuns = list_exported_functions( ModuleName ),

	% Match on FunctionName:
	[ Arity || { Name, Arity } <- ExportedFuns, Name =:= FunctionName ].



% Tells whether the specified function (name with arity) is exported by the
% specified module.
%
-spec is_function_exported( basic_utils:module_name(), function_name(),
							arity() ) -> boolean().
is_function_exported( ModuleName, FunctionName, Arity ) ->
	lists:member( { FunctionName, Arity },
				  list_exported_functions( ModuleName ) ).



% Checks whether a potential upcoming call to the specified MFA
% (Module,Function,Arguments) has a chance of succeeding.
%
-spec check_potential_call( basic_utils:module_name(), function_name(),
		[ basic_utils:argument() ] ) ->
				'ok' | 'module_not_found' | 'function_not_exported'.
check_potential_call( ModuleName, FunctionName, Arguments )
  when is_atom( ModuleName ) andalso is_atom( FunctionName )
	   andalso is_list( Arguments ) ->

	case code_utils:is_beam_in_path( ModuleName ) of

		not_found ->
			module_not_found;

		_ ->
			Arity = length( Arguments ),
			case is_function_exported( ModuleName, FunctionName, Arity ) of

				true ->
					ok;

				false ->
					function_not_exported

			end

	end;

check_potential_call( ModuleName, FunctionName, Arguments ) ->

	case is_atom( ModuleName ) of

		true ->
			ok;

		false ->
			throw( { non_atom_module_name, ModuleName } )

	end,

	case is_atom( FunctionName ) of

		true ->
			ok;

		false ->
			throw( { non_atom_function_name, FunctionName } )

	end,

	% Only remaining possibility:
	throw( { non_list_arguments, Arguments } ).
