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




% Gathering of various convenient meta-related facilities, notably regarding
% metaprogramming, types and parse transforms.
%
% See meta_utils_test.erl for the corresponding test.
%
% Note that this module is a prerequisite of at least most of our parse
% transforms, hence it must be bootstrapped *before* they are built, and cannot
% use them.
%
% So, to compile it, just go to the root of this layer and execute for example
% 'make all'.
%
% To known the other bootstrapped modules (i.e. the subset of our modules that
% this module can use), see the BOOTSTRAP_MODULES variable in GNUmakevars.inc.
%
% See also: the type_utils module, about the management of datatypes themselves.
%
-module(meta_utils).


% This module being a bootstrap one, the 'table' pseudo-module is not available
% (as this module is not processed by the 'Common' parse transform):
%
% Indeed, no table pseudo-module available from meta_utils, as it cannot be
% parse-transformed; only ?table is available here, not the other *_hashtable
% counterparts (once that meta_utils module is compiled, if it relied on
% foo_hashtable, then the parse transform could not operate on any module
% compiled before foo_hashtable):
%
-define( table, map_hashtable ).



% Shorthands:
-type module_name() :: basic_utils:module_name().

-type type_name() :: type_utils:type_name().
-type type_arity() :: type_utils:type_arity().

-type ast_field_description() :: ast_utils:ast_field_description().


% Not expected to be legit symbols:
%
-define( any_module_name, '_' ).

-type module_name_match() :: module_name() | ?any_module_name.




%% Type replacement section.

-define( any_type_name,  '_' ).
-define( any_type_arity, '_' ).


-type type_name_match()  :: type_name()  | ?any_type_name.
-type type_arity_match() :: type_arity() | ?any_type_arity.


% The same arity is kept, and just specifying the module name means that the
% type name is not to change.
%
% Note that this implies that a (local or remote) type can only be replaced by a
% remote type (a priori not a problematic limitation).
%
-type type_replacement() :: { module_name(), type_name() } | module_name().


% Local subsection:

-type local_type_id_match() :: { type_name_match(), type_arity_match() }.

% Either we directly set the target module and type names (using same arity), or
% we apply an anonymous function to determine the corresponding information,
% based on context:
-type local_type_replacement() :: type_replacement()
			| fun( ( type_name(), type_arity() ) -> type_replacement() ).


% Table defining replacements of local types:
-type local_type_replacement_table() :: ?table:?table( local_type_id_match(),
												   local_type_replacement() ).


% Remote subsection:

-type remote_type_id_match() :: { module_name_match(), type_name_match(),
								  type_arity_match() }.

% Either we directly set the target module and type names (using same arity), or
% we apply an anonymous function to determine the corresponding information,
% based on context:
-type remote_type_replacement() :: type_replacement()
			 | fun( ( module_name(), type_name(), type_arity() ) ->
								type_replacement() ) .


% Table defining replacements of remote types:
-type remote_type_replacement_table() :: ?table:?table( remote_type_id_match(),
													remote_type_replacement() ).




%% Call replacement section.

-define( any_function_name,  '_' ).
-define( any_function_arity, '_' ).


-type function_name_match()  :: function_name() | ?any_function_name.
-type function_arity_match() :: arity()         | ?any_function_arity.


% The same arity is kept, and just specifying the module name means that the
% function name of the call is not to change.
%
% Note that this implies that a (local or remote) call can only be replaced by a
% remote call (a priori not a problematic limitation).
%
-type call_replacement() :: { module_name(), function_name() } | module_name().




% Local subsection:

-type local_call_match() :: { function_name_match(), function_arity_match() }.

% Either we directly set the target module and function names (using same
% arity), or we apply an anonymous function to determine the corresponding
% information, based on context:
%
-type local_call_replacement() :: call_replacement()
			 | fun( ( function_name(), arity() ) -> call_replacement() ) .


% Table defining replacements of local call:
-type local_call_replacement_table() :: ?table:?table( local_call_match(),
												local_call_replacement() ).


% Remote subsection:

-type remote_call_match() :: { module_name_match(), function_name_match(),
							   function_arity_match() }.

% Either we directly set the target module and function names (using same
% arity), or we apply an anonymous function to determine the corresponding
% information, based on context:
%
-type remote_call_replacement() :: call_replacement()
			 | fun( ( module_name(), function_name(), arity() ) ->
								call_replacement() ) .


% Table defining replacements of remote call:
-type remote_call_replacement_table() :: ?table:?table( remote_call_match(),
												remote_call_replacement() ).




% Key implementation notes:
%
% - again: any exported function meant to be used by parse transforms shall rely
% exclusively (through all its code paths) on bootstrapped modules (including
% {basic_utils, id, meta, text}_utils and map_hashtable), like defined in the
% BOOTSTRAP_MODULES variable of GNUmakevars.inc
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


% Useful information:
%
% - how to convert source code into actual code: on
% http://stackoverflow.com/questions/,
% 2160660/how-to-compile-erlang-code-loaded-into-a-string


% Use -P to see the code generated by a parse-transform; ex: 'erlc -P' or in the
% shell as 'c( "X.erl", [ 'P' ] )'.



% For function_info:
-include("meta_utils.hrl").


% For the file_info record:
-include_lib("kernel/include/file.hrl").



% Options specified to a parse transform at runtime, like report_warnings,
% beam,report_errors, {cwd,"X"}, {outdir,"Y"}, {i,"Z"}, {parse_transform,P},
% debug_info,warnings_as_errors, etc.
%
-type parse_transform_options() :: proplists:proplist().


% Abstract form, part of an AST (ex: {attribute,40,file,{"foo.erl",40}}):
%
-type form() :: erl_parse:abstract_form().


% Abstract Syntax Tree, standard representation of parse trees for Erlang
% programs as Erlang terms. This representation is known as the abstract format.
%
% For more information: http://www.erlang.org/doc/apps/erts/absform.html
%
-type ast() :: [ form() ].



% Location of a form in an AST, so that the order of forms can be recreated.
%
% We use sortable identifiers so that any number of new forms can be introduced
% between any two of them, if needed.
%
% Location is relative to the position of a form in a given AST, while the line
% information embedded in forms is relative to the file in which they are
% defined.
%
% 'auto_located' means that the corresponding form is yet to be located at this
% position (thus a tranformation pass is still to be applied to needed in the
% overall list before it is sortable).
%
-type location() :: id_utils:sortable_id() | 'auto_located'.



% When processing an AST (ex: read from a BEAM file), the order of the forms
% matters (for example to report compile errors, which are relative to a context
% defined by the last '-file' attribute previously encountered, i.e. like
% {attribute,40,file,{"foo.erl",40}}). So even if we store forms in tables
% according to their type, when (re)generating the AST we have to recreate the
% same order.
%
% To do so, instead of managing a list of forms, we manage any sets of located
% forms by including in each form an identifier allowing to recreate the form
% order in the original AST.
%
-type located_form() :: { location(), form() }.


% An AST including location information:
%
-type located_ast() :: [ located_form() ].



% The name of a (parse-level) attribute (ex: '-my_attribute( my_value ).').
%
-type attribute_name() :: atom().


% The value of a (parse-level) attribute (ex: '-my_attribute( my_value ).').
%
-type attribute_value() :: term().



% Parse-level attribute:
%
-type attribute() :: { attribute_name(), attribute_value() }.


% For easy access to parse attributes:
%
-type attribute_table() :: ?table:?table( attribute_name(), attribute_value() ).



-type compile_option_name() :: atom().

-type compile_option_value() :: term().

% For easy access to compilation information:
%
-type compile_option_table() :: ?table:?table( compile_option_name(),
											   [ compile_option_value() ] ).



% The name of a function:
%
-type function_name() :: basic_utils:function_name().


% Declaration of a function based on a name with an arity (unique function
% signature within a module):
%
-type function_id() :: { function_name(), arity() }.


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


% Located type specification of a function:
%
-type located_function_spec() :: { location(), function_spec() }.


% All information regarding a function:
-type function_info() :: #function_info{}.


% All information regarding AST replacements:
-type ast_replacements() :: #ast_replacements{}.



% A table associating, to a given location, the corresponding line in the source
% file (to recreate the corresponding export form) and a list of the identifiers
% of the functions to declare exported there.
%
% Note:
%
% - this table must be explicitly updated whenever adding or removing a function
% in a module_info'functions' field; see: add_function/2 and remove_function/2
%
% - [ function_id() ] used, not a set, to better preserve order
%
-type export_table() :: ?table:?table( location(),
						   { ast_utils:line(), [ meta_utils:function_id() ] } ).


% A table referencing, for each module listed, a list of the functions that are
% imported from it by the current module:
%
-type import_table() :: ?table:?table( basic_utils:module_name(),
									   [ meta_utils:function_id() ] ).



% A table associating to each function identifier a full function information.
%
-type function_table() :: ?table:?table( meta_utils:function_id(),
										 meta_utils:function_info() ).


% A table associating to each record name the list of the descriptions of its
% fields.
%
-type record_table() :: ?table:?table( basic_utils:record_name(),
									   field_table() ).


% A table associating to a given field of a record its description (type and
% default value, if specified).
%
-type field_table() :: ?table:?table( basic_utils:field_name(),
		  { basic_utils:maybe( ast_utils:ast_type() ),
			basic_utils:maybe( ast_utils:ast_immediate_value() ) } ).


% Type of functions to transform terms during a recursive traversal (see
% traverse_term/4).
%
% Note: apparently we cannot use the 'when' notation here (InputTerm ... when
% InputTerm :: term()).
%
-type term_transformer() :: fun( ( term(), basic_utils:user_data() ) ->
									   { term(), basic_utils:user_data() } ).



-type module_info() :: #module_info{}.



% Directly inspired from erl_lint:


% Description of a compilation-related issue (error or warning).
%
-type issue_description() :: term().


% Full information about a compilation-related issue.
%
% The module is the one emitting that issue (ex: erl_lint)
%
-type issue_info() :: { ast_utils:line(), module(), issue_description() }.


% A warning regarding a source file, corresponding to a list of error
% informations.
%
-type issue_report() :: { file_utils:file_name(), [ issue_info() ] }.


% For type replacements:
-export_type([ module_name_match/0, type_name_match/0, type_arity_match/0,
			   type_replacement/0,
			   local_type_id_match/0, local_type_replacement/0,
			   local_type_replacement_table/0,
			   remote_type_id_match/0, remote_type_replacement/0,
			   remote_type_replacement_table/0 ]).


% For call replacements:
-export_type([ function_name_match/0, function_arity_match/0,
			   local_call_match/0, remote_call_match/0,
			   call_replacement/0, local_call_replacement_table/0,
			   remote_call_replacement_table/0 ]).


-export_type([ parse_transform_options/0, form/0, ast/0,
			   location/0, located_form/0, located_ast/0,
			   attribute_name/0, attribute_value/0, attribute/0,
			   attribute_table/0,

			   function_name/0, function_id/0,
			   clause_def/0, function_spec/0, located_function_spec/0,
			   function_info/0,
			   ast_replacements/0,
			   term_transformer/0, module_info/0,
			   issue_description/0, issue_info/0, issue_report/0 ]).



% Parse-transform related functions:
%
-export([ init_module_info/0, pre_check_ast/1,
		  add_function/2, remove_function/2,
		  function_info_to_string/1,
		  located_ast_to_string/1,

		  get_local_type_replacement_table/1,
		  get_remote_type_replacement_table/1,
		  replace_types_in/2, update_types_in_functions/2,

		  get_local_call_replacement_table/1,
		  get_remote_call_replacement_table/1,
		  update_calls_in_functions/2,

		  traverse_term/4,
		  term_to_form/1, variable_names_to_ast/2,
		  string_to_form/1, string_to_form/2,
		  string_to_expressions/1, string_to_expressions/2,
		  string_to_value/1,
		  beam_to_ast/1,
		  extract_module_info_from_ast/1, recompose_ast_from_module_info/1,
		  erl_to_ast/1,
		  check_module_info/1, module_info_to_string/1,
		  write_ast_to_file/2, write_module_info_to_file/2,
		  raise_error/1, get_error_form/3, format_error/1 ]).


% General functions:
%
-export([ list_exported_functions/1, get_arities_for/2,
		  is_function_exported/3, check_potential_call/3 ]).



% For debugging:
-export([ interpret_issue_reports/1, interpret_issue_report/1,
		  interpret_issue_info/2, interpret_issue_description/2 ] ).


% To silence unused warnings:
-export([ display_debug/1, display_debug/2,
		  display_trace/1, display_trace/2,
		  display_info/1, display_info/2,
		  display_warning/1, display_warning/2,
		  display_error/1, display_error/2,
		  display_fatal/1, display_fatal/2 ]).



% Returns a new, blank instance of the module_info record, typically to be fed
% with an input AST afterwards.
%
-spec init_module_info() -> module_info().
init_module_info() ->

	EmptyTable = ?table:new(),

	#module_info{ compilation_options=EmptyTable,
				  parse_attributes=EmptyTable,
				  records=EmptyTable,
				  function_imports=EmptyTable,
				  function_exports=EmptyTable,
				  functions=EmptyTable }.



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
			CurrentFunString = function_info_to_string( CurrentFunInfo ),

			AddedFunString = function_info_to_string( FunInfo ),

			display_error( "Function ~p already defined, as ~s, "
						   "whereas to be added as ~s.",
						   [ FunId, CurrentFunString, AddedFunString ] ),

			throw( { function_already_defined, FunId } );

		false ->
			ok

	end,

	NewFunTable = ?table:addEntry( FunId, FunInfo, FunTable ),

	% Now updating the exports:
	NewExportTable = ensure_exported( FunId, ExportLocs, ExportTable,
									  ModuleInfo ),

	ModuleInfo#module_info{ function_exports=NewExportTable,
							functions=NewFunTable }.



% Ensures that specified function is exported at the specified location(s).
%
-spec ensure_exported( function_id(), [ location() ], module_info(),
					   export_table() ) -> export_table().
ensure_exported( _FunId, _ExportLocs=[], _ModuleInfo, ExportTable ) ->
	ExportTable;

ensure_exported( FunId, _ExportLocs=[ _Loc=auto_located | T ], ModuleInfo,
				 ExportTable ) ->

	% When a function export is to be auto-located, we attach it just after the
	% module definition, to avoid possibly placing an export after a function
	% definition:

	ModuleLoc = case ModuleInfo#module_info.module_def of

		undefined ->
			throw( { auto_locate_whereas_no_module_def, FunId } );

		{ MLoc, _Form } ->
			MLoc

	end,

	FunLoc = id_utils:get_higher_next_depth_sortable_id( ModuleLoc ),

	% This location may have already been used, thus:
	case ?table:lookupEntry( FunLoc, ExportTable ) of

		{ value, { Line, FunIds } } ->

			case lists:member( FunId, FunIds ) of

				true ->
					% Already registered, perfect as is:
					ensure_exported( FunId, T, ModuleInfo, ExportTable );

				false ->
					% Adding it then:
					NewEntry = { Line, [ FunId | FunIds ] },
					NewExportTable = ?table:addEntry( ModuleLoc, NewEntry),
					ensure_exported( FunId, T, ModuleInfo, NewExportTable )

			end;

		key_not_found ->
			% We create a new location entry then:
			NewEntry = { _DefaultLine=0, [ FunId ] },
			NewExportTable = ?table:addEntry( ModuleLoc, NewEntry),
			ensure_exported( FunId, T, ModuleInfo, NewExportTable )

	end;


ensure_exported( FunId, _ExportLocs=[ Loc | T ], ModuleInfo, ExportTable ) ->

	case ?table:lookupEntry( Loc, ExportTable ) of

		{ value, { Line, FunIds } } ->

			case lists:member( FunId, FunIds ) of

				true ->
					% Already registered, perfect as is:
					ensure_exported( FunId, T, ModuleInfo, ExportTable );

				false ->
					% Adding it then:
					NewEntry = { Line, [ FunId | FunIds ] },
					NewExportTable = ?table:addEntry( Loc, NewEntry),
					ensure_exported( FunId, T, ModuleInfo, NewExportTable )

			end;

		key_not_found ->
			% Not even a registered location:
			throw( { invalid_export_location, Loc, FunId } )

	end.



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
	NewExportTable = ensure_not_exported( FunId, ExportLocs, ExportTable ),

	ModuleInfo#module_info{ function_exports=NewExportTable,
							functions=NewFunTable }.



% Ensures that specified function is exported at the specified location(s).
%
-spec ensure_not_exported( function_id(), [ location() ], export_table() ) ->
								 export_table().
ensure_not_exported( _FunId, _ExportLocs=[], ExportTable ) ->
	ExportTable;

ensure_not_exported( FunId, _ExportLocs=[ Loc | T ], ExportTable ) ->

	case ?table:lookupEntry( Loc, ExportTable ) of

		{ value, { Line, FunIds } } ->

			% 0 or 1 reference expected, which is handled the same by:
			NewFunIds = lists:delete( FunId, FunIds ),
			NewExportTable = ?table:addEntry( Loc, { Line, NewFunIds } ),
			ensure_not_exported( FunId, T, NewExportTable );

		key_not_found ->
			throw( { inconsistent_export_location, Loc, FunId } )

	end.



% Returns a textual description of the specified function information.
%
-spec function_info_to_string( function_info() ) -> text_utils:ustring().
function_info_to_string( #function_info{ name=Name,
										 arity=Arity,
										 location=_Location,
										 definition=Clauses,
										 spec=LocatedSpec,
										 exported=Exported } ) ->

	ExportString = case Exported of

		undefined ->
			"local";

		ExportLoc ->
			text_utils:format( "exported in ~s",
							   [ id_utils:sortable_id_to_string( ExportLoc ) ] )

	end,

	DefString = io_lib:format( "~B clause(s) defined", [ length( Clauses ) ] ),

	SpecString = case LocatedSpec of

		undefined ->
			"no type specification";

		_ ->
			"a type specification"

	end,

	io_lib:format( "~s/~B, ~s, with ~s and ~s",
				   [ Name, Arity, ExportString, DefString, SpecString ] ).




%% Type replacement section.



% Returns a table describing local type replacements.
%
% Ex: [ { { void, 0 }, basic_utils },
%       { { my_maybe, 1 }, { basic_utils, maybe } },
%       % First clause will never match due to arity:
%       { { '_', 3 }, fun( other_void, 0 ) ->
%                                    other_utils;
%                        ( _, '_' ) ->
%                                   {foo_utils,some_type}
%                     end }
% ]
%
% will return a description of the transformation of:
%
%  - void() into basic_utils:void(), as the same type name is implied there; it
%  is just the addition (prefix) of a module, as a remote type
%
%  - my_maybe(T) into basic_utils:maybe(T)
%
%  - other_void() into other_utils:other_void()
%
%  - any type depending on three others by foo_utils:some_type/3
%
-spec get_local_type_replacement_table(
		[ { local_type_id_match(), type_replacement() } ] ) ->
				local_type_replacement_table().
get_local_type_replacement_table( Replacements ) ->
	EmptyTable = ?table:new(),
	get_local_type_repl_helper( Replacements, EmptyTable ).



% (helper)
get_local_type_repl_helper( _Replacements=[], Table ) ->
	Table;

% Replacement can be either { TargetModule, TargetType } or TargetModule:
get_local_type_repl_helper( _Replacements=[
		{ Src={ _SourceTypeMatch, _ArityMatch },
		  Replacement={ _TargetModule, _TargetType } } | T ], Table ) ->

	% Up to one transformation per source type:
	NewTable = ?table:addNewEntry( Src, Replacement, Table ),
	get_local_type_repl_helper( T, NewTable );

% Same target type here:
get_local_type_repl_helper( _Replacements=[
		{ Src={ SourceTypeMatch, _ArityMatch }, TargetModule } | T ], Table )
  when is_atom( TargetModule ) ->

	Replacement = { TargetModule, SourceTypeMatch },

	% Up to one transformation per source type:
	NewTable = ?table:addNewEntry( Src, Replacement, Table ),
	get_local_type_repl_helper( T, NewTable );


get_local_type_repl_helper(_Replacements=[
		{ Src={ _SourceTypeMatch, _ArityMatch }, ReplaceFun } | T ], Table )
  when is_function( ReplaceFun ) ->

	% Up to one transformation per source type:
	NewTable = ?table:addNewEntry( Src, ReplaceFun, Table ),
	get_local_type_repl_helper( T, NewTable ).



% Returns a table describing remote type replacements.
%
% Ex: [ { { a_module, void, 0 }, basic_utils },
%       { { a_module, my_maybe, 1 }, { basic_utils, maybe } },
%       % First clause will never match due to arity:
%       { { '_', '_', 3 }, fun( other_void, 0 ) ->
%                                    other_utils;
%                             ( _, '_' ) ->
%                                   {foo_utils,some_type}
%                     end }
% ]
%
% will return a description of the transformation of:
%
%  - a_module:void() into basic_utils:void(), as the same type name is implied
%  there; it is just the modification of the module used by a remote type
%  - a_module:my_maybe(T) into basic_utils:maybe(T)
%  - M:other_void() into M:other_utils()
%  - any type of any module depending on three other types by
%  foo_utils:some_type/3
%
-spec get_remote_type_replacement_table(
		[ { remote_type_id_match(), type_replacement() } ] ) ->
				remote_type_replacement_table().
get_remote_type_replacement_table( Replacements ) ->
	EmptyTable = ?table:new(),
	get_remote_type_repl_helper( Replacements, EmptyTable ).



% (helper)
get_remote_type_repl_helper( _Replacements=[], Table ) ->
	Table;

% Replacement can be either { TargetModule, TargetType } or TargetModule:
get_remote_type_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, _SourceTypeMatch, _ArityMatch },
			Replacement={ _TargetModule, _TargetType } } | T ], Table ) ->

	% Up to one transformation per source type:
	NewTable = ?table:addNewEntry( Src, Replacement, Table ),
	get_remote_type_repl_helper( T, NewTable );

% Same target type here:
get_remote_type_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, SourceTypeMatch, _ArityMatch }, TargetModule } | T ],
							 Table )
  when is_atom( TargetModule ) ->

	Replacement = { TargetModule, SourceTypeMatch },

	% Up to one transformation per source type:
	NewTable = ?table:addNewEntry( Src, Replacement, Table ),
	get_remote_type_repl_helper( T, NewTable );


get_remote_type_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, _SourceTypeMatch, _ArityMatch }, ReplaceFun } | T ],
							Table )
  when is_function( ReplaceFun ) ->

	% Up to one transformation per source type:
	NewTable = ?table:addNewEntry( Src, ReplaceFun, Table ),
	get_remote_type_repl_helper( T, NewTable ).



% Replaces local and remote types in specified located AST according to the two
% specified tables.
%
-spec replace_types_in( located_ast(), ast_replacements() ) -> located_ast().
replace_types_in( InputLocatedAST, Replacements ) ->

	%display_debug( "Local type replacement table: ~s",
	%	   [ ?table:toString( Replacements#ast_replacements.local_types ) ] ),

	%display_debug( "Remote type replacement table: ~s",
	%	   [ ?table:toString( Replacements#ast_replacements.remote_types ) ] ),

	OutputLocatedAST = replace_types_helper( InputLocatedAST, Replacements,
											 _Acc=[] ),

	%display_debug( "AST after type replacement:~n~s",
	%			   [ located_ast_to_string( OutputLocatedAST ) ] ),

	OutputLocatedAST.


replace_types_helper( _InputLocatedAST=[], _Replacements, Acc ) ->
	Acc;

replace_types_helper( _InputLocatedAST=[ { Loc, Form } | T ], Replacements,
					  Acc ) ->
	NewForm = replace_types_in_type_def( Form, Replacements ),
	replace_types_helper( T, Replacements, [ { Loc, NewForm } | Acc ] ).




replace_types_in_type_def( _Form={ attribute, Line, type,
								   { TypeName, TypeDef, TypeVars } },
						   Replacements ) ->

	NewTypeDef = traverse_type( TypeDef, Replacements ),

	NewTypeVars = [ traverse_type( Elem, Replacements ) || Elem <- TypeVars ],

	%display_debug( "Translation of type definition:~n~p~nis:~n~p~nwith ~p.",
	%			   [ TypeDef, NewTypeDef, NewTypeVars ] ),

	{ attribute, Line, type, { TypeName, NewTypeDef, NewTypeVars } };



replace_types_in_type_def( _Form={ attribute, Line, opaque,
								   { TypeName, TypeDef, TypeVars } },
						   Replacements ) ->

	NewTypeDef = traverse_type( TypeDef, Replacements ),

	NewTypeVars = [ traverse_type( Elem, Replacements ) || Elem <- TypeVars ],

	%display_debug( "Translation of opaque type definition:~n~p~nis:~n~p~n"
	%               "with ~p.", [ TypeDef, NewTypeDef, NewTypeVars ] ),

	{ attribute, Line, opaque, { TypeName, NewTypeDef, NewTypeVars } };



replace_types_in_type_def( _Form={ attribute, Line, record,
								   { TypeName, Fields } },
						   Replacements ) ->

	NewFields = update_types_in_fields( Fields, Replacements ),

	%display_debug( "Translation of record field definitions:~n~p~nis:~n~p~n.",
	%               [ Fields, NewFields ] ),

	{ attribute, Line, record, { TypeName, NewFields } };


replace_types_in_type_def( UnexpectedForm, _Replacements ) ->
	raise_error( { unexpected_typedef_form, UnexpectedForm } ).



% Updates the types in known functions from specified function table, based on
% specified replacements.
%
-spec update_types_in_functions( function_table(), ast_replacements() ) ->
									   function_table().
update_types_in_functions( FunctionTable, Replacements ) ->

	FunIdInfoPairs = ?table:enumerate( FunctionTable ),

	NewFunIdInfoPairs = [ { FunId, update_fun_info_for_types( FunInfo,
															  Replacements ) }
						  || { FunId, FunInfo } <- FunIdInfoPairs ],

	?table:new( NewFunIdInfoPairs ).



% Updates the types in the -spec fields, based on specified replacements.
%
update_fun_info_for_types( FunInfo=#function_info{ spec=undefined },
						   _Replacements ) ->
	FunInfo;

update_fun_info_for_types( FunInfo=#function_info{ spec={ Loc, FunSpec } },
						   Replacements ) ->

	NewFunSpec = case FunSpec of

		% Ex for '-spec f( type_a() ) -> type_b().':
		% SpecList = [ {type,652,'fun',
		%     [{type,652,product,[{user_type,652,type_a,[]}]},
		%       {user_type,652,type_b,[]}]
		%   } ]
		{ attribute, Line, spec, { FunId, SpecList } } ->
			%display_trace( "SpecList = ~p", [ SpecList ] ),
			NewSpecList = [ update_spec( Spec, Replacements )
							|| Spec <- SpecList ],
			{ attribute, Line, spec, { FunId, NewSpecList } };

		_ ->
			raise_error( { unexpected_fun_spec, FunSpec } )

	end,

	FunInfo#function_info{ spec={ Loc, NewFunSpec } };


update_fun_info_for_types( _FunInfo=#function_info{ spec=UnexpectedLocSpec },
				  _Replacements ) ->
	raise_error( { unexpected_located_fun_spec, UnexpectedLocSpec } ).



% Updates the specified function specification.
%
update_spec( { type, Line, 'fun', ClausesSpecs },
			 Replacements ) ->

	NewClausesSpecs = update_clause_spec( ClausesSpecs, Replacements ),
	{ type, Line, 'fun', NewClausesSpecs };

update_spec( UnexpectedFunSpec, _Replacements ) ->
	raise_error( { unexpected_fun_spec, UnexpectedFunSpec } ).



update_clause_spec( [ { type, Line, product, ParamTypes }, ResultType ],
					Replacements ) ->

	NewParamTypes = [ traverse_type( ParamType, Replacements )
					  || ParamType <- ParamTypes ],

	NewResultType = traverse_type( ResultType, Replacements ),

	[ { type, Line, product, NewParamTypes }, NewResultType ];

update_clause_spec( UnexpectedClauseSpec, _Replacements ) ->
	raise_error( { unexpected_clause_spec, UnexpectedClauseSpec } ).




% Updates the types in specified record fields, based on specified replacements.
%
-spec update_types_in_fields( [ ast_field_description() ],
					  ast_replacements() ) -> [ ast_field_description() ].
update_types_in_fields( Fields, Replacements ) ->

	%display_debug( "Input fields: ~p.", [ Fields ] ),

	NewFields = [ update_types_in_field( F, Replacements ) || F <- Fields ],

	%display_debug( "New fields: ~p.", [ NewFields ] ),

	NewFields.



-spec update_types_in_field( ast_field_description(), ast_replacements() ) ->
								   ast_field_description().
% Type specified, without or with a default value:
update_types_in_field( _F={ typed_record_field,
		   % { record_field, _Line1, { atom, _Line2, _FieldName } },
		   %  - or -
		   % { record_field, _Line1, { atom, _Line2, _FieldName },
		   %		{ _ImmediateType, Line2, DefaultValue } }:
		   RecordField,
		   %{ type, Line3, TypeName, TypeVars } }:
		   TypeDef },
		   Replacements ) ->

	NewTypeDef = traverse_type( TypeDef, Replacements ),

	{ typed_record_field, RecordField, NewTypeDef };


% No type and no default value specified:
update_types_in_field( F={ record_field, _Line1,
						   % { atom, Line2, FieldName }:
						   _FieldNameDef },
					   _Replacements) ->
	F;

% No type specified, yet with a default value:
update_types_in_field( F={ record_field, _Line1,
						   % { atom, Line2, FieldName }:
						   _FieldNameDef,
						   % { _ImmediateType, Line2, DefaultValue }:
						   _DefaultValueDef },
					   _Replacements ) ->
	F;


update_types_in_field( F, _Replacements ) ->
	raise_error( { unexpected_record_field, F } ).




% Traversing types.
%
% (records like #type, #user_type, could be used instead)
%
% (helper)
%
% Tuple type found:
%
traverse_type( _TypeDef={ type, Line, tuple, ElementTypes }, Replacements )
  when is_list( ElementTypes ) ->
	{ type, Line, tuple,
	  [ traverse_type( Elem, Replacements ) || Elem <- ElementTypes ] };

traverse_type( TypeDef={ type, _Line, tuple, any }, _Replacements ) ->
	TypeDef;

% List type found, ex:
% {attribute,43,type,{foo6,{type,43,list,[{type,43,boolean,[]}]},[]}},
traverse_type( _TypeDef={ type, Line, list, [ ElementType ] },
			   Replacements ) ->
	{ type, Line, list, [ traverse_type( ElementType, Replacements ) ] };

% Other built-in type:
traverse_type( _TypeDef={ type, Line, BuiltinType, TypeVars }, Replacements )
  when is_list( TypeVars ) ->
	NewTypeVars = [ traverse_type( Elem, Replacements ) || Elem <- TypeVars ],
	{ type, Line, BuiltinType, NewTypeVars };

% Local user type found:
traverse_type( _TypeDef={ user_type, Line, TypeName, TypeVars },
			   Replacements ) ->

	TypeArity = length( TypeVars ),

	NewTypeVars = [ traverse_type( Elem, Replacements ) || Elem <- TypeVars ],

	Outcome = case Replacements#ast_replacements.local_types of

		undefined ->
			unchanged;

		LocalReplaceTable ->

			% Returning the new type information:
			case ?table:lookupEntry( { TypeName, TypeArity },
									 LocalReplaceTable ) of

				% Module and type overridden:
				{ value, E={ _NewModuleName, _NewTypeName } } ->
					E;

				% Same type, only module overridden:
				% (never happens, as module always specified in table)
				%{ value, NewModuleName } when is_atom( NewModuleName ) ->
				%	{ NewModuleName, TypeName };

				{ value, TransformFun } when is_function( TransformFun ) ->
					TransformFun( TypeName, TypeArity );

				key_not_found ->

					% Maybe a wildcard arity was defined then?
					case ?table:lookupEntry( { TypeName, _AnyArity='_' },
											 LocalReplaceTable ) of

						{ value, E={ _NewModuleName, _NewTypeName } } ->
							E;

						% Same type, only module overridden:
						% (never happens)
						%{ value, NewModuleName }
						%    when is_atom( NewModuleName ) ->
						%	{ NewModuleName, TypeName };

						{ value, TransformFun }
						  when is_function( TransformFun ) ->
							TransformFun( TypeName, TypeArity );

						key_not_found ->
							% Nope, let it as it is:
							unchanged

					end

			end

	end,

	case Outcome of

		unchanged ->
			% TypeDef with updated TypeVars:
			{ user_type, Line, TypeName, NewTypeVars };

		{ SetModuleName, SetTypeName } ->
			ast_utils:forge_remote_type( SetModuleName, SetTypeName,
										 NewTypeVars, Line )

	end;


% Remote user type found:
traverse_type( _TypeDef={ remote_type, Line1,
						 [ M={ atom, Line2, ModuleName },
						   T={ atom, Line3, TypeName }, TypeVars ] },
			   Replacements ) ->

	TypeArity = length( TypeVars ),

	NewTypeVars = [ traverse_type( Elem, Replacements ) || Elem <- TypeVars ],

	% Returning the new type information:
	Outcome = case Replacements#ast_replacements.remote_types of

		undefined ->
			unchanged;

		RemoteReplaceTable ->
			case ?table:lookupEntry( { ModuleName, TypeName, TypeArity },
									   RemoteReplaceTable ) of

				 % Module and type overridden:
				{ value, E={ _NewModuleName, _NewTypeName } } ->
					E;

				 % Same type, only module overridden:
				{ value, NewModuleName } when is_atom( NewModuleName ) ->
					{ NewModuleName, TypeName };

				{ value, TransformFun } when is_function( TransformFun ) ->
					TransformFun( ModuleName, TypeName, TypeArity );

				key_not_found ->

					AnyArity = '_',

					% Maybe a wildcard arity was defined for that type then?
					case ?table:lookupEntry( { ModuleName, TypeName, AnyArity },
											 RemoteReplaceTable ) of

						{ value, E={ _NewModuleName, _NewTypeName } } ->
							E;

						 % Same type, only module overridden (never happens by
						 % design):
						 %{ value, NewModuleName }
						 %        when is_atom( NewModuleName ) ->
						 %    { NewModuleName, TypeName };

						{ value, TransformFun }
						  when is_function( TransformFun ) ->
							TransformFun( ModuleName, TypeName, TypeArity );

						key_not_found ->

							% Nope, then maybe a wildcard type (and arity) then?
							case ?table:lookupEntry( { ModuleName, _AnyType='_',
											 AnyArity }, RemoteReplaceTable ) of

								{ value, E={ _NewModuleName, _NewTypeName } } ->
									E;

								% Same type, only module overridden:
								{ value, NewModuleName }
								  when is_atom( NewModuleName ) ->
									{ NewModuleName, TypeName };

								{ value, TransformFun }
								  when is_function( TransformFun ) ->
									TransformFun( ModuleName, TypeName,
												  TypeArity );

								key_not_found ->
									% Nope, let it as it is:
									unchanged

							end

					end

			end

	end,

	case Outcome of

		unchanged ->
			% TypeDef with updated TypeVars:
			{ remote_type, Line1, [ M, T, NewTypeVars ] };

		{ SetModuleName, SetTypeName } ->
			ast_utils:forge_remote_type( SetModuleName, SetTypeName,
										 NewTypeVars, Line1, Line2, Line3 )

	end;


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
traverse_type( _TypeDef={ ann_type, Line, [ Var, InternalTypeDef ] },
			   Replacements ) ->

	NewInternalTypeDef = traverse_type( InternalTypeDef, Replacements ),

	{ ann_type, Line, [ Var, NewInternalTypeDef ] };


% Variable declaration, possibly obtained through declarations like:
% -type my_type( T ) :: other_type( T ).
% or:
% -opaque tree( T ) :: { T, [ tree(T) ] }.
traverse_type( TypeDef={ var, _Line, _TypeName }, _Replacements ) ->
	TypeDef;


% Immediate values like {atom,42,undefined}, possibly obtained through
% declarations like: -type my_type() :: integer() | 'undefined'.
%
traverse_type( TypeDef={ TypeName, _Line, _Value }, _Replacements ) ->
	case lists:member( TypeName, type_utils:get_immediate_types() ) of

		true ->
			TypeDef;

		false ->
			raise_error( { unexpected_immediate_value, TypeDef } )

	end;


traverse_type( TypeDef, _Replacements ) ->
	raise_error( { unhandled_typedef, TypeDef } ).







%% Call replacement section.


% Returns a table describing local call replacements.
%
% Ex: [ { { halt, 0 }, basic_utils },
%       { { setAttributes, 1 }, { some_utils, set_attr } },
%       % First clause will never match due to arity:
%       { { '_', 3 }, fun( my_fun, 0 ) ->
%                                   other_utils;
%                        ( _, '_' ) ->
%                                   {foo_utils,some_fun}
%                     end }
% ]
%
% will return a description of the transformation of:
%
%  - halt/0 into basic_utils:halt/0, as the same function name is implied there;
%  it is just the addition (prefix) of a module, as a remote call
%
%  - setAttributes/1 into some_utils:set_attr/1
%
%  - my_fun/0 into other_utils:my_fun/0
%
%  - any call to a function of arity 3 by foo_utils:some_fun/3
%
-spec get_local_call_replacement_table(
		[ { local_call_match(), call_replacement() } ] ) ->
				local_call_replacement_table().
get_local_call_replacement_table( Replacements ) ->
	EmptyTable = ?table:new(),
	get_local_call_repl_helper( Replacements, EmptyTable ).



% (helper)
get_local_call_repl_helper( _Replacements=[], Table ) ->
	Table;

% Replacement can be either { TargetModule, TargetFunctionName } or
% TargetModule:
%
get_local_call_repl_helper( _Replacements=[
		{ Src={ _SourceFunctionNameMatch, _ArityMatch },
		  Replacement={ _TargetModule, _TargetFunctionName } } | T ], Table ) ->

	% Up to one transformation per source function:
	NewTable = ?table:addNewEntry( Src, Replacement, Table ),
	get_local_call_repl_helper( T, NewTable );

% Same target function name here:
get_local_call_repl_helper( _Replacements=[
		{ Src={ SourceFunctionNameMatch, _ArityMatch }, TargetModule } | T ],
							Table ) when is_atom( TargetModule ) ->

	Replacement = { TargetModule, SourceFunctionNameMatch },

	% Up to one transformation per source function:
	NewTable = ?table:addNewEntry( Src, Replacement, Table ),
	get_local_call_repl_helper( T, NewTable );


get_local_call_repl_helper(_Replacements=[
		{ Src={ _SourceFunctionNameMatch, _ArityMatch }, ReplaceFun } | T ],
						   Table ) when is_function( ReplaceFun ) ->

	% Up to one transformation per source function:
	NewTable = ?table:addNewEntry( Src, ReplaceFun, Table ),
	get_local_call_repl_helper( T, NewTable ).



% Returns a table describing remote call replacements.
%
% Ex: [ { { a_module, void, 0 }, basic_utils },
%       { { a_module, my_maybe, 1 }, { basic_utils, maybe } },
%       % First clause will never match due to arity:
%       { { '_', '_', 3 }, fun( other_void, 0 ) ->
%                                    other_utils;
%                             ( _, '_' ) ->
%                                   {foo_utils,some_type}
%                     end }
% ]
%
% will return a description of the transformation of:
%
%  - a_module:void() into basic_utils:void(), as the same type name is implied
%  there; it is just the modification of the module used by a remote type
%  - a_module:my_maybe(T) into basic_utils:maybe(T)
%  - M:other_void() into M:other_utils()
%  - any type of any module depending on three other types by
%  foo_utils:some_type/3
%
-spec get_remote_call_replacement_table(
		[ { remote_call_match(), call_replacement() } ] ) ->
				remote_call_replacement_table().
get_remote_call_replacement_table( Replacements ) ->
	EmptyTable = ?table:new(),
	get_remote_call_repl_helper( Replacements, EmptyTable ).


% (helper)
get_remote_call_repl_helper( _Replacements=[], Table ) ->
	Table;

% Replacement can be either { TargetModule, TargetFunctionName } or
% TargetModule:
%
get_remote_call_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, _SourceFunctionNameMatch, _ArityMatch },
			Replacement={ _TargetModule, _TargetFunctionName } } | T ],
							 Table ) ->

	% Up to one transformation per source function:
	NewTable = ?table:addNewEntry( Src, Replacement, Table ),
	get_remote_call_repl_helper( T, NewTable );

% Same target function name here:
get_remote_call_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, SourceFunctionNameMatch, _ArityMatch },
	  TargetModule } | T ], Table ) when is_atom( TargetModule ) ->

	Replacement = { TargetModule, SourceFunctionNameMatch },

	% Up to one transformation per source function:
	NewTable = ?table:addNewEntry( Src, Replacement, Table ),
	get_remote_call_repl_helper( T, NewTable );


get_remote_call_repl_helper( _Replacements=[
	{ Src={ _ModuleMatch, _SourceFunctionNameMatch, _ArityMatch },
	  ReplaceFun } | T ], Table ) when is_function( ReplaceFun ) ->

	% Up to one transformation per source function:
	NewTable = ?table:addNewEntry( Src, ReplaceFun, Table ),
	get_remote_call_repl_helper( T, NewTable ).



% Updates the calls in known functions from specified function table, based on
% specified replacements.
%
-spec update_calls_in_functions( function_table(), ast_replacements() ) ->
									   function_table().
update_calls_in_functions( FunctionTable, Replacements ) ->

	FunIdInfoPairs = ?table:enumerate( FunctionTable ),

	NewFunIdInfoPairs = [ { FunId,
							update_fun_info_for_calls( FunInfo, Replacements ) }
						  || { FunId, FunInfo } <- FunIdInfoPairs ],

	?table:new( NewFunIdInfoPairs ).



% Updates the calls in the function definitions, based on specified
% replacements.
%
update_fun_info_for_calls( FunInfo=#function_info{ definition=ClauseDefs },
						   Replacements ) ->

	% Top-level function clauses are apparently the same as 'case', 'receive',
	% etc. clauses:
	%
	NewClauseDefs = [ traverse_expression( ClauseDef, Replacements )
					  || ClauseDef <- ClauseDefs ],

	FunInfo#function_info{ definition=NewClauseDefs }.






% Traverses specified expression, operating relevant replacements (e.g. call
% ones).
%
% Note: we used to traverse the AST recursively, "blindly", i.e. without
% expecting the intended structure of AST elements (ex: the structure of a tuple
% corresponding to a 'receive' statement).
%
% Now, for a more complete control, we match the AST against its intended
% structure, as defined in http://erlang.org/doc/apps/erts/absform.html.
%
% Case expression found:
traverse_expression( E={ 'case', Line, TestExpression, Clauses },
					 Replacements ) ->

	display_debug( "Intercepting case expression ~p...", [ E ] ),

	NewTestExpression = traverse_expression( TestExpression, Replacements ),

	NewClauses = [ traverse_expression( C, Replacements ) || C <- Clauses ],

	Res = { 'case', Line, NewTestExpression, NewClauses },

	display_debug( "... returning case expression ~p", [ Res ] ),
	Res;



% Remote call found, with an immediate name for both the module and the
% function:
%
traverse_expression( E={ call, Line1, { remote, _Line2,
			_M={ atom, _Line3, ModuleName }, _F={ atom, Line4, FunctionName } },
			Params }, Replacements ) ->

	display_debug( "Intercepting remote call ~p...", [ E ] ),

	Arity = length( Params ),

	% First recurses:
	NewParams = [ traverse_expression( Param, Replacements )
				  || Param <- Params ],

	Outcome = case Replacements#ast_replacements.remote_calls of

		undefined ->
			unchanged;

		RemoteReplaceTable ->

			case ?table:lookupEntry( { ModuleName, FunctionName, Arity },
									 RemoteReplaceTable ) of

				{ value, E={ _NewModuleName, _NewFunctionName } } ->
					E;

				{ value, TransformFun } when is_function( TransformFun ) ->
					TransformFun( FunctionName, Arity );

				key_not_found ->

					% Maybe a wildcard arity was defined then?
					case ?table:lookupEntry(
							{ ModuleName, FunctionName, _AnyArity='_' },
							RemoteReplaceTable ) of

						{ value, E={ _NewModuleName, _NewFunctionName } } ->
							E;

						% Same function name, only module overridden:
						% (never happens)
						%{ value, NewModuleName }
						%       when is_atom( NewModuleName ) ->
						%	{ NewModuleName, FunName };

						{ value, TransformFun }
						  when is_function( TransformFun ) ->
							TransformFun( FunctionName, Arity );

						key_not_found ->
							% Maybe a wildcard function name was defined then?

							% (note: the case of a wildcard function name and a
							% set, actual arity is not deemed relevant)

							case ?table:lookupEntry( { ModuleName,
									   _AnyFunctionName='_', _AnyArity='_' },
													 RemoteReplaceTable ) of

								{ value,
								  { NewModuleName, _NewFunctionName='_' } } ->
									{ NewModuleName, FunctionName } ;

								{ value,
								  E={ _NewModuleName, _NewFunctionName } } ->
									E;

									% Same function name, only module
									% overridden: (never happens)
									%
									%{ value, NewModuleName }
									%       when is_atom( NewModuleName ) ->
									%    { NewModuleName, FunName };

								{ value, TransformFun }
								  when is_function( TransformFun ) ->
									TransformFun( FunctionName, Arity );

								key_not_found ->
									unchanged

							end

					end

			end

	end,

	case Outcome of

		unchanged ->
			% Original expression, yet with updated parameters:
			%Res = { call, Line1, { remote, Line2, M, F }, NewParams },
			% Used for uniformity:
			Res = ast_utils:forge_remote_call( ModuleName, FunctionName,
											   NewParams, Line1, Line4 ),
			display_debug( "... returning remote call (case R1) ~p", [ Res ] ),
			Res;

		{ SetModuleName, SetFunctionName } ->
			Res = ast_utils:forge_remote_call( SetModuleName, SetFunctionName,
											   NewParams, Line1, Line4 ),
			display_debug( "... returning remote call (case R2) ~p", [ Res ] ),
			Res

	end;



% Here, at least one name (module and/or function) is not immediate:
%
% (note: we do not manage yet the case where for example the function name
% results from an expression yet a wildcard has been defined for it)
%
traverse_expression( _E={ call, Line1,
						  { remote, Line2, ModuleExpr, FunctionExpr }, Params },
					 Replacements ) ->

	NewModuleExpr = traverse_expression( ModuleExpr, Replacements ),

	NewFunctionExpr = traverse_expression( FunctionExpr, Replacements ),

	NewParams = [ traverse_expression( Param, Replacements )
				  || Param <- Params ],

	% Cannot use ast_utils:forge_remote_call, we have not atoms:
	%
	Res = { call, Line1, { remote, Line2, NewModuleExpr, NewFunctionExpr },
			NewParams },

	display_debug( "... returning remote call (case R3) ~p", [ Res ] ),

	Res;



% Local call found:
traverse_expression( E={ call, Line1, F={ atom, Line2, FunName }, Params },
					 Replacements ) ->

	display_debug( "Intercepting local call ~p...", [ E ] ),

	Arity = length( Params ),

	% First recurses:
	NewParams = [ traverse_expression( Param, Replacements )
				  || Param <- Params ],

	Outcome = case Replacements#ast_replacements.local_calls of

		undefined ->
			unchanged;

		LocalReplaceTable ->

			case ?table:lookupEntry( { FunName, Arity }, LocalReplaceTable ) of

				{ value, E={ _NewModuleName, _NewFunName } } ->
					E;

				{ value, TransformFun } when is_function( TransformFun ) ->
					TransformFun( FunName, Arity );

				key_not_found ->

					% Maybe a wildcard arity was defined then?
					case ?table:lookupEntry( { FunName, _AnyArity='_' },
											 LocalReplaceTable ) of

						{ value, E={ _NewModuleName, _NewFunName } } ->
							E;

						% Same function name, only module overridden: (never
						% happens)
						%{ value, NewModuleName }
						%       when is_atom( NewModuleName ) ->
						%	{ NewModuleName, FunName };

						{ value, TransformFun }
						  when is_function( TransformFun ) ->
							TransformFun( FunName, Arity );

						key_not_found ->
							% Nope, let it as it is:
							unchanged

					end

			end

	end,

	case Outcome of

		unchanged ->
			% Original expression yet with updated parameters:
			Res={ call, Line1, F, NewParams },
			display_debug( "... returning local call ~p", [ Res ] ),
			Res;

		{ SetModuleName, SetFunctionName } ->
			Res = ast_utils:forge_remote_call( SetModuleName, SetFunctionName,
											   NewParams, Line1, Line2 ),
			display_debug( "... returning remote call ~p", [ Res ] ),
			Res

	end;



% Match expression found:
traverse_expression( E={ match, Line, LeftExpr, RightExpr }, Replacements ) ->

	display_debug( "Intercepting match expression ~p...", [ E ] ),

	NewLeftExpr = traverse_expression( LeftExpr, Replacements ),

	NewRightExpr = traverse_expression( RightExpr, Replacements ),

	Res = { match, Line, NewLeftExpr, NewRightExpr },

	display_debug( "... returning match expression ~p", [ Res ] ),

	Res;


% Receive expression found:
traverse_expression( E={ 'receive', Line, Clauses }, Replacements ) ->

	display_debug( "Intercepting receive expression ~p...", [ E ] ),

	NewClauses = [ traverse_expression( C, Replacements ) || C <- Clauses ],

	Res = { 'receive', Line, NewClauses },

	display_debug( "... returning receive expression ~p", [ Res ] ),

	Res;


% Clause (belonging to an expression such as top-level function clause, or
% 'case', 'receive' clauses, etc.) found:
%
traverse_expression( Clause={ clause, Line, ValueExpr, Guards, ResultExpr },
					  Replacements ) ->

	display_debug( "Intercepting clause ~p...", [ Clause ] ),

	% Rather complete, out of safety:

	NewValueExpr = [ traverse_expression( E, Replacements ) || E <- ValueExpr ],

	% Guard example: {call,102, {atom,102,is_integer}, [{var,102,'X'}]}
	NewGuards = traverse_expression( Guards, Replacements ),

	NewResultExpr = traverse_expression( ResultExpr, Replacements ),

	Res = { clause, Line, NewValueExpr, NewGuards, NewResultExpr },

	display_debug( "... returning clause ~p", [ Res ] ),

	Res;


% List of expressions found:
%
% (note: this clause may be removed in the future, once all AST elements will
% have been specifically intercepted by a dedicated clause, and when the nature
% of their elements will be established and thus traversed specifically, rather
% than opening the possibility that each element may be a list)
%
traverse_expression( ExprList, Replacements ) when is_list( ExprList ) ->

	display_debug( "Intercepting expression list ~p...", [ ExprList ] ),

	NewExprList = [ traverse_expression( E, Replacements ) || E <- ExprList ],

	display_debug( "... returning expression list ~p", [ NewExprList ] ),

	NewExprList;



% Other expression found:
traverse_expression( E, _Replacements ) ->
	display_debug( "Letting expression ~p as is.", [ E ] ),
	E.












% Traverses specified term (possibly with nested subterms - the function will
% recurse in lists and tuples), calling specified transformer function on each
% instance of specified type, in order to replace that instance by the result of
% that function.
%
% Returns an updated term, with these replacements made.
%
% Ex: the input term could be T={ a, [ "foo", { c, [ 2.0, 45 ] } ] } and the
% function might replace, for example, floats by <<bar>>; then T'={ a, [ "foo",
% { c, [ <<bar>>, 45 ] } ] } would be returned.
%
% Note: the transformed terms are themselves recursively transformed, to ensure
% nesting is managed. Of course this implies that the term transform should not
% result in iterating the transformation infinitely.
%
% As a result it may appear that a term of the targeted type is transformed
% almost systematically twice: it is first transformed as such, and the result
% is transformed in turn. If the transformed term is the same as the original
% one, then that content will be shown as analysed twice.
%
-spec traverse_term( term(), type_utils:primitive_type_description(),
					 term_transformer(), basic_utils:user_data() ) ->
						   { term(), basic_utils:user_data() }.

% Here the term is a list and this is the type we want to intercept:
traverse_term( TargetTerm, _TypeDescription=list, TermTransformer, UserData )
  when is_list( TargetTerm ) ->

	{ TransformedTerm, NewUserData } = TermTransformer( TargetTerm, UserData ),

	traverse_transformed_term( TransformedTerm, _TypeDescription=list,
							   TermTransformer, NewUserData );


% Here the term is a list and we are not interested in them:
traverse_term( TargetTerm, TypeDescription, TermTransformer, UserData )
  when is_list( TargetTerm ) ->

	traverse_list( TargetTerm, TypeDescription, TermTransformer, UserData );


% Here the term is a tuple (or a record...), and we want to intercept them:
traverse_term( TargetTerm, TypeDescription, TermTransformer, UserData )
  when is_tuple( TargetTerm )
	andalso ( TypeDescription =:= tuple orelse TypeDescription =:= record ) ->

	{ TransformedTerm, NewUserData } = TermTransformer( TargetTerm, UserData ),

	traverse_transformed_term( TransformedTerm, TypeDescription,
							   TermTransformer, NewUserData );


% Here the term is a tuple (or a record...), and we are not interested in them:
traverse_term( TargetTerm, TypeDescription, TermTransformer, UserData )
  when is_tuple( TargetTerm ) ->

	traverse_tuple( TargetTerm, TypeDescription, TermTransformer, UserData );


% Base case (current term is not a binding structure, it is a leaf of the
% underlying syntax tree):
%
traverse_term( TargetTerm, TypeDescription, TermTransformer, UserData ) ->

	case type_utils:get_type_of( TargetTerm ) of

		TypeDescription ->
			TermTransformer( TargetTerm, UserData );

		_ ->
			% Unchanged:
			{ TargetTerm, UserData }

	end.



% Helper to traverse a list.
%
traverse_list( TargetList, TypeDescription, TermTransformer, UserData ) ->

	{ NewList, NewUserData } = lists:foldl(
								 fun( Elem, { AccList, AccData } ) ->

			{ TransformedElem, UpdatedData } = traverse_term( Elem,
							TypeDescription, TermTransformer, AccData ),

			% New accumulator, produces a reversed element list:
			{ [ TransformedElem | AccList ], UpdatedData }

								 end,

								 _Acc0={ _Elems=[], UserData },

								 TargetList ),

	{ lists:reverse( NewList ), NewUserData }.



% Helper to traverse a tuple.
%
traverse_tuple( TargetTuple, TypeDescription, TermTransformer, UserData ) ->

	% We do exactly as with lists:
	TermAsList = tuple_to_list( TargetTuple ),

	{ NewList, NewUserData } = traverse_list( TermAsList, TypeDescription,
											  TermTransformer, UserData ),

	{ list_to_tuple( NewList ), NewUserData }.



% Helper to traverse a transformed term (ex: if looking for a { user_id, String
% } pair, we must recurse in nested tuples like: { 3, { user_id, "Hello" }, 1 }.
%
traverse_transformed_term( TargetTerm, TypeDescription, TermTransformer,
						   UserData ) ->

	case TermTransformer( TargetTerm, UserData ) of

		{ TransformedTerm, NewUserData } when is_list( TransformedTerm ) ->
			traverse_list( TransformedTerm, TypeDescription, TermTransformer,
						   NewUserData );

		{ TransformedTerm, NewUserData } when is_tuple( TransformedTerm ) ->
			traverse_tuple( TransformedTerm, TypeDescription, TermTransformer,
						   NewUserData );

		% { ImmediateTerm, NewUserData } ->
		Other ->
			Other

	end.




% Section to manage ASTs and forms.


% Converts the specified Erlang term (ex: the float '42.0') into a corresponding
% form (ex: '{ float, _Line=0, 42.0 }').
%
-spec term_to_form( term() ) -> form().
term_to_form( Term ) ->

	case erl_syntax:abstract( Term ) of

		% Either the doc or the type information for erl_syntax:abstract/1 is
		% incorrect:

		%badarg ->
		%	throw( { term_abstraction_failed, Term } );

		SyntaxTree ->

			% Could be used with erl_syntax:is_tree/1:
			% case erl_syntax:revert( SyntaxTree ) of...
			erl_syntax:revert( SyntaxTree )

	end.



% Converts a list of names of variables into the corresponding AST.
%
% Ex: wanting to specify '[ V1, Alpha, A ]', we have: variable_names_to_ast( [
% "V1", "Alpha", "A" ], _Line=0 ) = [ {cons,0, {var,0,'V1'},
% {cons,0,{var,0,'Alpha'}, {cons,0,{var,0,'A'}, {nil,0} } } } ]
%
-spec variable_names_to_ast( [ string() ], ast_utils:line() ) -> ast().
variable_names_to_ast( VariableNames, Line ) ->

	% Could be done directly recursively by incrementally 'consing' reversed
	% list.

	NameListString = "[ " ++ text_utils:join( ", ",  VariableNames ) ++ " ].",

	string_to_expressions( NameListString, Line ).



% Converts the specified source code of a form (as a string) into its
% corresponding abstract form (assuming being in line #1).
%
% Ex: string_to_form( "f() -> hello_world." ) returns
%   { function, 1, f, 0, [ { clause, 1, [], [], [ {atom,1,hello_world} ] } ] }
%
-spec string_to_form( string() ) -> form().
string_to_form( FormString ) ->
	string_to_form( FormString, _Loc=1 ).



% Converts the specified source code of a form (i.e., a string) into its
% corresponding abstract form.
%
% Ex: string_to_form( "f() -> hello_world.", 42 ) returns
%   { function, 1, f, 0, [ { clause, 42, [], [], [ {atom,1,hello_world} ] } ] }
%
-spec string_to_form( string(), ast_utils:file_loc() ) -> form().
string_to_form( FormString, Location ) ->

	% First get Erlang tokens from that string:
	Tokens = case erl_scan:string( FormString, Location ) of

		% Ex: [{atom,1,f},{'(',1},{')',1},{'->',1},{atom,1,hello_world},{dot,1}]
		{ ok, Toks, _EndLocation } ->
			%display_debug( "Tokens: ~p", [ Toks ] ),
			Toks;

		ErrorTok ->
			throw( { form_tokenizing_error, FormString, ErrorTok } )

	end,

	% Tokens to erl_parse trees:

	case erl_parse:parse_form( Tokens ) of

		{ ok, ParseTree } ->
			ParseTree;

		ErrorPar ->
			throw( { form_parsing_error, FormString, ErrorPar } )

	end.



% Converts the specified source code of a list of expressions (i.e., a string)
% into its corresponding AST (assuming being in line #1).
%
% Ex: string_to_expressions( "[ { a, 1 }, foobar ]" ) returns
%   [ { cons, 1, { tuple, 1, [ {atom,1,a}, {integer,1,1} ] },
%     { cons, 1, {atom,1,foobar}, {nil,1} } } ]
%
-spec string_to_expressions( string() ) -> ast().
string_to_expressions( ExpressionString ) ->
	string_to_expressions( ExpressionString, _Loc=1 ).



% Converts the specified source code of a term (i.e., a string) and a location
% into the corresponding abstract form.
%
% Ex: string_to_expressions( "[ { a, 1 }, foobar ]", _Loc=42 ) returns
%   [ { cons, 42, { tuple, 42, [ {atom,42,a}, {integer,42,1} ] },
%     { cons, 42, {atom,42,foobar}, {nil,42} } } ]
%
-spec string_to_expressions( string(), ast_utils:file_loc() ) -> ast().
string_to_expressions( ExpressionString, Location ) ->

	% First get Erlang tokens from that string:
	Tokens = case erl_scan:string( ExpressionString, Location ) of

		% Ex: [ {'[',42}, {'{',42}, {atom,42,a}, {',',42}, {integer,42,1},
		% {'}',42}, {',',42}, {atom,42,foobar}, {']',42} ]
		{ ok, Toks, _EndLocation } ->
			%display_debug( "Tokens: ~p", [ Toks ] ),
			Toks;

		ErrorTok ->
			throw( { expression_tokenizing_error, ExpressionString, ErrorTok } )

	end,

	% Tokens to erl_parse trees:

	case erl_parse:parse_exprs( Tokens ) of

		{ ok, ParseTree } ->
			ParseTree;

		ErrorPar ->
			throw( { expression_parsing_error, ExpressionString, ErrorPar } )

	end.



% Converts the specified source code of a term (i.e., a string) into its
% corresponding value.
%
% Ex: string_to_value( "[ {tiger,[lion,leopard]} ]" ) returns the
% [{tiger,[lion,leopard]}] term.
%
-spec string_to_value( string() ) -> term().
string_to_value( ExpressionString ) ->

	% We automatically add the necessary final dot:
	[ Expr ] = string_to_expressions( ExpressionString ++ "." ),

	{ value, Result, _NewBindings } = erl_eval:expr( Expr, _Bindings=[] ),

	Result.



% Reads the specified BEAM file (expected to be compiled with debug information)
% and returns the corresponding AST.
%
% Note that the filename must be a relative or absolute path pointing directly
% to the BEAM file (it is not searched through the code path).
%
-spec beam_to_ast( file:filename() ) -> ast().
beam_to_ast( BeamFilename ) ->

	% We do not use functions from other Common modules here (ex: file_utils) as
	% they are not expected to be built yet (they will be built with the common
	% parse transform afterwards).
	%
	case file:read_link_info( BeamFilename ) of

		{ ok, FileInfo } ->
			#file_info{ type=regular } = FileInfo,
			ok;

		{ error, eloop } ->
			% Probably a recursive symlink:
			throw( { too_many_symlink_levels, BeamFilename } );

		{ error, enoent } ->
			throw( { non_existing_beam_file, BeamFilename } )

	end,

	% We could basically list all chunks, but we are only interested here in the
	% abstract code:
	%

	% Everything:
	%Chunks = [ abstract_code, attributes, compile_info, exports,
	%			labeled_exports, imports, indexed_imports, locals,
	%			labeled_locals, atoms ],

	% Just the code AST:
	Chunks = [ abstract_code ],

	% Everything but the code AST:
	% OtherChunks = [ attributes, compile_info, exports,
	%				  labeled_exports, imports, indexed_imports, locals,
	%				  labeled_locals, atoms ],

	%Options = [ allow_missing_chunks ],
	Options=[],

	case beam_lib:chunks( BeamFilename, Chunks, Options ) of

		{ ok, { _Module, [ { abstract_code, { _RawAbstractV1,
											  AbstractCode } } ] } } ->
			%display_debug( "Module = ~p.", [ Module ] ),
			AbstractCode;

		{ error, beam_lib, Reason } ->
			throw( { beam_reading_failed, Reason } )

	end.



% Processes the specified AST relative to a whole module, and returns the
% corresponding information gathered.
%
% Note: the extraction will probably fail (and stop any underlying parse
% transform) should the corresponding, specified code not be able to compile (as
% a rather precise linting is done).
%
-spec extract_module_info_from_ast( ast() ) -> module_info().
extract_module_info_from_ast( AST ) ->

	%display_debug( "Processing following AST:~n~p", [ AST ] ),
	%display_debug( "Processing AST:" ),

	%write_ast_to_file( AST, "original-extracted-ast.txt" ),

	% First we check whether the corresponding code compiles:

	% We could define specific compile options, yet they could be too
	% restrictive (more than the ones of the compiler) and moreover it would
	% force us to specify a filename.

	% We cannot simply count errors and warnings, as we have in each case a list
	% of per-file elements (a list of lists), in the order of the specified AST
	% (thus additionally a given file may happen multiple times); a count is not
	% useful here anyway.

	% Finally we have not real freedom in terms of output, as we prefer to
	% respect the native display format of the error messages so that tools (ex:
	% emacs, possible erlide and all) are still able to manage them.

	% Useless: would report pre-transform errors that would be solved after
	% transformation (ex: void() not existing)
	%pre_check_ast( AST ),

	InitModuleInfo = init_module_info(),

	ModuleInfo = process_ast( AST, InitModuleInfo ),

	% Uncomment with care, as must ultimately depend *only* on non-bootstrapped
	% modules (like {meta,text}_utils) - this should be the case here:
	%
	%display_debug( "Resulting module information:~n~s",
	%		   [ module_info_to_string( ModuleInfo ) ] ),

	case ModuleInfo#module_info.unhandled_forms of

		[] ->
			ok;

		UnhandledForms ->

			UnHandledStrings = [ text_utils:format( "~p", [ Form ] )
								 || { _Loc, Form } <- UnhandledForms ],

			display_warning( "~B forms have not be handled: ~s",
							 [ length( UnhandledForms ),
						 text_utils:strings_to_string( UnHandledStrings ) ] )

	end,

	% Additional linting, just after extraction:
	check_module_info( ModuleInfo ),

	ModuleInfo.


pre_check_ast( AST ) ->

	%display_debug( "~p", [ AST ] ),

	% Directly outputing the warnings or errors is generally useless; for
	% example, in addition to:
	%
	%  simple_parse_transform_target.erl:68: type void() undefined
	%
	% We would get: [{"simple_parse_transform_target.erl",
	%               [{68,erl_lint,{undefined_type,{void,0}}}]}]

	% Finally interpret_issue_reports/1 directly used to output the issues;
	% however some are legit (ex: 'type void() undefined'), so we must let them
	% go through:
	%
	case erl_lint:module( AST ) of

		{ ok, _Warnings=[] } ->
			%display_trace( "(no warning or error emitted)~n" ),
			ok;

		{ ok, Warnings } ->
			%display_error( "Warnings, reported as errors: ~p~n",
			%		   [ Warnings ] ),
			interpret_issue_reports( Warnings ),
			%exit( warning_reported );
			warning_reported;

		{ error, Errors, _Warnings=[] } ->
			%display_error( "Errors reported: ~p~n", [ Errors ] ),
			interpret_issue_reports( Errors ),
			%exit( error_reported );
			error_reported;

		{ error, Errors, Warnings } ->
			%display_error( "Errors reported: ~p~n", [ Errors ] ),
			interpret_issue_reports( Errors ),

			%display_error( "Warnings, reported as errors: ~p~n",
			%		   [ Warnings ] ),
			interpret_issue_reports( Warnings ),
			%exit( error_reported )
			error_reported

	end.



% Here all relevant parts of the specified AST (located forms) are matched in
% turn, and stored in the specified module_info once located using
% id_utils:sortable_id/0 identifiers, which allows easy insertions and
% reordering.



% Module handling:

% Any lacking, invalid or duplicated module declaration will be caught by the
% compiler anyway:
%
-spec process_ast( ast(), module_info() ) -> module_info().
process_ast( AST, ModuleInfo ) ->
	%display_debug( "Starting the AST processing..." ),
	process_ast( AST, ModuleInfo, id_utils:get_initial_sortable_id() ).



% (longer helper)
%
-spec process_ast( located_ast(), module_info(), id_utils:sortable_id() ) ->
						 module_info().

% Module handling:
process_ast( _AST=[ Form={ attribute, _Line, module, ModuleName } | T ],
			 W=#module_info{ module=undefined, module_def=undefined },
			 NextLocation ) ->

	%display_debug( "module declaration for ~s", [ ModuleName ] ),

	% When processing X.beam, we should not remove the lines like:
	% {attribute,37,file,{"X.erl",37} as they allow to report errors
	% appropriately.

	LocForm = { NextLocation, Form },

	process_ast( T, W#module_info{ module=ModuleName, module_def=LocForm },
				 id_utils:get_next_sortable_id( NextLocation ) );



% Include handling:
process_ast( _AST=[ Form={ attribute, _Line, file, { Filename, _N } } | T ],
			 W=#module_info{ includes=Inc, include_defs=IncDefs },
			 NextLocation ) ->

	%display_debug( "file declaration with ~s", [ Filename ] ),

	% We used to normalise paths, however then 'file_utils' would have to be
	% bootstrapped as well, which does not seem desirable.

	%NormFilename = file_utils:normalise_path( Filename ),
	NormFilename = Filename,

	% Avoids duplicates (in 'includes' only, not in definitions):
	%
	NewFilenames = case lists:member( NormFilename, Inc ) of

		true ->
			Inc;

		false ->
			[ NormFilename | Inc ]

	end,

	LocForm = { NextLocation, Form },

	process_ast( T, W#module_info{ includes=NewFilenames,
								   include_defs=[ LocForm | IncDefs ] },
				 id_utils:get_next_sortable_id( NextLocation ) );



% Type definition handling:
process_ast( _AST=[ Form={ attribute, _Line, type,
						   { TypeName, TypeDef, _SubTypeList } } | T ],
			 W=#module_info{ type_definitions=TypeDefs,
							 type_definition_defs=TypeDefsDefs },
			 NextLocation ) ->

	%display_debug( "(non-opaque) type declaration for ~p: ~p", [
	%                                             TypeName, Form ] ),

	LocForm = { NextLocation, Form },

	process_ast( T, W#module_info{
				   type_definitions=[ { TypeName, TypeDef, _IsOpaque=false }
									  | TypeDefs ],
				   type_definition_defs=[ LocForm | TypeDefsDefs ] },
				 id_utils:get_next_sortable_id( NextLocation ) );


process_ast( _AST=[ Form={ attribute, _Line, opaque,
						   { TypeName, TypeDef, _SubTypeList } } | T ],
			 W=#module_info{ type_definitions=TypeDefs,
							 type_definition_defs=TypeDefsDefs },
			 NextLocation ) ->

	%display_debug( "opaque type declaration for ~p: ~p", [ TypeName, Form ] ),

	LocForm = { NextLocation, Form },

	process_ast( T, W#module_info{
				   type_definitions=[ { TypeName, TypeDef, _IsOpaque=true }
									  | TypeDefs ],
				   type_definition_defs=[ LocForm | TypeDefsDefs ] },
				 id_utils:get_next_sortable_id( NextLocation ) );



% Type export handling:
process_ast( _AST=[ Form={ attribute, _Line, export_type, DeclaredTypes } | T ],
			 W=#module_info{ type_exports=TypeExports,
							 type_export_defs=TypeExportDefs },
			 NextLocation ) when is_list( DeclaredTypes ) ->

	%display_debug( "export type declaration for ~p", [ DeclaredTypes ] ),

	LocForm = { NextLocation, Form },

	process_ast( T, W#module_info{ type_exports= DeclaredTypes ++ TypeExports,
								   type_export_defs=
									   [ LocForm | TypeExportDefs ] },
				 id_utils:get_next_sortable_id( NextLocation ) );



% Function export handling:
process_ast( _AST=[ _Form={ attribute, Line, export, FunctionIds } | T ],
			 W=#module_info{ function_exports=ExportTable,
							 functions=FunctionTable }, NextLocation ) ->

	%display_debug( "export declaration for ~p", [ FunctionIds ] ),

	NewFunctionTable = lists:foldl(

		fun( FunId={ Name, Arity }, FunTableAcc ) ->

			NewFunInfo = case ?table:lookupEntry( FunId, FunTableAcc ) of

				 key_not_found ->

					% New entry then:
					#function_info{ name=Name,
									arity=Arity,
									% Implicit:
									%location=undefined
									%line=undefined
									%definition=[],
									%spec=undefined
									exported=[ NextLocation ] };

				 % A function *might* be exported more than once:
				 { value, FunInfo } -> % F=#function_info{ exported=false } } ->
					% Just add the fact that the function is exported then:
					NewExp = [ NextLocation | FunInfo#function_info.exported ],
					FunInfo#function_info{ exported=NewExp }

			end,

			?table:addEntry( FunId, NewFunInfo, FunTableAcc )

		end,
		_Acc0=FunctionTable,
		_List=FunctionIds ),

	% Initially, exactly one export entry per location:
	NewExportTable = ?table:addNewEntry( NextLocation, { Line, FunctionIds },
										 ExportTable ),

	process_ast( T, W#module_info{
					  function_exports=NewExportTable,
					  functions=NewFunctionTable },
				 id_utils:get_next_sortable_id( NextLocation ) );


% Record definition handling:
process_ast( _AST=[ Form={ attribute, _Line, import,
							{ ModuleName, FunIds } } | T ],
			 W=#module_info{ function_imports=ImportTable,
							 function_imports_defs=ImportDefs },
					NextLocation ) ->

	NewImportTable = ?table:appendListToEntry( ModuleName, FunIds,
											   ImportTable ),

	NewImportDefs = [ { NextLocation, Form } | ImportDefs ],

	process_ast( T, W#module_info{ function_imports=NewImportTable,
								   function_imports_defs=NewImportDefs },
				 id_utils:get_next_sortable_id( NextLocation ) );



% Compilation option handling:

% Full inlining:
process_ast( _AST=[ Form={ attribute, _Line, compile, inline } | T ],
			 W=#module_info{ compilation_options=CompileTable,
							 compilation_option_defs=CompileDefs },
					NextLocation ) ->

	NewCompileTable = ?table:appendListToEntry( inline, all, CompileTable ),

	NewCompileDefs = [ { NextLocation, Form } | CompileDefs ],

	process_ast( T, W#module_info{ compilation_options=NewCompileTable,
								   compilation_option_defs=NewCompileDefs },
				 id_utils:get_next_sortable_id( NextLocation ) );


% Regular inlining:
process_ast( _AST=[ Form={ attribute, _Line, compile,
						   { inline, InlineOpts } } | T ],
			 W=#module_info{ compilation_options=CompileTable,
							 compilation_option_defs=CompileDefs },
					NextLocation ) when is_list( InlineOpts ) ->

	InlineOpt = case ?table:lookupEntry( inline, CompileTable ) of

		{ value, all } ->
			all;

		{ value, InlineList } ->
			InlineOpts ++ InlineList;

		key_not_found ->
			InlineOpts

	end,

	NewCompileTable = ?table:addEntry( inline, InlineOpt, CompileTable ),

	NewCompileDefs = [ { NextLocation, Form } | CompileDefs ],

	process_ast( T, W#module_info{ compilation_options=NewCompileTable,
								   compilation_option_defs=NewCompileDefs },
				 id_utils:get_next_sortable_id( NextLocation ) );


process_ast( _AST=[ Form={ attribute, _Line, compile,
							{ CompilationOption, Options } } | T ],
			 W=#module_info{ compilation_options=CompileTable,
							 compilation_option_defs=CompileDefs },
					NextLocation ) ->

	NewCompileTable = ?table:appendListToEntry( CompilationOption, Options,
												CompileTable ),

	NewCompileDefs = [ { NextLocation, Form } | CompileDefs ],

	process_ast( T, W#module_info{ compilation_options=NewCompileTable,
								   compilation_option_defs=NewCompileDefs },
				 id_utils:get_next_sortable_id( NextLocation ) );


% Record handling:

process_ast( _AST=[ Form={ attribute, _Line, record,
							{ RecordName, DescFields } } | T ],
			 W=#module_info{ records=RecordTable,
							 record_defs=RecordDefs },
					NextLocation ) ->

	FieldTable = process_field_descriptions( DescFields ),

	NewRecordTable = ?table:addNewEntry( RecordName, FieldTable, RecordTable ),

	NewRecordDefs = [ { NextLocation, Form } | RecordDefs ],

	process_ast( T, W#module_info{ records=NewRecordTable,
								   record_defs=NewRecordDefs },
				 id_utils:get_next_sortable_id( NextLocation ) );


% Function definition handling:
process_ast( _AST=[ _Form={ function, Line, Name, Arity, Clauses } | T ],
			 W=#module_info{ functions=FunctionTable }, NextLocation ) ->

	%display_debug( "function definition for ~p/~p", [ Name, Arity ] ),

	% The non-first clauses could be checked as well:
	%
	% (when adding a function, we may not check if ever there was a pre-existing
	% one - multiple definitions will be rejected by the compiler anyway)

	FunId = { Name, Arity },

	FunInfo = case ?table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->
			% New entry then:
			#function_info{ name=Name,
							arity=Arity,
							location=NextLocation,
							line=Line,
							definition=Clauses
							% Implicit:
							%spec=undefined
							%exported=[]
						  };

		{ value, F=#function_info{ definition=[] } } ->
				% Already here because of an export; just add the missing
				% information then:
				F#function_info{ location=NextLocation,
								 line=Line,
								 definition=Clauses };

		% Here a definition was already set:
		_ ->
			raise_error( { multiple_definition_for, FunId } )

	end,

	NewFunctionTable = ?table:addEntry( _K=FunId, _V=FunInfo, FunctionTable ),

	%display_debug( "function ~s/~B with ~B clauses registered.",
	%			   [ Name, Arity, length( Clauses ) ] ),

	process_ast( T, W#module_info{ functions=NewFunctionTable },
				 id_utils:get_next_sortable_id( NextLocation ) );



% Spec attributes:
process_ast( _AST=[ Form={ attribute, _Line, spec, {
											   FunId={ FunctionName, Arity },
											   _SpecList } } | T ],
			 W=#module_info{ functions=FunctionTable },
			 NextLocation ) ->

	%display_debug( "spec definition for ~p/~p", [ FunctionName, Arity ] ),

	LocatedSpec = { NextLocation, Form },

	FunInfo = case ?table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->

			% New entry then:
			#function_info{ name=FunctionName,
							arity=Arity,
							% Implicit:
							%location=undefined,
							%line=undefined,
							%definition=[]
							spec=LocatedSpec };

		{ value, F=#function_info{ spec=undefined } } ->
			% Just add the form then:
			F#function_info{ spec=LocatedSpec };

		% Here a spec was already set:
		_ ->
			raise_error( { multiple_spec_for, FunId } )

	end,

	NewFunctionTable = ?table:addEntry( _K=FunId, _V=FunInfo, FunctionTable ),

	%display_debug( "spec for function ~s/~B registered.",
	%		   [ FunctionName, Arity ] ),

	process_ast( T, W#module_info{ functions=NewFunctionTable },
				 id_utils:get_next_sortable_id( NextLocation ) );



% Other attribute handling:
process_ast( _AST=[ Form={ attribute, _Line, AttributeName, AttributeValue }
					| T ],
			 W=#module_info{ parse_attributes=ParseAttributeTable,
							 parse_attribute_defs=AttributeDefs },
			 NextLocation ) ->

	%display_debug( "attribute definition for ~p", [ AttributeName ] ),

	LocForm = { NextLocation, Form },

	process_ast( T, W#module_info{
				   parse_attributes=?table:addEntry( AttributeName,
										  AttributeValue, ParseAttributeTable ),
				   parse_attribute_defs=[ LocForm | AttributeDefs ] },
				 id_utils:get_next_sortable_id( NextLocation ) );



% We expect the module name to be known when ending the processing:
process_ast( _AST=[ _Form={ eof, _Line } ],
			 Infos=#module_info{ module=undefined }, _NextLocation ) ->
	raise_error( { eof_while_no_module, Infos } );



% Form expected to be defined once, and to be the last one:
process_ast( _AST=[ Form={ eof, _Line } ], W=#module_info{ last_line=undefined,
							   module=Module, includes=Inc }, _NextLocation ) ->

	%display_debug( "eof declaration at ~p", [ Line ] ),

	% Surely not wanting anything past it:
	LocForm = { id_utils:get_sortable_id_upper_bound(), Form },

	% End of file found, doing some housekeeping.

	% We do not want to have the filename of the currently processed module in
	% the includes:

	% Reconstructs the supposedly deduced module filename:
	ModFilename = atom_to_list( Module ) ++ ".erl",

	% Due to the removal of include duplicates, can be listed only up to once:
	NoModInc = lists:delete( ModFilename, Inc ),

	% Only "normal", non-recursing exit of that function:
	W#module_info{ includes=NoModInc, last_line=LocForm };


% Handling errors:
%
% Apparently parse transforms *have* to manage errors by themselves. Indeed we
% stored them in the unhandled_forms field of the module_info record and
% reinjected them in the returned AST, hoping that the compiler chain would
% complain appropriately (at least as it does without parse transforms).
%
% However it does not seem to be the case, as reinserting the error forms (where
% they were, thanks to their location information) results in (when adding an
% intentional error in the source file):
%
% foo.erl: internal error in lint_module;
% crash reason: badarg
%
% in function  erl_anno:set/3
%    called as erl_anno:set(file,"class_Mesh.erl",undefined)
% in call from erl_lint:set_form_file/2 (erl_lint.erl, line 690)
% in call from erl_lint:eval_file_attr/2 (erl_lint.erl, line 679)
% in call from erl_lint:forms/2 (erl_lint.erl, line 641)
% in call from erl_lint:module/3 (erl_lint.erl, line 498)
% in call from compile:lint_module/1 (compile.erl, line 999)
% in call from compile:'-internal_comp/4-anonymous-1-'/2 (compile.erl, line 295)
% in call from compile:fold_comp/3 (compile.erl, line 321)
%
% So we manage the errors by ourselves (losing the emacs error mode support).


% (for some reason, the reported lines are incremented, we have to decrement
% them)

% Preprocessor (eep) errors:
process_ast( _AST=[ _Form={ error,
	   { Line, epp, { include, file, FileName } } } | _T ], _Infos,
			 _NextLocation ) ->

	raise_error( { include_file_not_found, FileName, { line, Line-1 } } );


process_ast( _AST=[ _Form={ error,
	   { Line, epp, { undefined, VariableName, none } } } | _T ], _Infos,
			 _NextLocation ) ->

	raise_error( { undefined_macro_variable, VariableName, { line, Line-1 } } );


process_ast( _AST=[ _Form={ error, { Line, epp, Reason } } | _T ], _Infos,
			 _NextLocation ) ->

	raise_error( { preprocessing_failed, Reason, { line, Line-1 } } );



% Parser (erl_parse) errors:

process_ast( _AST=[ _Form={ error, { Line, erl_parse, Reason } } | _T ],
			 _Infos, _NextLocation ) ->
	raise_error( { parsing_failed, text_utils:format( Reason, [] ),
			   { line, Line-1 } } );


% Catch-all, to ensure that we captured all possible forms:
%
process_ast( _AST=[ Form | T ], W=#module_info{
									 unhandled_forms=UnhandledForms },
			 NextLocation ) ->

	% display_warning( "unhandled form '~p' not managed.~n", [ Form ] ),
	% raise_error( { unhandled_form, Form } );

	LocForm = { NextLocation, Form },

	NewUnhandledForms = [ LocForm | UnhandledForms ],

	process_ast( T, W#module_info{ unhandled_forms=NewUnhandledForms },
				 id_utils:get_next_sortable_id( NextLocation ) );


process_ast( _AST=[], Infos, _NextLocation ) ->
	raise_error( { no_eof_found, Infos } ).



% Processes the fields of a given record.
%
-spec process_field_descriptions( [ ast_utils:ast_element() ] ) ->
										field_table().
process_field_descriptions( FieldDescriptions ) ->

	FieldTable = ?table:new(),

	process_field_descriptions( FieldDescriptions, FieldTable ).


process_field_descriptions( _FieldDescriptions=[], FieldTable ) ->
	FieldTable;


% Here no type or default value are specified for that field:
process_field_descriptions( _FieldDescriptions=[
	   { record_field, _Line1, { atom, _Line2, FieldName } } | T ],
							FieldTable ) ->
	NewFieldTable = ?table:addNewEntry( FieldName,
		  { _FieldType=undefined, _DefaultValue=undefined }, FieldTable ),
	process_field_descriptions( T, NewFieldTable );

% Here only a type is specified for that field:
process_field_descriptions( _FieldDescriptions=[
	   { typed_record_field,
		  {record_field, _Line1, { atom, _Line2, FieldName } }, FieldType }
												| T ], FieldTable ) ->
	NewFieldTable = ?table:addNewEntry( FieldName,
					  { FieldType, _DefaultValue=undefined }, FieldTable ),
	process_field_descriptions( T, NewFieldTable );

% Here only a default value is specified for that field:
process_field_descriptions( _FieldDescriptions=[
	   { record_field, _Line1, { atom, _Line2, FieldName }, DefaultValue }
												| T ], FieldTable ) ->
	NewFieldTable = ?table:addNewEntry( FieldName,
		  { _FieldType=undefined, DefaultValue }, FieldTable ),
	process_field_descriptions( T, NewFieldTable );

% Here a type and a default, immediate value are specified for that field:
process_field_descriptions( _FieldDescriptions=[
	   { typed_record_field,
		  { record_field, _Line1,
		   { atom, _Line2, FieldName }, DefaultValue }, FieldType }
												| T ], FieldTable ) ->
	NewFieldTable = ?table:addNewEntry( FieldName, { FieldType, DefaultValue },
										FieldTable ),
	process_field_descriptions( T, NewFieldTable );


process_field_descriptions( _FieldDescriptions=[ UnexpectedDesc | _T ],
							_FieldTable ) ->
	throw( { unexpected_field_description, UnexpectedDesc } ).



% Returns a textual description of the specified located AST.
%
% Note: relies on text_utils.
%
-spec located_ast_to_string( located_ast() ) -> text_utils:string().
located_ast_to_string( AST ) ->

	% Raw, not sorted on purpose:
	Strings = [ text_utils:format( "at ~s: ~p",
					[ id_utils:sortable_id_to_string( Loc ), Form ] )
				|| { Loc, Form } <- AST ],

	text_utils:strings_to_string( Strings ).




% Recomposes an AST from specified module information.
%
-spec recompose_ast_from_module_info( module_info() ) -> ast().
recompose_ast_from_module_info( #module_info{

			% Between parentheses: fields unused here, hence not bound.

			% Note: one should regularly check that all relevant fields of
			% module_info() are indeed read here, so that they are reinjected
			% indeed in the output AST.

			% (module)
			module_def=ModuleDef,

			% (compilation_options)
			compilation_option_defs=CompileOptDefs,

			% (parse_attributes)
			parse_attribute_defs=ParseAttributeDefs,

			% (includes)
			include_defs=IncludeDefs,

			% (type_definitions)
			type_definition_defs=TypeDefsDefs,

			% (type_exports)
			type_export_defs=TypeExportsDefs,

			% (records)
			record_defs=RecordDefs,

			% (function_imports)
			function_imports_defs=ImportDefs,

			function_exports=ExportTable,

			% The main part of the AST:
			functions=Functions,

			last_line=LastLineDef,

			unhandled_forms=UnhandledForms

								  } ) ->

	ExportInfos = ?table:enumerate( ExportTable ),

	%display_debug( "ExportInfos = ~p", [ ExportInfos ] ),

	ExportLocDefs = [ { Loc, { attribute, Line, export, FunIds } }
				   || { Loc, { Line, FunIds } } <- ExportInfos ],

	FunctionDefs = get_located_forms_for_functions( Functions ),

	% All these definitions are located, yet we start from a sensible order so
	% that inserted forms do not end up in corner cases:
	%
	% (order does not really matter thanks to explicit locations)
	%
	UnorderedLocatedAST = [ ModuleDef |
							   ParseAttributeDefs
							++ ExportLocDefs
							++ IncludeDefs
							++ ImportDefs
							++ CompileOptDefs
							++ RecordDefs
							++ TypeDefsDefs
							++ TypeExportsDefs
							++ FunctionDefs
							++ [ LastLineDef | UnhandledForms ] ],

	%display_debug( "Unordered located AST:~n~p~n", [ UnorderedLocatedAST ] ),

	OrderedAST = get_ordered_ast_from( UnorderedLocatedAST ),

	%display_debug( "Recomposed AST:~n~p~n", [ OrderedAST ] ),

	OrderedAST.



% Returns a pair made of:
%
% - an export-related table whose keys are locations of export attributes and
% whose values are lists of function identifiers that shall be declared there
% for export)
%
% - a list of the located forms corresponding to all the functions definition
% and spec that are described in the specified function table.
%
-spec get_located_forms_for_functions( ?table:?table() ) -> [ located_form() ].
get_located_forms_for_functions( FunctionTable ) ->

	% Dropping the keys (function_id(), i.e. function identifiers), focusing on
	% function_info():
	%
	FunInfos = ?table:values( FunctionTable ),

	lists:foldl( fun( #function_info{ name=Name,
									  arity=Arity,
									  location=Location,
									  line=Line,
									  definition=Clauses,
									  spec=MaybeSpec }, Acc ) ->

						 FunForm = { Location,
									 { function, Line, Name, Arity, Clauses } },

						 case MaybeSpec of

							 undefined ->
								 [ FunForm | Acc ];

							 LocSpecForm ->
								 [ LocSpecForm, FunForm | Acc ]

						 end

				 end,
				 _Acc0=[],
				 _List=FunInfos ).



% Returns an (ordered, with no location information) AST from the specified
% unordered, located AST.
%
-spec get_ordered_ast_from( located_ast() ) -> ast().
get_ordered_ast_from( UnorderedLocatedAST ) ->

	% First pass: we replace any 'auto_located' location by an actual position
	% just after the current one (collisions are allowed):
	%
	FullyLocatedAST = locate_all_in( UnorderedLocatedAST,
						id_utils:get_initial_sortable_id(), _Acc=[] ),

	% We then sort form according to their recorded location:
	OrderedLocatedAST = lists:keysort( _LocIndex=1, FullyLocatedAST ),

	% One of the most useful view of output:
	display_debug( "Ordered located AST:~n~s~n",
				   [ located_ast_to_string( OrderedLocatedAST ) ] ),

	% And then we remove that information once sorted, returning an ordered,
	% unlocated AST:
	%
	[ Form || { _Location, Form } <- OrderedLocatedAST ].



locate_all_in( _LocatedForms=[], _CurrentSortId, Acc ) ->
	% Order does not matter anymore:
	Acc;

locate_all_in( _LocatedForms=[ { _Loc=auto_located, Form } | T ], CurrentSortId,
			   Acc ) ->
	% Better than get_next_sortable_id/1 to ensure grouped with previous:
	NewSortId = id_utils:get_higher_next_depth_sortable_id( CurrentSortId ),
	locate_all_in( T, NewSortId, [ { NewSortId, Form } | Acc ] );

locate_all_in( _LocatedForms=[ LocForm={ ActualLoc, _Form } | T ],
			   _CurrentSortId, Acc ) ->
	locate_all_in( T, ActualLoc, [ LocForm | Acc ] ).






% Reads specified Erlang source file (*.erl) and returns the corresponding AST.
%
% For example useful to debug a parse transform first separately from the
% compile pipe-line, relying here on the usual, convenient error management
% instead of having little informative messages like: 'undefined parse transform
% 'foobar'' as soon as a call to a non-existing module:function/arity is made.
%
-spec erl_to_ast( file_utils:file_name() ) -> ast().
erl_to_ast( ErlSourceFilename ) ->

	case epp:parse_file( ErlSourceFilename, _Opts=[] ) of

		{ error, Error } ->
			throw( { parse_file_failed, ErlSourceFilename, Error } );

		{ ok, AST } ->
			AST

	end.



% Checks the correctness of specified module information.
%
-spec check_module_info( module_info() ) -> basic_utils:void().
check_module_info( #module_info{ module=undefined } ) ->
	raise_error( no_module_known );

check_module_info( #module_info{ module_def=undefined } ) ->
	raise_error( no_module_defined );

check_module_info( #module_info{ last_line=undefined } ) ->
	raise_error( no_last_line_found );


check_module_info( ModuleInfo=#module_info{ unhandled_forms=[] } ) ->
	%display_debug( "Checking AST." ),
	check_module_parse( ModuleInfo ),
	check_module_include( ModuleInfo ),
	check_module_type_definition( ModuleInfo ),
	check_module_export( ModuleInfo ),
	check_module_functions( ModuleInfo );

check_module_info( #module_info{ unhandled_forms=UnhandledForms } ) ->

	Forms = [ F || { _Loc, F } <- UnhandledForms ],

	raise_error( { unhandled_forms, Forms } ).



% Helper to check module parsed attributes.
%
check_module_parse( #module_info{
						 parse_attributes=ParseAttributeTable,
						 parse_attribute_defs=ParseAttributeDefs } ) ->

	Len = ?table:size( ParseAttributeTable ),

	case length( ParseAttributeDefs ) of

		Len ->
			ok;

		FormCount ->
			display_error( "Inconsistent parse attribute state: table "
						   "of ~B entries: ~s~nvs ~B forms:~n~p~n.",
						   [ Len, ?table:toString( ParseAttributeTable ),
							 FormCount, ParseAttributeDefs ] ),
			raise_error( { parse_attribute_mismatch,
						   ?table:enumerate( ParseAttributeTable ),
						   ParseAttributeDefs } )

	end.


% Helper to check module includes.
%
check_module_include( #module_info{
						 includes=Includes,
						 include_defs=IncludeDefs } ) ->

	Len = length( Includes ),

	case length( IncludeDefs ) of

		% Includes are filtered (ex: for duplicates):
		L when L < Len ->
			raise_error( { include_mismatch, Includes,
						   IncludeDefs } );

		_ ->
			ok

	end.


% Helper to check module type definitions.
%
check_module_type_definition( #module_info{
								 type_definitions=TypeDefs,
								 type_definition_defs=TypeDefsDefs } ) ->

	Len = length( TypeDefs ),

	case length( TypeDefsDefs ) of

		Len ->
			ok;

		_ ->
			raise_error( { type_definition_mismatch, TypeDefs,
						   TypeDefsDefs } )

	end.


% Helper to check module type exports.
%
check_module_export( #module_info{ type_exports=TypeExports,
								   type_export_defs=TypeExportDefs } ) ->

	Len = length( TypeExports ),

	case length( TypeExportDefs ) of

		% A single export attribute can export monre than one type:
		%
		L when L > Len ->
			raise_error( { type_export_mismatch, TypeExports,
						   TypeExportDefs } );

		_ ->
			ok

	end.



% Helper to check module functions.
%
check_module_functions( #module_info{ functions=Functions } ) ->

	FunInfos = ?table:enumerate( Functions ),

	[ check_function( FunId, FunInfo ) || { FunId, FunInfo } <- FunInfos ].



% Nothing to check for 'spec' or 'exported':
%
check_function( FunId, _FunInfo=#function_info{ definition=[] } ) ->
	raise_error( { no_definition_found_for, FunId } );

check_function( _FunId={ Name, Arity }, _FunInfo=#function_info{
													name=Name,
													arity=Arity } ) ->
	% Match:
	ok;

check_function( FunId, _FunInfo=#function_info{
								   name=SecondName,
								   arity=SecondArity } ) ->
	raise_error( { definition_mismatch, FunId,
				   { SecondName, SecondArity } } ).



% Returns a textual description of specified module information.
%
% Note: the location information is dropped for all located definitions.
%
-spec module_info_to_string( module_info() ) -> text_utils:ustring().
module_info_to_string( #module_info{
						 module=Module,
						 module_def={ _, _ModuleDef },
						 compilation_options=CompileTable,
						 compilation_option_defs=_CompileOptDefs,
						 parse_attributes=ParseAttributeTable,
						 parse_attribute_defs=_ParseAttributeDefs,
						 includes=Includes,
						 include_defs=_IncludeDefs,
						 type_definitions=TypeDefs,
						 type_definition_defs=_TypeDefsDefs,
						 type_exports=TypeExports,
						 type_export_defs=_TypeExportDefs,
						 records=RecordTable,
						 record_defs=_RecordDefs,
						 function_imports=FunImportTable,
						 function_imports_defs=_FunImportDefs,
						 function_exports=_FunctionExports,
						 functions=Functions,
						 last_line=LastLine,
						 unhandled_forms=UnhandledForms } ) ->

	FunctionStrings = [ io_lib:format( "~s",
									   [ function_info_to_string( Info ) ] )
						|| { _FunId, Info } <- ?table:enumerate( Functions ) ],

	LastLineString = case LastLine of

		undefined ->
			"unknown";

		{ _Loc, { eof, Count } } ->
			text_utils:format( "~B", [ Count ] )

	end,

	% To mark an additional offset for the sublists:
	NextIndentationLevel = 1,

	UnhandledString = case UnhandledForms of

		[] ->
			"all forms handled";

		_ ->
			UnhandledStrings = [ text_utils:format( "~p", [ Form ] )
								 || { _Loc, Form } <- UnhandledForms ],

			text_utils:format( "~B unhandled forms:~s",
							   [ length( UnhandledForms ),
								 text_utils:strings_to_string( UnhandledStrings,
											NextIndentationLevel ) ] )
	end,

	% Commented-out: the raw terms that correspond to the higher-level form
	% output just above.

	ParseAttributes = ?table:enumerate( ParseAttributeTable ),

	Infos = [

			%text_utils:format( "module name: '~s'", [ Module ] ),
			%text_utils:format( "module definition: ~p~n", [ ModuleDef ] ),

			case ?table:enumerate( CompileTable ) of

				[] ->
					"no compile option defined";

				CompileOpts ->
					CompStrings = [ text_utils:format( "for option '~s': ~p",
													   [ OptName, OptValue ] )
									|| { OptName, OptValue } <- CompileOpts ],
					text_utils:format( "~B compile options defined: ~s~n",
						   [ length( CompileOpts ),
							 text_utils:strings_to_string( CompStrings ) ] )

			end,

			 % Like: -foo( bar ).
			case ParseAttributes of

				[] ->
					"no parse attribute defined";

				_ ->
					ParseAttrString = text_utils:strings_to_sorted_string( [
							text_utils:format( "attribute '~s' set to: '~p'",
											   [ AttrName, AttrValue ] )
							 || { AttrName, AttrValue } <- ParseAttributes ],
							 NextIndentationLevel ),

					text_utils:format( "~B parse attributes defined:~s",
									   [ length( ParseAttributes ),
										 ParseAttrString ] )

			end,

			%text_utils:format( "parse attribute definitions: ~p~n",
			%				   [ [ P || { _, P } <- ParseAttributeDefs ] ] ),

			case Includes of

				[] ->
					"no file included";

				_ ->
					IncludeString = text_utils:strings_to_sorted_string( [
							text_utils:format( "'~s' included", [ Inc ] )
									   || Inc <- Includes ],
									   NextIndentationLevel ),
					text_utils:format( "~B includes specified:~s",
									   [ length( Includes ), IncludeString ] )

			end,

			%text_utils:format( "include definitions: ~p~n",
			%					 [ [ I || { _, I } <- IncludeDefs ] ] ),

			case TypeDefs of

				[] ->
					"no type defined";

				_ ->
					TypeDefString = text_utils:strings_to_sorted_string( [
							begin
								VisibleString = case IsOpaque of

									true ->
										"opaque";

									false ->
										""

								end,
								text_utils:format(
								  "~s type '~s' defined as: ~p",
								  [ VisibleString, Type, Def ] )

							end || { Type, Def, IsOpaque } <- TypeDefs ],
							NextIndentationLevel ),


					text_utils:format( "~B types defined:~s",
									   [ length( TypeDefs ), TypeDefString ] )

			end,

			%text_utils:format( "type definitions: ~p~n",
			%				   [ [ T || { _, T } <- TypeDefsDefs ] ] ),


			case TypeExports of

				[] ->
					"no type exported";

				_ ->
					TypeExpString = text_utils:strings_to_sorted_string( [
							text_utils:format( "~s/~B", [ Type, TypeArity ] )
									   || { Type, TypeArity } <- TypeExports ],
									   NextIndentationLevel ),
					text_utils:format( "~B type exports:~s",
								   [ length( TypeExports ), TypeExpString ] )

			end,

			 %text_utils:format( "type export definitions: ~p~n",
			 %				   [ [ E || { _, E } <- TypeExportDefs ] ] ),

			 case ?table:enumerate( RecordTable ) of

				[] ->
					 "no record declared";

				RecordEntries ->
					 RecordString = text_utils:strings_to_sorted_string( [

						begin

							FieldStrings = fields_to_strings( FieldTable ),

							FieldString = text_utils:strings_to_string(
										FieldStrings, NextIndentationLevel+1 ),

							text_utils:format(
							  "record '~s' having ~B fields:~s",
							  [ RecordName, length( FieldStrings ),
								FieldString ] )

						end || { RecordName, FieldTable } <- RecordEntries ],
						NextIndentationLevel ),
					 text_utils:format( "~B records defined:~s",
								   [ length( RecordEntries ), RecordString ] )

			 end,

			 case ?table:enumerate( FunImportTable ) of

				 [] ->
					 "no function imported";

				 ImportEntries ->
					 ImpString = text_utils:strings_to_sorted_string(
					   [ text_utils:format( "from module '~s': ~p",
							[ ModName, FunIds ] )
						 || { ModName, FunIds } <- ImportEntries ] ),
					 text_utils:format(
					   "Function imports declared from ~B modules: ~s",
					   [ length( ImportEntries ), ImpString ] )

			 end,

			 %text_utils:format( "~B function export definitions: ~p~n",
			 %					[ length( FunctionExports ),
			 %					  [ F || { _, F } <- FunctionExports ] ] ),

			 case FunctionStrings of

				 [] ->
					 "no function defined";

				 _ ->
					 text_utils:format( "~B functions defined:~s",
										[ length( FunctionStrings ),
										  text_utils:strings_to_string(
											FunctionStrings,
											NextIndentationLevel ) ] )

			 end,

			 text_utils:format( "line count: ~s", [ LastLineString ] ),

			 UnhandledString

			],

	text_utils:format( "Information about module '~s':~s",
					   [ Module, text_utils:strings_to_string( Infos ) ] ).



% Returns a list of textual representation for each of the record fields in
% specified table.
%
-spec fields_to_strings( field_table() ) -> [ text_utils:string() ].
fields_to_strings( FieldTable ) ->

	FieldEntries = ?table:enumerate( FieldTable ),

	[ text_utils:format( "field '~s' described as ~p, "
						 "and having for default value ~p",
						 [ FieldName, FieldType, DefaultValue ] )
	  || { FieldName, { FieldType, DefaultValue } } <- FieldEntries ].



% Writes specified AST into specified (text) file.
%
% Useful for example to determine differences between ASTs.
%
-spec write_ast_to_file( ast(), file_utils:file_name() ) -> basic_utils:void().
write_ast_to_file( AST, Filename ) ->

	% Note: we cannot actually use file_utils, which is not a prerequisite of
	% the 'Common' parse transform:

	% We overwrite any pre-existing file:
	{ ok, File } = file:open( Filename, [ write, raw ] ),

	[ ok = file:write( File, io_lib:format( "~p~n", [ F ] )  ) || F <- AST ],

	ok = file:close( File ).



% Writes specified module_info record into specified (text) file.
%
% Useful for example to determine faulty transformations.
%
-spec write_module_info_to_file( module_info(), file_utils:file_name() ) ->
							   basic_utils:void().
write_module_info_to_file( ModuleInfo, Filename ) ->

	% Note: we cannot actually use file_utils, which is not a prerequisite of
	% the 'Common' parse transform:

	% We overwrite any pre-existing file:
	{ ok, File } = file:open( Filename, [ write, raw ] ),

	ok = file:write( File, module_info_to_string( ModuleInfo ) ),

	ok = file:close( File ).




% Raises a (compile-time, rather ad hoc) error when applying a parse transform,
% to stop the build on failure and report the actual error.
%
% Used to be a simple throw, but then for parse transforms the error message was
% garbled in messages like:
%
% """
% internal error in lint_module;
% crash reason: function_clause
%
%  in function  erl_lint:'-compiler_options/1-lc$^0/1-0-'/1
%     called as erl_lint:'-compiler_options/1-lc$^0/1-0-'({
% table_type_defined_more_than_once,{line,12},foo_hashtable,bar_hashtable})
%
% See also: raise_parse_error/ for a better, more standard system for error
% management.
%
-spec raise_error( term() ) -> no_return().
raise_error( ErrorTerm ) ->

	%throw( ErrorTerm )
	%display_error( "~p", [ ErrorTerm ] ),

	% Does not add any information (just non-relevant erl_parse, epp
	% etc. state):
	%
	%erlang:exit( { ErrorTerm, erlang:get_stacktrace() } ).

	erlang:exit( ErrorTerm ).



% Returns an AST form in order to raise a (compile-time, standard) error when
% applying a parse transform, to stop the build on failure and report the actual
% error.
%
% The specified error term will be transformed by the specified module into a
% (textual) error message (see format_error/1), and then will be reported as
% originating from the specified line in the source file of the module being
% compiled.
%
-spec get_error_form( basic_utils:error_reason(), basic_utils:module_name(),
					  ast_utils:line() ) -> form().
get_error_form( ErrorTerm, FormatErrorModule, Line ) ->

	% Actually the most standard way of reporting an error seems to insert a
	% dedicated form in the AST.

	% May ultimately report (thanks to ?MODULE:format_error/1), when compiling a
	% foobar module and if:
	%
	% - Line is 15
	%
	% - 'apply( FormatErrorModule, format_error, [ ErrorTerm ] )' is "my error
	% message":
	%
	% the following error message: "foobar:15: my error message".
	%
	{ error, { Line, FormatErrorModule, ErrorTerm } }.



% This function (whose name is standard, conventional) is to be defined on a
% per-module basis (typically in the module defining the parse transform being
% applied) and allows to convert error terms (that are, here, related to
% parse-transforms) into textual messages that can be output by the build chain.
%
-spec format_error( basic_utils:error_reason() ) -> string().
format_error( ErrorTerm ) ->

	% Of course this is just an example:
	%
	text_utils:format( "my meta_utils error reported: ~s", [ ErrorTerm ] ).



% Interprets specified list of issue reports.
%
-spec interpret_issue_reports( [ issue_report() ] ) -> basic_utils:void().
interpret_issue_reports( _IssueReports=[] ) ->
	% Should never happen:
	display_trace( "(no remark emitted)" );

% No need to further special-case the number of issue reports, as it is not
% meaningful (one may include an arbitrary long list):

%interpret_issue_reports( _IssueReports=[ OneIssueReport ] ) ->
%	interpret_issue_report( OneIssueReport );

interpret_issue_reports( IssueReports ) ->

	[ interpret_issue_report( R ) || R <- IssueReports ].

	%text_utils:format( "~B remarks: ~s", [ length( IssueReports ),
	%					text_utils:strings_to_string( ReportStrings ) ] ).


% Interprets specific issue report.
%
-spec interpret_issue_report( issue_report() ) -> basic_utils:void().
interpret_issue_report( _IssueReport={ Filename, IssueInfos } ) ->

	% We could normalise it instead, yet file_utils would become a dependency:
	CanonicFilename = filename:basename( Filename ),

	[ interpret_issue_info( CanonicFilename, E ) || E <- IssueInfos ].

	%text_utils:format( "in file '~s': ~s", [ CanonicFilename,
	%		   text_utils:strings_to_string( IssueStrings ) ] ).



% Interprets specific error description.
%
-spec interpret_issue_info( file_utils:file_name(), issue_info() ) ->
								  basic_utils:void().
interpret_issue_info( Filename,
					  _IssueInfo={ Line, DetectorModule, IssueDesc } ) ->

	% Module is the detecting one, typically erl_lint:
	%text_utils:format( "line #~B, module '~p', ~s", [ Line, Module,
	%						interpret_issue_description( IssueDesc ) ] ).

	%text_utils:format( "line #~B: ~s", [ Line,
	%		interpret_issue_description( IssueDesc, DetectorModule ) ] ).

	io:format( "~s:~B: ~s~n", [ Filename, Line,
			interpret_issue_description( IssueDesc, DetectorModule ) ] ).



% Interprets specific issue description, detected by specified module.
%
% Note: full control is offered here to enrich this function at will, if wanted.
%
-spec interpret_issue_description( issue_description(),
								   basic_utils:module_name() ) -> string().
interpret_issue_description( IssueDescription, DectectorModule ) ->
	%For example, the detector module may be erl_lint:
	DectectorModule:format_error( IssueDescription ).



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
