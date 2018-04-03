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
% Creation date: Saturday, February 3, 2018.




% Module centralising the management of all information that can be extracted
% from ASTs.
%
-module(ast_info).


% For table type:
-include("meta_utils.hrl").


% For the corresponding records:
-include("ast_info.hrl").


-type module_info() :: #module_info{}.

-type type_info() :: #type_info{}.

-type function_info() :: #function_info{}.


% Location of a form in an AST, so that the order of forms can be recreated.
%
% We use sortable identifiers so that any number of new forms can be introduced
% between any two of them, if needed.
%
% Location is relative to the position of a form in a given AST, while the line
% information embedded in forms is relative to the file in which they are
% defined.
%
% Thanks to locations (which order forms appropriately, including the ones
% regarding file references), once forms have been recomposed by design a stored
% line always is always relative to the current file.
%
% 'auto_located' means that the corresponding form is yet to be located at this
% position (thus a tranformation pass is still to be applied to needed in the
% overall list before it is sortable).
%
-type location() :: ast_base:form_location() | 'auto_located'.


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


% Located type specification of a function:
%
-type located_function_spec() :: { location(), meta_utils:function_spec() }.



% Tables to be found in the module_info record:


-type compile_option_name() :: atom().

-type compile_option_value() :: term().


% For easy access to compilation information:
%
-type compile_option_table() :: ?table:?table( compile_option_name(),
											   [ compile_option_value() ] ).



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




% A table associating, to a given location, the corresponding line in the source
% file (to recreate the corresponding export form) and a list of the identifiers
% of the types to declare exported there.
%
% Note:
%
% - this table must be explicitly updated whenever adding or removing a type
% in a module_info'types' field; see: add_type/2 and remove_type/2
%
% - [ type_id() ] used, not a set, to better preserve order
%
-type type_export_table() :: ?table:?table( location(),
										{ ast_base:line(), [ type_id() ] } ).



% A table associating to each type identifier a full type information.
%
-type type_table() :: ?table:?table( type_id(), type_info() ).




% A table associating to each record name the description of the corresponding
% record.
%
-type record_table() :: ?table:?table( basic_utils:record_name(),
									   record_definition() ).


% The full definition of a record.
%
-type record_definition() :: { field_table(), location(), line() }.



% A table associating to a given field of a record its description.
%
% The ?table type (usually map_hashtable) cannot be used, as it does not
% preserve the order of its entries, whereas the fields are indexed in
% tuple-records according to their rank in the corresponding list.

% Best solution here is not a list_table (which does not strictly preserve
% element order either), but a plain (ordered) list (of pairs).
%
-type field_table() :: [ { basic_utils:field_name(), 
						   ast_record:field_definition() } ].



% A table referencing, for each module listed, a list of the functions that are
% imported from it by the current module:
%
-type function_import_table() :: ?table:?table( basic_utils:module_name(),
												[ function_id() ] ).




% A table associating, to a given location, the corresponding line in the source
% file (to recreate the corresponding export form) and a list of the identifiers
% of the functions to declare exported there.
%
% Note:
%
% - this table must be explicitly updated whenever adding or removing a function
% in a module_info 'functions' field; see: add_function/2 and remove_function/2
%
% - [ function_id() ] used, not a set, to better preserve order
%
-type function_export_table() :: ?table:?table( location(),
						   { ast_base:line(), [ function_id() ] } ).



% A table associating to each function identifier a full function information.
%
-type function_table() :: ?table:?table( function_id(), function_info() ).



% Top-level elements:
%
-export_type([ module_info/0, type_info/0, function_info/0,
			   location/0, located_form/0, located_ast/0,
			   located_function_spec/0, attribute/0 ]).


% Tables to be found in the module_info record:
-export_type([ compile_option_table/0, attribute_table/0,
			   type_export_table/0, type_table/0, record_table/0,
			   function_import_table/0, function_export_table/0,
			   function_table/0 ]).


% General module-info helpers:
-export([ ensure_function_exported/4, ensure_function_not_exported/3,
		  located_ast_to_string/1 ]).


% Module-info section:
-export([ extract_module_info_from_ast/1, init_module_info/0,
		  check_module_info/1,
		  recompose_ast_from_module_info/1,
		  write_module_info_to_file/2, module_info_to_string/1 ]).


-export([ function_info_to_string/1 ]).


% General type-info helpers:
-export([ ensure_type_exported/4, ensure_type_not_exported/3,
		  type_info_to_string/1 ]).


% Local shorthands:

-type ast() :: ast_base:ast().
-type line() :: ast_base:line().
-type form() :: ast_base:form().
-type type_id() :: type_utils:type_id().
-type function_id() :: meta_utils:function_id().



% Section for general helpers.



% Ensures that specified function is exported at the specified location(s).
%
-spec ensure_function_exported( function_id(), [ location() ], module_info(),
					   function_export_table() ) -> function_export_table().
ensure_function_exported( _FunId, _ExportLocs=[], _ModuleInfo, ExportTable ) ->
	ExportTable;

ensure_function_exported( FunId, _ExportLocs=[ _Loc=auto_located | T ],
						  ModuleInfo, ExportTable ) ->

	% When a function export is to be auto-located, we attach it just after the
	% module definition, to avoid possibly placing an export after a function
	% definition:

	ModuleLoc = case ModuleInfo#module_info.module_def of

		undefined ->
			throw( { auto_locate_whereas_no_module_def, FunId } );

		{ MLoc, _Form } ->
			MLoc

	end,

	FunExportLoc = id_utils:get_higher_next_depth_sortable_id( ModuleLoc ),

	% This location may have already been used, thus:
	case ?table:lookupEntry( FunExportLoc, ExportTable ) of

		{ value, { Line, FunIds } } ->

			case lists:member( FunId, FunIds ) of

				true ->
					% Already registered, perfect as is, continues with the next
					% locations:
					ensure_function_exported( FunId, T, ModuleInfo,
											  ExportTable );

				false ->
					% Adding it then:
					NewEntry = { Line, [ FunId | FunIds ] },
					NewExportTable = ?table:addEntry( FunExportLoc, NewEntry ),
					ensure_function_exported( FunId, T, ModuleInfo,
											  NewExportTable )

			end;

		key_not_found ->
			% We create a new location entry then:
			NewEntry = { _DefaultLine=0, [ FunId ] },
			NewExportTable = ?table:addEntry( FunExportLoc, NewEntry ),
			ensure_function_exported( FunId, T, ModuleInfo, NewExportTable )

	end;


ensure_function_exported( FunId, _ExportLocs=[ Loc | T ], ModuleInfo,
						  ExportTable ) ->

	% Here we have an immediate location:
	case ?table:lookupEntry( Loc, ExportTable ) of

		{ value, { Line, FunIds } } ->

			case lists:member( FunId, FunIds ) of

				true ->
					% Already registered, perfect as is, continues with the next
					% locations:
					ensure_function_exported( FunId, T, ModuleInfo,
											  ExportTable );

				false ->
					% Adding it then:
					NewEntry = { Line, [ FunId | FunIds ] },
					NewExportTable = ?table:addEntry( Loc, NewEntry),
					ensure_function_exported( FunId, T, ModuleInfo,
											  NewExportTable )

			end;

		key_not_found ->
			% Not even a registered location:
			throw( { invalid_export_location, Loc, FunId } )

	end.



% Ensures that specified function is not exported at the specified location(s),
% in the specified function export table.
%
-spec ensure_function_not_exported( meta_utils:function_id(), [ location() ],
						function_export_table() ) -> function_export_table().
ensure_function_not_exported( _FunId, _ExportLocs=[], ExportTable ) ->
	ExportTable;

ensure_function_not_exported( FunId, _ExportLocs=[ Loc | T ], ExportTable ) ->

	case ?table:lookupEntry( Loc, ExportTable ) of

		{ value, { Line, FunIds } } ->

			% 0 or 1 reference expected, which is handled the same by:
			NewExportTable = case lists:delete( FunId, FunIds ) of

				[] ->
					?table:removeEntry( Loc, ExportTable );

				ShrunkFunIds ->
					?table:addEntry( Loc, { Line, ShrunkFunIds } )

			end,

			ensure_function_not_exported( FunId, T, NewExportTable );

		key_not_found ->
			throw( { inconsistent_export_location, Loc, FunId } )

	end.





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




% Processes the specified AST relative to a whole module, and returns the
% corresponding information gathered.
%
% Note: the extraction will probably fail (and stop any underlying parse
% transform) should the corresponding, specified code not be able to compile (as
% a rather precise linting is done).
%
-spec extract_module_info_from_ast( ast() ) -> module_info().
extract_module_info_from_ast( AST ) ->

	%ast_utils:display_debug( "Processing following AST:~n~p",
	%  [ AST ] ),

	%ast_utils:display_debug( "Processing AST:" ),

	%ast_utils:write_ast_to_file( AST, "original-extracted-ast.txt" ),

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

	ModuleInfo = ast_scan:scan( AST ),

	% Uncomment with care, as must ultimately depend *only* on non-bootstrapped
	% modules (like {meta,text}_utils) - this should be the case here:
	%
	%ast_utils:display_debug( "Resulting module information:~n~s",
	%		   [ module_info_to_string( ModuleInfo ) ] ),

	case ModuleInfo#module_info.unhandled_forms of

		[] ->
			ok;

		UnhandledForms ->

			UnHandledStrings = [ text_utils:format( "~p", [ Form ] )
								 || { _Loc, Form } <- UnhandledForms ],

			ast_utils:display_warning( "~B forms have not be handled: ~s",
							 [ length( UnhandledForms ),
						 text_utils:strings_to_string( UnHandledStrings ) ] )

	end,

	% Additional linting, just after extraction:
	check_module_info( ModuleInfo ),

	ModuleInfo.




% Returns a new, blank instance of the module_info record, typically to be fed
% with an input AST afterwards.
%
-spec init_module_info() -> module_info().
init_module_info() ->

	EmptyTable = ?table:new(),

	#module_info{ compilation_options=EmptyTable,
				  parse_attributes=EmptyTable,
				  type_exports=EmptyTable,
				  types=EmptyTable,
				  records=EmptyTable,
				  function_imports=EmptyTable,
				  function_exports=EmptyTable,
				  functions=EmptyTable }.




% Checks the correctness of specified module information.
%
-spec check_module_info( module_info() ) -> basic_utils:void().
check_module_info( #module_info{ module=undefined } ) ->
	ast_utils:raise_error( no_module_known );

check_module_info( #module_info{ module_def=undefined } ) ->
	ast_utils:raise_error( no_module_defined );

check_module_info( #module_info{ last_line=undefined } ) ->
	ast_utils:raise_error( no_last_line_found );


check_module_info( ModuleInfo=#module_info{ unhandled_forms=[] } ) ->
	%ast_utils:display_debug( "Checking AST." ),
	check_module_parse( ModuleInfo ),
	check_module_include( ModuleInfo ),
	check_module_types( ModuleInfo ),
	check_module_functions( ModuleInfo );

check_module_info( #module_info{ unhandled_forms=UnhandledForms } ) ->

	Forms = [ F || { _Loc, F } <- UnhandledForms ],

	ast_utils:raise_error( [ unhandled_forms, Forms ] ).



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
			ast_utils:display_error( "Inconsistent parse attribute state: ~s "
									 "vs ~B forms:~n~p",
						   [ ?table:toString( ParseAttributeTable ),
							 FormCount, ParseAttributeDefs ] ),
			ast_utils:raise_error( [ parse_attribute_mismatch,
									 ?table:enumerate( ParseAttributeTable ),
									 ParseAttributeDefs ] )

	end.



% Helper to check module includes.
%
check_module_include( #module_info{ includes=Includes,
									include_defs=IncludeDefs } ) ->

	Len = length( Includes ),

	case length( IncludeDefs ) of

		% Includes are filtered (ex: for duplicates):
		L when L < Len ->
			ast_utils:raise_error( [ include_mismatch, Includes,
									 IncludeDefs ] );

		_ ->
			ok

	end.




% Helper to check module types.
%
check_module_types( #module_info{ types=Types } ) ->

	TypeInfos = ?table:enumerate( Types ),

	[ check_type( TypeId, TypeInfo ) || { TypeId, TypeInfo } <- TypeInfos ].



% Nothing to check for 'spec' or 'exported':
%
check_type( TypeId, _TypeInfo=#type_info{ definition=[] } ) ->
	ast_utils:raise_error( [ no_definition_found_for, TypeId ] );

check_type( _TypeId={ Name, Arity }, _TypeInfo=#type_info{
										 name=Name, variables=TypeVars } ) ->

	case length( TypeVars ) of

		Arity ->
			ok;

		OtherArity ->
			ast_utils:raise_error(
			  [ type_arity_mismatch, Name, { Arity, OtherArity } ] )

	end;

check_type( TypeId, _TypeInfo=#type_info{ name=SecondName,
										  variables=TypeVars } ) ->

	SecondArity = length( TypeVars ),

	ast_utils:raise_error( [ type_definition_mismatch, TypeId,
							 { SecondName, SecondArity } ] ).



% Helper to check module functions.
%
check_module_functions( #module_info{ functions=Functions } ) ->

	FunInfos = ?table:enumerate( Functions ),

	[ check_function( FunId, FunInfo ) || { FunId, FunInfo } <- FunInfos ].



% Nothing to check for 'spec' or 'exported':
%
check_function( FunId, _FunInfo=#function_info{ clauses=[] } ) ->
	ast_utils:raise_error( [ no_clause_found_for, FunId ] );

check_function( _FunId={ Name, Arity },
				_FunInfo=#function_info{ name=Name, arity=Arity } ) ->
	% Match:
	ok;

check_function( FunId, _FunInfo=#function_info{ name=SecondName,
												arity=SecondArity } ) ->
	ast_utils:raise_error( [ function_definition_mismatch, FunId,
							 { SecondName, SecondArity } ] ).



% Recomposes an AST from specified module information.
%
-spec recompose_ast_from_module_info( module_info() ) -> ast().
recompose_ast_from_module_info( #module_info{

			% Between parentheses: fields unused here, hence not bound.

			% Note: one should regularly check that all relevant fields of
			% module_info() are indeed read here, so that they are reinjected
			% indeed in the output AST.

			% (module)
			module_def=ModuleLocDef,

			% (compilation_options)
			compilation_option_defs=CompileOptLocDefs,

			% (parse_attributes)
			parse_attribute_defs=ParseAttributeLocDefs,

			remote_spec_defs=RemoteSpecLocDefs,

			% (includes)
			include_defs=IncludeLocDefs,

			type_exports=TypeExportTable,

			types=TypeTable,

			records=RecordTable,

			% (function_imports)
			function_imports_defs=ImportLocDefs,

			function_exports=FunctionExportTable,

			% The main part of the AST:
			functions=FunctionTable,

			optional_callbacks_defs=OptCallbacksLocDefs,

			last_line=LastLineLocDef,

			unhandled_forms=UnhandledLocForms

								  } ) ->


	{ TypeExportLocDefs, TypeLocDefs } = ast_type:get_located_forms_for(
										   TypeExportTable, TypeTable ),

	RecordLocDefs = ast_record:get_located_forms_for( RecordTable ),

	{ FunExportLocDefs, FunctionLocDefs } = ast_function:get_located_forms_for(
									FunctionExportTable, FunctionTable ),


	% All these definitions are located, yet we start from a sensible order so
	% that inserted forms do not end up in corner cases:
	%
	% (order does not really matter thanks to explicit locations)
	%
	UnorderedLocatedAST = [ ModuleLocDef |
							   ParseAttributeLocDefs
							++ RemoteSpecLocDefs
							++ FunExportLocDefs
							++ IncludeLocDefs
							++ ImportLocDefs
							++ OptCallbacksLocDefs
							++ CompileOptLocDefs
							++ RecordLocDefs
							++ TypeExportLocDefs
							++ TypeLocDefs
							++ FunctionLocDefs
							++ [ LastLineLocDef | UnhandledLocForms ] ],

	%ast_utils:display_debug( "Unordered located AST:~n~p~n",
	%  [ UnorderedLocatedAST ] ),

	OrderedAST = get_ordered_ast_from( UnorderedLocatedAST ),

	ast_utils:display_debug( "Recomposed AST:~n~p~n",
							 [ OrderedAST ] ),

	OrderedAST.








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
	ast_utils:display_debug( "Ordered located AST:~n~s~n",
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
						 type_exports=TypeExports,
						 types=Types,
						 records=RecordTable,
						 function_imports=FunImportTable,
						 function_imports_defs=_FunImportDefs,
						 function_exports=_FunctionExports,
						 functions=Functions,
						 optional_callbacks_defs=OptCallbacksDefs,
						 last_line=LastLine,
						 unhandled_forms=UnhandledForms } ) ->

	FunctionStrings = [ io_lib:format( "~s",
								   [ function_info_to_string( FunInfo ) ] )
					|| { _FunId, FunInfo } <- ?table:enumerate( Functions ) ],

	TypeStrings = [ io_lib:format( "~s", [ type_info_to_string( TypeInfo ) ] )
						|| { _TypeId, TypeInfo } <- ?table:enumerate( Types ) ],

	LastLineString = case LastLine of

		undefined ->
			"unknown";

		{ _Loc, { eof, Count } } ->
			io_lib:format( "~B", [ Count ] )

	end,

	% To mark an additional offset for the sublists:
	NextIndentationLevel = 1,

	UnhandledString = case UnhandledForms of

		[] ->
			"all forms have been handled";

		_ ->
			UnhandledStrings = [ text_utils:format( "~p", [ Form ] )
								 || { _Loc, Form } <- UnhandledForms ],

			text_utils:format( "~B unhandled form(s):~s",
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
					text_utils:format( "~B compile option(s) defined:~s",
						   [ length( CompileOpts ),
							 text_utils:strings_to_string( CompStrings,
												   NextIndentationLevel ) ] )

			end,

			case OptCallbacksDefs of

				[] ->
					"no optional callback defined";

				_ ->
					text_utils:format( "~B lists of optional callback defined",
									   [ length( OptCallbacksDefs ) ] )

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

					text_utils:format( "~B parse attribute(s) defined:~s",
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
							text_utils:format( "~s", [ Inc ] )
									   || Inc <- Includes ],
									   NextIndentationLevel ),
					text_utils:format( "~B include(s) specified:~s",
									   [ length( Includes ), IncludeString ] )

			end,

			%text_utils:format( "include definitions: ~p~n",
			%					 [ [ I || { _, I } <- IncludeDefs ] ] ),

			case ?table:enumerate( TypeExports ) of

				[] ->
					"no type exported";

				TypeExportEntries ->

					TypeExpString = text_utils:strings_to_sorted_string(
						[ text_utils:format( "at line #~B:~s", [ Line,
							  text_utils:strings_to_string(
								[ text_utils:format( "~s/~B",
													 [ TypeName, TypeArity ] )
								  || { TypeName, TypeArity } <- TypeIds ],
								NextIndentationLevel + 1 ) ] )
						  || { _Loc, { Line, TypeIds } } <- TypeExportEntries ],
						NextIndentationLevel ),

					text_utils:format( "~B type export declaration(s):~s",
							   [ length( TypeExportEntries ), TypeExpString ] )

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

								% Mute variables below: NextLocation, Line:
						end || { RecordName, { FieldTable, _, _ } }
								   <- RecordEntries ],
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
						 || { ModName, FunIds } <- ImportEntries ],
								   NextIndentationLevel ),
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

			 case TypeStrings of

				 [] ->
					 "no type defined";

				 _ ->
					 text_utils:format( "~B types defined:~s",
										[ length( TypeStrings ),
										  text_utils:strings_to_string(
											TypeStrings,
											NextIndentationLevel ) ] )

			 end,

			 text_utils:format( "line count: ~s", [ LastLineString ] ),

			 UnhandledString

			],

	text_utils:format( "Information about module '~s':~s",
					   [ Module, text_utils:strings_to_string( Infos ) ] ).





% Returns a list of textual representations for each of the record fields in
% specified table.
%
-spec fields_to_strings( field_table() ) -> [ text_utils:string() ].
fields_to_strings( FieldTable ) ->

	[ field_to_string( FieldName, FieldType, DefaultValue )
	  || { FieldName, { FieldType, DefaultValue, _FirstLine, _SecondLine } }
			 <- FieldTable ].



% Returns a textual representation of specified record field.
%
% (helper)
%
field_to_string( FieldName, FieldType, DefaultValue ) ->

	TypeString = case FieldType of

		undefined ->
			"no type";

		_ ->
			text_utils:format( "following type '~p'", [ FieldType ] )

	end,

	DefaultValueString = case DefaultValue of

		undefined ->
			"no default value";

		_ ->
			text_utils:format( "following default value '~p'",
							   [ DefaultValue ] )

	end,

	text_utils:format( "field '~s' with ~s and ~s defined",
					   [ FieldName, TypeString, DefaultValueString ] ).




% Returns a textual description of the specified function information.
%
-spec function_info_to_string( function_info() ) -> text_utils:ustring().
function_info_to_string( #function_info{ name=Name,
										 arity=Arity,
										 location=_Location,
										 line=Line,
										 clauses=Clauses,
										 spec=LocatedSpec,
										 callback=IsCallback,
										 exported=Exported } ) ->

	ExportString = case Exported of

		undefined ->
			"local";

		ExportLoc ->
			text_utils:format( "exported in ~s",
				  [ id_utils:sortable_id_to_string( ExportLoc ) ] )

	end,

	DefString = text_utils:format( "defined from line #~B, "
			   "with ~B clause(s) defined", [ Line, length( Clauses ) ] ),

	SpecString = case LocatedSpec of

		undefined ->
			"no type specification";

		_ ->
			case IsCallback of

				true ->
					"a callback type specification";

				false ->
					"a (standard) type specification"

			end

	end,

	text_utils:format( "~s/~B, ~s, ~s and ~s",
				   [ Name, Arity, ExportString, DefString, SpecString ] ).



% Ensures that specified type is exported at the specified location(s).
%
-spec ensure_type_exported( type_id(), [ location() ], module_info(),
							type_export_table() ) -> type_export_table().
ensure_type_exported( _TypeId, _ExportLocs=[], _ModuleInfo, ExportTable ) ->
	ExportTable;

ensure_type_exported( TypeId, _ExportLocs=[ _Loc=auto_located | T ],
					  ModuleInfo, ExportTable ) ->

	% When a type export is to be auto-located, we attach it just after the
	% module definition, to avoid possibly placing an export after a type
	% definition:

	ModuleLoc = case ModuleInfo#module_info.module_def of

		undefined ->
			throw( { auto_locate_whereas_no_module_def, TypeId } );

		{ MLoc, _Form } ->
			MLoc

	end,

	TypeExportLoc = id_utils:get_higher_next_depth_sortable_id( ModuleLoc ),

	% This location may have already been used, thus:
	case ?table:lookupEntry( TypeExportLoc, ExportTable ) of

		{ value, { Line, TypeIds } } ->

			case lists:member( TypeId, TypeIds ) of

				true ->
					% Already registered, perfect as is, continues with the next
					% locations:
					ensure_type_exported( TypeId, T, ModuleInfo, ExportTable );

				false ->
					% Adding it then:
					NewEntry = { Line, [ TypeId | TypeIds ] },
					NewExportTable = ?table:addEntry( TypeExportLoc, NewEntry,
													  ExportTable ),
					ensure_type_exported( TypeId, T, ModuleInfo,
										  NewExportTable )

			end;


		key_not_found ->
			% We create a new location entry then:
			NewEntry = { _DefaultLine=0, [ TypeId ] },
			NewExportTable = ?table:addEntry( TypeExportLoc, NewEntry,
											  ExportTable ),
			ensure_type_exported( TypeId, T, ModuleInfo,
								  NewExportTable )

	end;


ensure_type_exported( TypeId, _ExportLocs=[ Loc | T ], ModuleInfo,
					  ExportTable ) ->

	% Here we have an immediate location:
	case ?table:lookupEntry( Loc, ExportTable ) of

		{ value, { Line, TypeIds } } ->

			case lists:member( TypeId, TypeIds ) of

				true ->
					% Already registered, perfect as is, continues with the next
					% locations:
					ensure_type_exported( TypeId, T, ModuleInfo, ExportTable );

				false ->
					% Adding it then:
					NewEntry = { Line, [ TypeId | TypeIds ] },
					NewExportTable = ?table:addEntry( Loc, NewEntry,
													  ExportTable ),
					ensure_type_exported( TypeId, T, ModuleInfo,
										  NewExportTable )

			end;


		key_not_found ->
			% Not even a registered location:
			throw( { invalid_export_location, Loc, TypeId } )

	end.



% Ensures that specified type is not exported at the specified location(s).
%
-spec ensure_type_not_exported( type_id(), [ location() ],
								type_export_table() ) -> type_export_table().
ensure_type_not_exported( _TypeId, _ExportLocs=[], ExportTable ) ->
	ExportTable;

ensure_type_not_exported( TypeId, _ExportLocs=[ Loc | T ], ExportTable ) ->

	case ?table:lookupEntry( Loc, ExportTable ) of

		{ value, { Line, TypeIds } } ->

			% 0 or 1 reference expected, which is handled the same by:
			NewExportTable = case lists:delete( TypeId, TypeIds ) of

				[] ->
					?table:removeEntry( Loc, ExportTable );

				ShrunkTypeIds ->
					?table:addEntry( Loc, { Line, ShrunkTypeIds },
									 ExportTable )

			end,

			ensure_type_not_exported( TypeId, T, NewExportTable );

		key_not_found ->
			throw( { inconsistent_export_location, Loc, TypeId } )

	end.



% Returns a textual description of the specified type information.
%
-spec type_info_to_string( type_info() ) -> text_utils:ustring().
type_info_to_string( #type_info{ name=Name,
								 variables=TypeVariables,
								 opaque=IsOpaque,
								 location=_Location,
								 line=_Line,
								 definition=Definition,
								 exported=Exported } ) ->

	ExportString = case Exported of

		[] ->
			"local";

		ExportLoc ->
			text_utils:format( "exported in ~s",
							   [ id_utils:sortable_id_to_string( ExportLoc ) ] )

	end,

	% In theory, we expect opaque types to be exported ones, yet we prefer to
	% handle all possible cases:
	%
	OpaqueString = case IsOpaque of

		true ->
			"opaque";

		false ->
			"non-opaque"

	end,

	DefString = text_utils:format( "defined by: ~p", [ Definition ] ),

	Arity = length( TypeVariables ),

	text_utils:format( "~s/~B, ~s, ~s and ~s",
					   [ Name, Arity, OpaqueString, ExportString, DefString ] ).
