% Copyright (C) 2018-2025 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Les605
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
% Creation date: Sunday, January 21, 2018.

-module(ast_scan).

-moduledoc """
Module in charge of **scanning an AST**, a prerequisite notably for later
transformations (see ast_transform.erl for that).
""".


% For the table macro for example:
-include("meta_utils.hrl").


% For the module_info record:
-include("ast_info.hrl").



-doc """
Name of any parse attribute.

(typically in Form={attribute, FileLoc, AttributeName, AttributeValue})
""".
-type parse_attribute_name() :: atom().



-doc "A context of a scan.".
-type scan_context() :: { ast_base:file_reference(), ast_base:line() }.



-doc "An error report.".
-type error_report() :: ustring().


-export_type([ parse_attribute_name/0, scan_context/0, error_report/0 ]).


-export([ scan/1, check_parse_attribute_name/1, check_parse_attribute_name/2 ]).




% Type shorthands:

-type void() :: type_utils:void().
-type error_reason() :: basic_utils:error_reason().

-type ast() :: ast_base:ast().
-type form_context() :: ast_base:form_context().

-type module_info() :: ast_info:module_info().
-type compile_option_table() :: ast_info:compile_option_table().
-type located_form() :: ast_info:located_form().
-type marker_table() :: ast_info:section_marker_table().
-type ast_location() :: ast_info:ast_location().

-type ustring() :: text_utils:ustring().


% Implementation notes:
%
% The integral scanning of ASTs got inspiration from erl_id_trans.erl.
%
% Numbered sections refer to the ones in
% http://erlang.org/doc/apps/erts/absform.html, with additional subsections for
% each bullet.
%
% Note: we used to traverse the AST recursively, "blindly", i.e. without
% expecting the intended structure of AST elements (e.g. the structure of a
% tuple corresponding to a 'receive' statement).
%
% Then, for a more complete control, we matched the AST against its intended,
% "official" structure, as defined in
% http://erlang.org/doc/apps/erts/absform.html; however, for a mere scan, this
% is mostly useless (as a more basic traversal is sufficient), and moreover
% counter-productive as we might want to deviate from the base, Erlang rules in
% the *input* AST (but of course the output one shall be legit).
%
% Now we thus perform a minimal traversal, i.e. one that is just deep enough in
% order that we can fill appropriately our module_info record.
%
% Moreover, as this scanning is meant to operate on an input AST (from the point
% of view of Ceylan-Myriad), we adhere to the Ceylan-Myriad conventions rather
% than to the vanilla Erlang ones (that apply to the final, output AST).
%
% Deep, strict traversal is of use mainly for transformation (see
% ast_transform.erl) rather than for scanning.
%
% Ultimately, resulting forms are to be validated anyway by the compiler (most
% precise and efficient Erlang-level checking).
%
% We tried to order scan clauses roughly in decreasing average frequency of
% appearance in the actual code to transform, yet often the order of clauses
% matters (as it has a meaning and an impact).



% About the orOrder of the forms in the AST.
%
% A parse transform receives an (ordered) list of forms as input AST: forms are
% ordered only by their position in that list (file locations, i.e. line/column
% numbers, are contextual to the last file declared as included, hence form
% order matters, and a form cannot be ordered by itself), stored in a
% module_info record.
%
% Knowing that forms may have to be added, moved, updated, removed, etc. during
% the transformation of an AST, positioning forms at specified points in an AST
% stream is not straightforward (e.g. the mere index of a form in the AST is not
% stable, as any addition before this form would require this index to be
% updated); moreover being able to keep track of their original order (to which
% the parse transform tries to stick as much as possible) is certainly a safe
% measure for debuggability.
%
% So, instead of storing forms in an AST, we store located forms in a located
% AST: instead of having [F1, F2, ..., Fn], we may have for example [{Loc2,F2},
% {Loc1,F1}, ..., {Locn,Fn} ], where Loc are (stable, immutable) locations,
% i.e. form_location() - which are themselves id_utils:sortable_id().
%
% It allows to store, in a located AST, (located) forms in an arbitrary order:
% sorting them according to their location reorders them as intended (note that
% the AST scan results in an in-order located AST, which is more convenient for
% insertions).
%
% Special AST pseudo-locations have been defined, including 'auto_locate_after',
% which is, when iterating through a located AST, to be replaced by an actual
% location just after the current one (relative positioning, based on the
% current order of the stream).
%
% We also found it useful to define section markers (see
% ast_info:section_marker/0), which are standard reference points within an AST,
% to offer absolute positioning.
%
% Scanning an AST tries to automatically set these section markers, based on the
% content found.
%
% For example, one may want a given form to be placed at the beginning of the
% section for function exports; then the location of this form shall be
% specified as being after the corresponding marker, i.e. after the location
% stored in marker 'export_functions_marker'.
%
% However, depending on the AST content, markers may not be set, or may even not
% be sorted according to our canonical order (e.g. 'export_functions_marker' may
% be set before 'export_types_marker', it is still legit compilation-wise).


-doc """
Scans the specified AST, expected to correspond to a module definition, and
returns the corresponding module_info record.
""".
-spec scan( ast() ) -> module_info().
scan( AST ) ->

	%ast_utils:display_trace( "scanning AST" ),

	InitModuleInfo = ast_info:init_module_info(),

	FirstASTLoc = id_utils:get_initial_sortable_id(),

	FirstMarkerTable = ?table:add_new_entry( begin_marker, FirstASTLoc,
		InitModuleInfo#module_info.markers ),

	FirstModuleInfo = InitModuleInfo#module_info{ markers=FirstMarkerTable },

	%ast_utils:display_trace( "beginning scan" ),

	% Application of 7.1.1:
	%
	% "If D is a module declaration consisting of the forms F_1, ..., F_k, then
	% Rep(D) = [Rep(F_1), ..., Rep(F_k)]."
	%
	% So iterating in the list of forms that make for the AST:
	%
	case scan_forms( AST, FirstModuleInfo, _NextASTLoc=FirstASTLoc,
					 _CurrentFileRef=undefined ) of

		ModInfo=#module_info{ errors=[] } ->
			%ast_utils:display_trace( "no error to report" ),
			% No error, let's continue:
			ModInfo;

		_ModInfo=#module_info{ errors=Errors } ->
			ReorderedErrors = lists:reverse( Errors ),
			%ast_utils:display_debug( "reporting errors: ~p",
			%                         [ ReorderedErrors ] ),
			[ report_error( E ) || E <- ReorderedErrors ],

			%exit( Errors )
			%exit( errors_reported )
			%exit( "fix your code :)" )
			%io:format( "fix your code :)" ),

			% We added back a call to halt/1, as otherwise, with just 'exit(
			% "fix your code :)" )', the following undesirable stacktrace is
			% returned:
			%
			% """
			% *** Internal compiler error ***
			% exception exit: "fix your code :)"
			% in function  ast_scan:scan/1 (ast_scan.erl, line 206)
			% in call from ast_info:extract_module_info_from_ast/1
			% [...]

			% So now:
			erlang:halt( 20 )

			% If wanting instead to ignore errors, at least at this step, no
			% halting could be done, and this module info be returned; however
			% bogus error are bound to be reported afterwards (e.g. injected
			% functions with no clause), so not halting the compilation would
			% have little interest:
			%
			%ModInfo

	end.



-doc """
Reports the specified error, using the same format as erlc, so that tools can
parse these errors as well.

For example: `foo.erl:102: can't find include file "bar.hrl"`.
""".
-spec report_error( { scan_context(), error_reason() } ) -> void().
report_error( { Context, Error } ) ->

	% No trace_utils yet here:
	%io:format( "Reporting error ~p in context ~p.~n", [ Error, Context ] ),

	ErrorString = case Error of

		% For example Msg="head mismatch"
		{ parse_error, Msg } when is_list( Msg ) ->
			%text_utils:format( "parse error: ~ts", [ Msg ] );
			text_utils:format( "~ts", [ Msg ] );

		{ undefined_macro_variable, VariableName }
										when is_atom( VariableName ) ->
			text_utils:format( "undefined macro variable '~ts'",
							   [ VariableName ] );

		% For example a wrong use of a record, such as R#my_rec instead of
		% R=#myrec:
		%
		{ unexpected_pattern, Pattern } ->
			text_utils:format( "unexpected pattern: ~p", [ Pattern ] );

		{ scan_error, _ScanError={ string, _L, Text } } when is_list( Text ) ->
			text_utils:format( "syntax error near \"~ts\"", [ Text ] );

		{ scan_error, ScanError } ->
			text_utils:format( "scan error: ~p", [ ScanError ] );

		{ epp_error, { undefined, MacroName, Arity } }
				when is_atom( MacroName ) andalso is_integer( Arity ) ->
			text_utils:format( "undefined macro ~ts/~B", [ MacroName, Arity ] );

		% For example DirectiveName=ifdef
		{ epp_error, { bad, DirectiveName } } when is_atom( DirectiveName )  ->
			text_utils:format( "invalid '~ts' preprocessor directive",
							   [ DirectiveName ] );

		% For example Problem="unbalanced"
		{ epp_error, { illegal, Problem, DirectiveName } }
				when is_atom( DirectiveName )  ->
			text_utils:format( "illegal ~ts '~ts' preprocessor directive",
							   [ Problem, DirectiveName ] );

		% For example MacroName=debug
		{ epp_error, { mismatch, MacroName } } ->
			text_utils:format( "mismatch regarding macro '~ts' (wrong arity?)",
							   [ MacroName ] );

		% For example MacroName=send_info_fmt
		{ epp_error, { arg_error, MacroName } } ->
			text_utils:format( "argument error in macro '~ts' "
				"(e.g. non-closed parenthesis?)", [ MacroName ] );

		% For example HeaderPath="myriad/include/ast_info.hrl"
		{ epp_error, { include, lib, HeaderPath } } ->
			text_utils:format( "could not find include_lib header file '~ts'",
							   [ HeaderPath ] );

		{ epp_error, { call, [ Char, Str ] } } ->
			text_utils:format( "preprocessor error for character '~c' "
				"regarding '~ts'", [ Char, Str ] );

		{ epp_error, { redefine, DefineStr } } ->
			text_utils:format(
				"the preprocessor symbol '~ts' is already defined",
				[ DefineStr ] );

		% For example 'multiple definitions for ...', emitted by ast_scan:
		String when is_list( String ) ->
			%text_utils:format( "~ts [raw error string reported]", [ String ] );
			text_utils:format( "~ts", [ String ] );

		Other ->
			text_utils:format( "~p (raw error reported)", [ Other ] )
			%text_utils:format( "~p", [ Other ] )

	end,

	%trace_utils:debug_fmt( "Full error was: ~p", [ Error ] ),

	io:format( "~ts: ~ts~n", [ context_to_string( Context ), ErrorString ] ).



-doc "Returns a textual representation of the specified compilation context.".
-spec context_to_string( scan_context() ) -> ustring().
context_to_string( { Filename, FileLoc } ) ->

	% Respects the standard formatting:
	%
	% (note: using file_utils:normalise_path/1 would suppose file_utils is
	% already built...)
	%
	text_utils:format( "~ts:~ts",
					   [ Filename, ast_utils:format_file_loc( FileLoc ) ] ).



%%
%% First part: top-level forms.
%%



-doc """
Main scanning function.

Here all relevant parts of the specified AST (located forms) are matched in
turn, and stored in the specified module_info once located using
ast_base:form_location/0 identifiers, which allow easy insertions and
reordering.
""".
-spec scan_forms( ast(), module_info(), ast_base:form_location(),
				  ast_base:file_reference() ) -> module_info().


% Overall structure (based on http://erlang.org/doc/apps/erts/absform.html):
%
% Section 7.1: Module Declarations and Forms:
%  - (7.1.1: Module declaration, already done)
%  - 7.1.2:  Function export
%  - 7.1.3:  Function import
%  - 7.1.4:  Module
%  - 7.1.5:  File reference (include)
%  - 7.1.6:  Function definition
%  - 7.1.7:  Local function type specification
%  - 7.1.8:  Remote function type specification
%  - 7.1.9:  Record definition
%  - 7.1.10: Type definition
%  - 7.1.11: Other, "wild" parse attributes.
%  - 7.1.12: Type export [lacking in reference page]
%  - 7.1.13: Compile export [lacking in reference page]
%  - 7.1.14 : Parse errors
%  - 7.1.15 : End of file

% Extra Section 7.9: Catch-all for unexpected forms



%%
%% Section 7.1: Module Declarations and Forms.
%%



% (7.1.1: Module declaration, already done)

% 7.1.2: Function export handling.
%
% "If F is an attribute -export([Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) =
% {attribute, FILE_LOC, export, [{Fun_1,A_1}, ..., {Fun_k,A_k}]}."
%
scan_forms( _AST=[ _Form={ 'attribute', FileLoc, 'export', FunctionIds } | T ],
			M=#module_info{ function_exports=ExportTable,
							functions=FunctionTable,
							markers=MarkerTable },
			NextASTLoc, CurrentFileReference ) ->

	%ast_utils:display_debug( "function export declaration for ~p",
	% [ FunctionIds ] ),

	Context = { CurrentFileReference, FileLoc },

	% Records this export regarding each of these functions:
	NewFunctionTable = lists:foldl(

		fun( FunId, FunTableAcc ) ->

			{ Name, Arity } = ast_function:check_function_id( FunId, Context ),

			NewFunInfo = case ?table:lookup_entry( FunId, FunTableAcc ) of

				key_not_found ->

					% New entry then:
					#function_info{ name=Name,
									arity=Arity,
									% Implicit:
									%ast_location=undefined
									%file_location=undefined
									%clauses=[],
									%spec=undefined,
									%callback=undefined,
									exported=[ NextASTLoc ] };

				% A function *might* be exported more than once:
				{ value, FunInfo } -> % F=#function_info{ exported=[] } } ->
					% Just add the fact that the function is exported then:
					NewExp = [ NextASTLoc | FunInfo#function_info.exported ],
					FunInfo#function_info{ exported=NewExp }

			end,

			?table:add_entry( FunId, NewFunInfo, FunTableAcc )

		end,
		_Acc0=FunctionTable,
		_List=FunctionIds ),

	% At least initially, exactly one export entry per location:
	NewExportTable = ?table:add_new_entry( NextASTLoc, { FileLoc, FunctionIds },
										   ExportTable ),

	NewMarkerTable = case ?table:has_entry( export_functions_marker,
											MarkerTable ) of

		true ->
			% Already found, nothing to do.
			MarkerTable;

		false ->
			?table:add_entry( export_functions_marker, NextASTLoc,
							  MarkerTable )

	end,


	% And then recurses with the next forms:
	scan_forms( T, M#module_info{ function_exports=NewExportTable,
								  functions=NewFunctionTable,
								  markers=NewMarkerTable },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );



% 7.1.3: Function import handling.
%
% "If F is an attribute -import(Mod,[Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) =
% {attribute, FILE_LOC, import, {Mod, [{Fun_1,A_1}, ..., {Fun_k,A_k}]}}."
%
scan_forms( _AST=[ Form={ 'attribute', FileLoc, 'import',
							{ ModuleName, FunIds } } | T ],
			M=#module_info{ function_imports=ImportTable,
							function_imports_defs=ImportDefs,
							markers=MarkerTable },
			NextASTLoc, CurrentFileReference ) ->

	Context = { CurrentFileReference, FileLoc },

	ast_utils:check_module_name( ModuleName, Context ),
	ast_function:check_function_ids( FunIds, Context ),

	NewImportTable = ?table:append_list_to_entry( ModuleName, FunIds,
												  ImportTable ),

	NewImportDefs = [ { NextASTLoc, Form } | ImportDefs ],

	NewMarkerTable = case ?table:has_entry( import_functions_marker,
											MarkerTable ) of

		true ->
			% Already found, nothing to do.
			MarkerTable;

		false ->
			?table:add_entry( import_functions_marker, NextASTLoc,
							  MarkerTable )

	end,

	scan_forms( T, M#module_info{ function_imports=NewImportTable,
								  function_imports_defs=NewImportDefs,
								  markers=NewMarkerTable },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );



% 7.1.4: Module handling.
%
% "If F is an attribute -module(Mod), then Rep(F) = {attribute, FILE_LOC,
% module, Mod}."
%
scan_forms( _AST=[ Form={ 'attribute', FileLoc, 'module', ModuleName } | T ],
			M=#module_info{ module=undefined,
							markers=MarkerTable },
			NextASTLoc, CurrentFileReference ) ->

	%ast_utils:display_debug( "module declaration for ~ts", [ ModuleName ] ),

	Context = { CurrentFileReference, FileLoc },

	ast_utils:check_module_name( ModuleName, Context ),

	% Note: when processing X.beam, we should not remove the lines like:
	% {attribute,37,file,{"X.erl",37}} as they allow to report errors
	% appropriately.

	LocForm = { NextASTLoc, Form },

	NewMarkerTable =
		?table:add_new_entry( module_marker, NextASTLoc, MarkerTable ),

	scan_forms( T, M#module_info{ module={ ModuleName, LocForm },
								  markers=NewMarkerTable },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );


% Any lacking, invalid or duplicated module declaration will be caught by
% the compiler anyway.
%
scan_forms( _AST=[ _Form={ 'attribute', FileLoc, 'module', ModuleName } | _T ],
			#module_info{ module={ PreviousModuleName, _PreviousLocDef } },
			_NextASTLoc, CurrentFileReference ) ->
	throw( { multiple_module_definitions, PreviousModuleName, ModuleName,
				{ CurrentFileReference, FileLoc } } );



% 7.1.5: File reference (include) handling.
%
% "If F is an attribute -file(File,FileLoc), then Rep(F) = {attribute, FILE_LOC,
% file, {File,InFileLoc}}."
%
% Allows to keep track of when an included file begins and also ends, i.e. to
% determine the current file to which any new line number corresponds.
%
scan_forms( _AST=[ Form={ 'attribute', _FileLoc, 'file',
							{ FilePath, _InFileLoc } } | T ],
			M=#module_info{ includes=Inc, include_defs=IncDefs }, NextASTLoc,
			_CurrentFileReference ) ->

	%ast_utils:display_debug( "file declaration with ~ts at ~ts",
	% [ FilePath, ast_utils:file_loc_to_string( InFileLoc ) ] ),

	% We used to normalise paths, however then 'file_utils' would have to be
	% bootstrapped as well, which does not seem desirable.

	%NewCurrentFileReference = text_utils:string_to_binary(
	%   file_utils:normalise_path( Filepath ) ),
	NewCurrentFileReference = text_utils:string_to_binary( FilePath ),

	% Avoids duplicates (in 'includes' only, not in definitions):
	%
	NewFilePaths = case lists:member( NewCurrentFileReference, Inc ) of

		true ->
			Inc;

		false ->
			[ NewCurrentFileReference | Inc ]

	end,

	LocForm = { NextASTLoc, Form },

	scan_forms( T, M#module_info{ includes=NewFilePaths,
								  include_defs=[ LocForm | IncDefs ] },
		id_utils:get_next_sortable_id( NextASTLoc ), NewCurrentFileReference );




% 7.1.6: Function definition handling.
%
% "If F is a function declaration Name Fc_1 ; ... ; Name Fc_k, where each Fc_i
% is a function clause with a pattern sequence of the same length Arity, then
% Rep(F) = {function, FILE_LOC, Name, Arity, [Rep(Fc_1), ...,Rep(Fc_k)]}."
%
scan_forms(
  [ _Form={ 'function', FileLoc, FunctionName, FunctionArity, Clauses } | T ],
  M=#module_info{ functions=FunctionTable,
				  markers=MarkerTable,
				  errors=Errors },
				  %unhandled_forms=UnhandledForms },
  NextASTLoc, CurrentFileReference ) ->

	%ast_utils:display_debug( "function definition for ~p/~p",
	%   [ FunctionName, FunctionArity ] ),

	% The non-first clauses could be checked as well:
	%
	% (when adding a function, we may not check if ever there was a pre-existing
	% one - multiple definitions will be rejected by the compiler anyway)

	Context = { CurrentFileReference, FileLoc },

	ast_function:check_function_name( FunctionName, Context ),
	ast_utils:check_arity( FunctionArity, Context ),
	ast_clause:check_function_clauses( Clauses, FunctionArity, Context ),

	FunId = { FunctionName, FunctionArity },

	{ FunInfo, MaybeError } =
			case ?table:lookup_entry( FunId, FunctionTable ) of

		key_not_found ->
			% New entry then:
			Finfo = #function_info{ name=FunctionName,
									arity=FunctionArity,
									ast_location=NextASTLoc,
									file_location=FileLoc,
									clauses=Clauses
									% Implicit:
									%spec=undefined,
									%callback=undefined,
									%exported=[]
								  },
			{ Finfo, undefined };


		{ value, F=#function_info{ clauses=[] } } ->
			% Already here because of an export; just add the missing
			% information then:
			%
			Finfo = F#function_info{ ast_location=NextASTLoc,
									 file_location=FileLoc,
									 clauses=Clauses },
			{ Finfo, undefined };


		% Here a definition was already set: (previous line not necessarily in
		% the context of the same file)

		% We tried to silence that error (let it go through) in order to rely on
		% the compiler, yet commenting-out this led to having a double
		% definition not properly handled:
		%
		% Would result in ignoring this definition, hence compiling incorrectly:
		%
		%{ value, F } ->
		%   { F, ok }

		% We could crash directly, yet the error message would not be nicely
		% integrated in an IDE (as not standard):

		%{ value, #function_info{ file_location=PreviousFileLoc } } ->
		%   ast_utils:raise_error( [ multiple_definitions_for, FunId,
		%     { previous_line, PreviousFileLoc } ],
		%     Context )

		% A better approach could have been to store a list of such list of
		% clauses, and to reinject them as they are (as unhandled forms), so
		% that the compiler sees them and complains in a standard manner
		% afterwards; but then it triggers a warning of ours (that we want to
		% keep) about unhandled forms, and the standard message is less
		% informative than ours (only "FILE:FILE_LOC: function foo/1 already
		% defined"), so we finally prefer ours:

		{ value, F=#function_info{ name=FunctionName,
								   arity=FunctionArity,
								   file_location=SomeFileLoc } } ->

			% Note that the past error was not necessarily in the same file:
			%
			% (this information could be determined thanks to the located forms,
			% yet you require quite a lot of development)

			Report = text_utils:format( "multiple definitions for ~ts/~B "
				"(past definition at ~ts)", [ FunctionName, FunctionArity,
					ast_utils:file_loc_to_string( SomeFileLoc ) ] ),

			Error = { Context, Report },
			%UnhandledLocForm = { NextASTLoc, Form },

			{ F, Error }
			% { F, UnhandledLocForm }



	end,

	NewFunctionTable = ?table:add_entry( _K=FunId, _V=FunInfo, FunctionTable ),

	NewMarkerTable = case ?table:has_entry( definition_functions_marker,
											MarkerTable ) of

		true ->
			% Already found, nothing to do.
			MarkerTable;

		false ->
			?table:add_entry( definition_functions_marker, NextASTLoc,
							  MarkerTable )

	end,

	NewErrors = case MaybeError of

		undefined ->
			Errors;

		_ ->
			[ MaybeError | Errors ]

	end,

	%ast_utils:display_debug( "function ~ts/~B with ~B clauses registered.",
	%   [ FunctionName, FunctionArity, length( Clauses ) ] ),

	scan_forms( T, M#module_info{ functions=NewFunctionTable,
								  markers=NewMarkerTable,
								  errors=NewErrors },
								  %unhandled_forms=NewUnhandledForms },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );



% 7.1.7: Local function type specification handling (including callbacks).
%
% "If F is a function specification -Spec Name Ft_1; ...; Ft_k, where Spec is
% either the atom spec or the atom callback, and each Ft_i is a possibly
% constrained function type with an argument sequence of the same length Arity,
% then Rep(F) = {attribute,FileLoc,Spec,{{Name,Arity},[Rep(Ft_1), ...,
% Rep(Ft_k)]}}."
%
scan_forms( [ Form={ 'attribute', FileLoc, SpecType,
						{ FunId, FunctionTypes } } | T ],
			M=#module_info{ functions=FunctionTable },
			NextASTLoc, CurrentFileReference )
					when SpecType =:= 'spec' orelse SpecType =:= 'callback' ->

	Context = { CurrentFileReference, FileLoc },

	{ FunctionName, FunctionArity } =
		ast_function:check_function_id( FunId, Context ),

	ast_function:check_function_types( FunctionTypes, FunctionArity, Context ),

	%ast_utils:display_debug( "~ts definition for ~p/~p",
	% [ SpecType, FunctionName, FunctionArity ] ),

	LocatedSpec = { NextASTLoc, Form },

	IsCallback = case SpecType of

		callback ->
			true;

		spec ->
			false

	end,

	FunInfo = case ?table:lookup_entry( FunId, FunctionTable ) of

		key_not_found ->

			% New entry then:
			#function_info{ name=FunctionName,
							arity=FunctionArity,
							% Implicit:
							%ast_location=undefined,
							%file_location=undefined,
							%clauses=[]
							spec=LocatedSpec,
							callback=IsCallback };

		{ value, F=#function_info{ spec=undefined } } ->
			% Just add that spec form and callback status then:
			F#function_info{ spec=LocatedSpec, callback=IsCallback };

		% Here a spec was already set:
		%_ ->
		%   ast_utils:raise_error( [ multiple_specs_for, FunId ], Context )

		% Finally we prefer letting the compiler complain by itself about these
		% multiple specs:
		%
		{ value, F } ->
			F

	end,

	NewFunctionTable = ?table:add_entry( _K=FunId, _V=FunInfo, FunctionTable ),

	%ast_utils:display_debug( "spec for function ~ts/~B registered.",
	%   [ FunctionName, FunctionArity ] ),

	scan_forms( T, M#module_info{ functions=NewFunctionTable },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );


% (optional callbacks, not specified in the spec yet known of the id parse
% transform)
%
scan_forms( [ Form={ 'attribute', FileLoc, _AttributeName='optional_callbacks',
					 _AttributeValue=FunIds } | T ],
			M=#module_info{ optional_callbacks_defs=LocatedDefs },
			NextASTLoc, CurrentFileReference ) ->

	Context = { CurrentFileReference, FileLoc },

	% Surprisingly, in erl_id_trans.erl, the corresponding check may fail (as is
	% in a try/catch clause), and in this case is replaced by its original
	% value.
	%
	ast_function:check_function_ids( FunIds, Context ),

	LocForm = { NextASTLoc, Form },

	scan_forms( T, M#module_info{
					optional_callbacks_defs=[ LocForm | LocatedDefs ] },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );


% (asm attribute, not specified in the spec yet known of the id parse transform;
% checked and then treated as any wild parse attribute)
%
scan_forms( [ Form={ 'attribute', _FileLoc, AttributeName='asm',
					 Def={ 'function', _N, _A, _Code } } | T ],
			  M=#module_info{ parse_attributes=ParseAttributeTable },
			NextASTLoc, CurrentFileReference ) ->

	%ast_utils:display_debug( "Asm attribute definition: '~p'.", [ Def ] ),

	LocForm = { NextASTLoc, Form },

	% Expected once:
	scan_forms( T, M#module_info{ parse_attributes=?table:append_to_entry(
			AttributeName, { _AttributeValue=Def, LocForm },
			ParseAttributeTable ) },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );



% 7.1.8: Remote function type specification handling.
%
% "If F is a function specification -spec Mod:Name Ft_1; ...; Ft_k, where each
% Ft_i is a possibly constrained function type with an argument sequence of the
% same length Arity, then Rep(F) =
% {attribute,FileLoc,spec,{{Mod,Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}."
%
scan_forms( [ Form={ 'attribute', FileLoc, 'spec',
					 _MFA={ ModuleName, FunctionName, FunctionArity },
					 FunctionTypes } | T ],
			M=#module_info{ remote_spec_defs=RemoteSpecDefs },
			NextASTLoc, CurrentFileReference ) ->

	Context = { CurrentFileReference, FileLoc },

	FunId = { FunctionName, FunctionArity },

	ast_utils:check_module_name( ModuleName, Context ),
	ast_function:check_function_id( FunId, Context ),
	ast_function:check_function_types( FunctionTypes, FunctionArity, Context ),

	%ast_utils:display_debug( "remote spec definition for ~p/~p",
	%   [ FunctionName, FunctionArity ] ),

	% Specs for remote functions not specifically processed.

	LocatedSpec = { NextASTLoc, Form },

	NewRemoteSpecDefs = [ LocatedSpec | RemoteSpecDefs ],

	%ast_utils:display_debug( "remote spec for function ~ts/~B registered.",
	%   [ FunctionName, FunctionArity ] ),

	scan_forms( T, M#module_info{ remote_spec_defs=NewRemoteSpecDefs },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );



% 7.1.9: Record definition handling.
%
% "If F is a record declaration -record(Name,{V_1, ..., V_k}), where each V_i is
% a record field, then Rep(F) = {attribute, FILE_LOC, record, {Name,[Rep(V_1),
% ..., Rep(V_k)]}}. For Rep(V), see below.
%
scan_forms( _AST=[ _Form={ 'attribute', FileLoc, 'record',
							{ RecordName, DescFields } } | T ],
			M=#module_info{ records=RecordTable, markers=MarkerTable },
			NextASTLoc, CurrentFileReference ) ->

	Context = { CurrentFileReference, FileLoc },

	ast_type:check_record_name( RecordName, Context ),

	% Finally we let the compiler complain:
	%?table:has_entry( RecordName, RecordTable ) orelse
	%   ast_utils:raise_error(
	%       [ multiple_definitions_for_record, RecordName ], Context ),

	FieldTable = scan_field_descriptions( DescFields, CurrentFileReference ),

	NewRecordDef = { FieldTable, NextASTLoc, FileLoc },

	%ast_utils:display_debug( "Adding for record '~p' following "
	%                         "definition:~n~p", [ RecordName, NewRecordDef ] ),

	% New entry by design:
	NewRecordTable = ?table:add_entry( RecordName, NewRecordDef, RecordTable ),

	NewMarkerTable = case ?table:has_entry( definition_records_marker,
											MarkerTable ) of

		true ->
			% Already found, nothing to do.
			MarkerTable;

		false ->
			?table:add_entry( definition_records_marker, NextASTLoc,
							  MarkerTable )

	end,

	scan_forms( T, M#module_info{ records=NewRecordTable,
								  markers=NewMarkerTable },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );



% 7.1.10: Type definition handling.
%
% "If F is a type declaration -Type Name(V_1, ..., V_k) :: T, where Type is
% either the atom type or the atom opaque, each V_i is a variable, and T is a
% type, then Rep(F) = {attribute, FILE_LOC, Type, {Name, Rep(T), [Rep(V_1), ...,
% Rep(V_k)]}}."
%
scan_forms( _AST=[ _Form={ 'attribute', FileLoc, TypeDesignator,
							{ TypeName, TypeDef, TypeVariables } } | T ],
			M=#module_info{ types=TypeTable, markers=MarkerTable },
			NextASTLoc, CurrentFileReference )
		when TypeDesignator =:= 'type' orelse TypeDesignator =:= 'opaque' ->

	%ast_utils:display_debug( "type declaration for ~p: ~p", [
	%                         TypeName, Form ] ),

	Context = { CurrentFileReference, FileLoc },

	ast_type:check_type_name( TypeName, Context ),
	ast_type:check_type_definition( TypeDef, Context ),
	ast_type:check_type_variables( TypeVariables, Context ),

	IsOpaque = not ( TypeDesignator =:= type ),

	TypeArity = length( TypeVariables ),

	TypeId = { TypeName, TypeArity },

	NewTypeInfo = case ?table:lookup_entry( TypeId, TypeTable ) of

		% If a TypeInfo is found for that type name, it must be only because it
		% has already been exported (not expected to be already defined):

		{ value, #type_info{ file_location=DefFileLoc, exported=[] } } ->

			% We have to report this multiple definition, otherwise it will not
			% be seen by the compiler afterwards (the extra type definition
			% would be skipped before reaching the compiler)

			% "some line" as we do not know here to which source files this
			% corresponds:
			%
			ast_utils:raise_usage_error( "type ~ts/~B defined here, whereas "
				"it had already been defined at (some) ~ts.",
				[ TypeName, TypeArity,
				  ast_utils:file_loc_to_string( DefFileLoc ) ],
				CurrentFileReference, FileLoc );


		{ value, ExportTypeInfo } ->
			ExportTypeInfo#type_info{ name=TypeName,
									  variables=TypeVariables,
									  opaque=IsOpaque,
									  ast_location=NextASTLoc,
									  file_location=FileLoc,
									  definition=TypeDef
									  %exported: already set
											   };


		% Usual case:
		key_not_found ->
			%ast_utils:display_debug( "New type '~ts' defined (at line #~p) "
			%     "as:~n~p", [ TypeName, FileLoc, TypeDef ] ),
			#type_info{ name=TypeName,
						variables=TypeVariables,
						opaque=IsOpaque,
						ast_location=NextASTLoc,
						file_location=FileLoc,
						definition=TypeDef
						%exported=[]
					  }

	end,

	NewTypeTable = ?table:add_entry( TypeId, NewTypeInfo, TypeTable ),

	NewMarkerTable = case ?table:has_entry( definition_types_marker,
											MarkerTable ) of

		true ->
			% Already found, nothing to do.
			MarkerTable;

		false ->
			?table:add_entry( definition_types_marker, NextASTLoc, MarkerTable )

	end,

	scan_forms( T, M#module_info{ types=NewTypeTable,
								  markers=NewMarkerTable },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );


% 7.1.11: Other, "wild" parse attributes: this section will come later, so that
% it matches only if none of the other attribute-related ones (such as for
% 'export_type') matched.


% 7.1.12: Type export handling [lacking in reference page].
%
% Supposedly:
%
% "If F is an attribute -export_type([Type_1/A_1, ..., Type_k/A_k]), then Rep(F)
% = {attribute, FILE_LOC, export_type, [{Type_1,A_1}, ..., {Type_k,A_k}]}."
%
scan_forms( _AST=[ _Form={ 'attribute', FileLoc, 'export_type', TypeIds } | T ],
			M=#module_info{ type_exports=ExportTable,
							types=TypeTable,
							markers=MarkerTable },
			NextASTLoc, CurrentFileReference ) ->

	%ast_utils:display_debug( "Type export declaration for ~p", [ TypeIds ] ),

	Context = { CurrentFileReference, FileLoc },

	% Records for each of these types this export:
	NewTypeTable = lists:foldl(

		fun( TypeId, TypeTableAcc ) ->

			{ Name, _Arity } = ast_type:check_type_id( TypeId, Context ),

			NewTypeInfo = case ?table:lookup_entry( TypeId, TypeTableAcc ) of

				key_not_found ->

					% New entry then (arity known, but not yet the detailed
					% variables):
					%
					#type_info{ name=Name,
								% Implicit:
								%variables=undefined,
								%opaque=undefined
								%ast_location=undefined
								%file_location=undefined
								%definition=[],
								exported=[ NextASTLoc ] };

				% A type *might* be exported more than once:
				{ value, TypeInfo } -> % F=#type_info{ exported=[] } } ->
					% Just add the fact that the type is exported then:
					NewExp = [ NextASTLoc | TypeInfo#type_info.exported ],
					TypeInfo#type_info{ exported=NewExp }

			end,

			?table:add_entry( TypeId, NewTypeInfo, TypeTableAcc )

		end,
		_Acc0=TypeTable,
		_List=TypeIds ),

	% Initially, exactly one export entry per location:
	NewExportTable =
		?table:add_new_entry( NextASTLoc, { FileLoc, TypeIds }, ExportTable ),

	NewMarkerTable = case ?table:has_entry( export_types_marker,
											MarkerTable ) of

		true ->
			% Already found, nothing to do.
			MarkerTable;

		false ->
			?table:add_entry( export_types_marker, NextASTLoc, MarkerTable )

	end,

	scan_forms( T, M#module_info{ type_exports=NewExportTable,
								  types=NewTypeTable,
								  markers=NewMarkerTable },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );



% 7.1.13: Compilation option handling [lacking in reference page].
%
% We may have here full or regular inlining (single function or list thereof),
% and possibly other options.
%
% Note that options can also be specified through the command line (e.g.
% "-Dmyriad_debug_mode").
%
scan_forms( _AST=[ Form={ 'attribute', FileLoc, 'compile', CompileInfo } | T ],
			 M=#module_info{ compilation_options=CompileTable,
							 compilation_option_defs=CompileDefs },
			NextASTLoc, CurrentFileReference ) ->

	Context = { CurrentFileReference, FileLoc },

	%ast_utils:display_debug( "Registration compilation option:~n~p~n",
	%                         [ CompileInfo ] ),

	NewCompileTable =
		register_compile_attribute( CompileInfo, CompileTable, Context ),

	NewCompileDefs = [ { NextASTLoc, Form } | CompileDefs ],

	scan_forms( T, M#module_info{ compilation_options=NewCompileTable,
								  compilation_option_defs=NewCompileDefs },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );


% 7.1.11: Other, "wild" parse attributes (includes not-so-wild ones, such as
% 'behaviour').
%
% (section body is here, to match iff none of the other attribute-related
% sections matched)
%
scan_forms(
		[ Form={ 'attribute', FileLoc, AttributeName, AttributeValue } | T ],
		M=#module_info{ parse_attributes=ParseAttributeTable },	NextASTLoc,
		CurrentFileReference ) ->

	%ast_utils:display_debug( "Parse attribute definition for '~p': ~p",
	%                         [ AttributeName, AttributeValue ] ),

	Context = { CurrentFileReference, FileLoc },

	check_parse_attribute_name( AttributeName, Context ),

	LocForm = { NextASTLoc, Form },

	% As a wild attribute may be defined more than once:
	NewParseAttributeTable = ?table:append_to_entry( AttributeName,
		{ AttributeValue, LocForm }, ParseAttributeTable ),

	scan_forms( T, M#module_info{ parse_attributes=NewParseAttributeTable },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );


% 7.1.14 : Parse errors handling.

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
% So we manage the errors by ourselves, aggregating them first, then reporting
% them (along with warnings) in a standard format so that tools can accommodate
% them.
%
% (for some reason, the reported lines are incremented, we have to decrement
% them)

% Preprocessor (eep) errors:


% eep include error:
scan_forms( _AST=[ _Form={ 'error',
		{ FileLoc, 'epp', { 'include', 'file', FileName } } } | _T ],
			_M=#module_info{ errors=_Errors },
			_NextASTLoc, CurrentFileReference ) ->

	% Better error message if managed directly:
	ast_utils:raise_usage_error( "include file not found: '~ts'.",
								 [ FileName ], CurrentFileReference, FileLoc );

	%NewError = { Context, { include_file_not_found, FileName } },

	%scan_forms( T, M#module_info{ errors=[ NewError | Errors ] },
	%   id_utils:get_next_sortable_id( NextASTLoc ),
	%   CurrentFileReference );


% eep undefined macro variable error:
scan_forms( _AST=[ _Form={ 'error',
		{ FileLoc, 'epp', { 'undefined', VariableName, 'none' } } } | T ],
			M=#module_info{ errors=Errors }, NextASTLoc,
			CurrentFileReference ) ->

	% Wrong and now, with Line replaced by FileLoc, meaningless:
	%Context = { CurrentFileReference, FileLoc-1 },
	Context = { CurrentFileReference, FileLoc },

	%ast_utils:raise_error( [ undefined_macro_variable, VariableName ],
	%                       Context );

	NewError = { Context, { undefined_macro_variable, VariableName } },

	scan_forms( T, M#module_info{ errors=[ NewError | Errors ] },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );


% eep general errors:
scan_forms( _AST=[ _Form={ 'error', { FileLoc, 'epp', Reason } } | T ],
			M=#module_info{ errors=Errors }, NextASTLoc,
			CurrentFileReference ) ->

	% Wrong and now, with Line replaced by FileLoc, meaningless:
	%Context = { CurrentFileReference, FileLoc-1 },
	Context = { CurrentFileReference, FileLoc },

	%ast_utils:raise_error( [ preprocessing_failed, Reason ], Context );

	NewError = { Context, { epp_error, Reason } },

	scan_forms( T, M#module_info{ errors=[ NewError | Errors ] },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );


% Parser (erl_scan) errors:
scan_forms( _AST=[ _Form={ 'error', { FileLoc, 'erl_scan', Reason } } | T ],
			M=#module_info{ errors=Errors }, NextASTLoc,
			CurrentFileReference ) ->

	% Wrong and now, with Line replaced by FileLoc, meaningless:
	%Context = { CurrentFileReference, FileLoc-1 },
	Context = { CurrentFileReference, FileLoc },

	%ast_utils:raise_error( [ parsing_failed, io_lib:format( Reason, [] ) ],
	%					   Context );

	NewError = { Context, { scan_error, Reason } },

	scan_forms( T, M#module_info{ errors=[ NewError | Errors ] },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );


% Parser (erl_parse) errors:
scan_forms( _AST=[ _Form={ 'error', { FileLoc, 'erl_parse', Reason } } | T ],
			M=#module_info{ errors=Errors }, NextASTLoc,
			CurrentFileReference ) ->

	% Wrong and now, with Line replaced by FileLoc, meaningless:
	%Context = { CurrentFileReference, FileLoc-1 },
	Context = { CurrentFileReference, FileLoc },

	%ast_utils:raise_error( [ parsing_failed, io_lib:format( Reason, [] ) ],
	%                       Context );

	NewError = { Context, { parse_error, Reason } },

	scan_forms( T, M#module_info{ errors=[ NewError | Errors ] },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );

% Any kind of other error:
scan_forms( _AST=[ _Form={ 'error', ErrorTerm } | _T ],
			_ModuleInfo, _NextASTLoc, CurrentFileReference ) ->

	% No line information available, so:
	ast_utils:raise_error( [ scan_error, ErrorTerm ],
						   _Context=CurrentFileReference );


% Any kind of warning:
scan_forms( _AST=[ _Form={ 'warning', WarningTerm } | T ],
			ModuleInfo, NextASTLoc, CurrentFileReference ) ->

	% No line information available, so:
	ast_utils:notify_warning( [ scan_warning, WarningTerm ],
							  _Context=CurrentFileReference ),

	scan_forms( T, ModuleInfo, id_utils:get_next_sortable_id( NextASTLoc ),
				CurrentFileReference );



% 7.1.15 : End of file handling.

% We expect the module name to be known when ending the processing (so that we
% can remove its corresponding file from the includes):
%
% Here, no module found:
scan_forms( _AST=[ _Form={ 'eof', FileLoc } ],
			M=#module_info{ module=undefined, errors=Errors }, _NextASTLoc,
			CurrentFileReference ) ->
	Context = { CurrentFileReference, FileLoc },

	NewError = { Context, "end of file reached whereas no suitable module "
						  "declaration (e.g. '-module(foobar).') found." },

	% Directly returned:
	M#module_info{ errors=[ NewError | Errors ] };



% Form expected to be defined once, and to be the last one:
scan_forms( _AST=[ Form={ 'eof', _FileLoc } ],
			M=#module_info{ module={ ModuleName, _ModuleLocDef },
							includes=Inc,
							function_exports=ExportTable,
							last_file_location=undefined,
							markers=MarkerTable },
			NextASTLoc, _CurrentFileReference ) ->

	%ast_utils:display_debug( "eof declaration at ~p.", [ FileLoc ] ),

	% Certainly better than id_utils:get_sortable_id_upper_bound(), as for
	% example we may want to add form after that original end:
	%
	EndLocation = NextASTLoc,

	NewMarkerTable = finalize_marker_table( EndLocation, MarkerTable ),

	LocForm = { NextASTLoc, Form },

	% End of file found, doing some housekeeping.

	% We generate here a default export form, supposedly at line 0 for easier
	% interpretation; it will start empty but may be enriched by parse
	% transforms, when they need to export functions that they just added.

	ExportASTLoc = ast_info:get_default_export_function_location(),

	NewExportTable = ?table:add_entry( ExportASTLoc,
		{ _FirstFileLoc=ast_utils:get_generated_code_location(), _Empty=[] },
		ExportTable ),

	% We just do not want to have the filename of the currently processed module
	% among the includes:

	% Reconstructs the supposedly deduced module filename:
	BinModFilename = text_utils:string_to_binary(
		atom_to_list( ModuleName ) ++ ".erl" ),

	% Due to the removal of include duplicates, can be listed only up to once:
	NoModInc = lists:delete( BinModFilename, Inc ),

	% Only "normal", non-recursing exit of that longer function:
	M#module_info{ includes=NoModInc,
				   function_exports=NewExportTable,
				   last_file_location=LocForm,
				   markers=NewMarkerTable };


%%
%% Section 7.9: Catch-all, to ensure that we captured all possible forms.
%%

scan_forms( _AST=[ UnhandledForm | T ],
			M=#module_info{ unhandled_forms=UnhandledForms }, NextASTLoc,
			CurrentFileReference ) ->

	ast_utils:display_error( "Unhandled form '~p' not managed.~n",
							 [ UnhandledForm ] ),

	%throw( { unhandled_form, UnhandledForm, { location, NextASTLoc },
	%        { file, CurrentFileReference } } ).

	LocForm = { NextASTLoc, UnhandledForm },

	NewUnhandledForms = [ LocForm | UnhandledForms ],

	scan_forms( T, M#module_info{ unhandled_forms=NewUnhandledForms },
		id_utils:get_next_sortable_id( NextASTLoc ), CurrentFileReference );


% The scan termination is expected to happen *only* when eof is found:
scan_forms( _AST=[], _ModuleInfo, _NextASTLoc, CurrentFileReference ) ->
	ast_utils:raise_error( [ no_eof_found ], CurrentFileReference );

scan_forms( Unexpected, _ModuleInfo, _NextASTLoc, CurrentFileReference ) ->
	ast_utils:raise_error( { not_an_ast, Unexpected }, CurrentFileReference ).




-doc "Registers the specified parse attribute regarding compilation.".
-spec register_compile_attribute( term(), compile_option_table(),
			scan_context() ) -> { compile_option_table(), [ located_form() ] }.
% Full inlining requested:
register_compile_attribute( _CompileInfo='inline', CompileTable, _Context ) ->
	% Overrides any previously existing inline entry:
	?table:add_entry( inline, all, CompileTable );


% Regular inlining:
register_compile_attribute( _CompileInfo={ 'inline', InlineValues },
		CompileTable, Context ) when is_list( InlineValues ) ->

	ast_utils:check_inline_options( InlineValues, Context ),

	NewInlineValues = case ?table:lookup_entry( inline, CompileTable ) of

		{ value, all } ->
			all;

		{ value, InlineList } ->
			InlineValues ++ InlineList;

		key_not_found ->
			InlineValues

	end,

	?table:add_entry( inline, NewInlineValues, CompileTable );


% Non-inlining, compile option pair with multiple values specified:
register_compile_attribute( _CompileInfo={ CompileOpt, OptValues },
							CompileTable, _Context )
		when is_atom( CompileOpt ) andalso is_list( OptValues ) ->

	?table:append_list_to_entry( CompileOpt, OptValues, CompileTable );


% Non-inlining, pair with a single compile option (hence not a list):
register_compile_attribute( CompileInfo={ CompileOpt, OptValue }, CompileTable,
							Context ) when is_atom( CompileOpt ) ->

	% Pair-based compile info option made of an atom and a non-list; not
	% currently specifically managed, yet not wanting spurious warnings: (refer
	% to https://www.erlang.org/doc/man/compile#file-2; last updated for Erlang
	% 26/erts-14.2, March 2024)
	%
	KnownCompileOptsPairTags = [ debug_info_key, makedep_output, makedep_target,
		error_location, source, outdir, i, d, parse_transform, no_auto_import,
		extra_chunks, check_ssa, warn_format, nowarn_bif_clash,
		nowarn_unused_function, nowarn_deprecated_function, nowarn_removed,
		nowarn_unused_record, nowarn_redefined_builtin_type, inline_size ],

	lists:member( CompileOpt, KnownCompileOptsPairTags ) orelse
		begin
			Msg = lists:flatten( io_lib:format( "Myriad-unknown compile "
				"option pair (yet still included): ~p", [ CompileInfo ] ) ),

			ast_utils:notify_warning( Msg, Context )

		end,

	?table:append_to_entry( CompileOpt, OptValue, CompileTable );


% For all compile info triplets:
register_compile_attribute(
		CompileInfo={ CompileOpt, FirstOptValue, SecondOptValue }, CompileTable,
		Context ) when is_atom( CompileOpt ) ->

	% The known triplets are:
	% - {d,Macro,Value}
	% - {feature, Feature, enable | disable}
	%
	KnownCompileOptsTripletTags = [ 'd', 'feature' ],

	lists:member( CompileOpt, KnownCompileOptsTripletTags ) orelse
		begin
			Msg = lists:flatten( io_lib:format( "Myriad-unknown compile "
				"option triplet (yet still included): ~p", [ CompileInfo ] ) ),

			ast_utils:notify_warning( Msg, Context )

		end,

	?table:append_to_entry( CompileOpt, { FirstOptValue, SecondOptValue },
							CompileTable );


register_compile_attribute( _CompileInfo=[], CompileTable, _Context ) ->
	CompileTable;


register_compile_attribute( _CompileInfo=[ CpInfo | T ], CompileTable,
							Context ) ->

	NewCompileTable = register_compile_attribute( CpInfo, CompileTable,
												  Context ),

	register_compile_attribute( T, NewCompileTable, Context );


register_compile_attribute( CompileInfoOpt, CompileTable,
							Context ) when is_atom( CompileInfoOpt ) ->

	% Atom-based compile info option with no parameter; not currently
	% specifically managed, yet not wanting spurious warnings: (refer to
	% https://www.erlang.org/doc/man/compile#file-2; last updated for Erlang
	% 26/erts-14.2, March 2024)
	%
	KnownOptionlessCompileOpts = [ brief, basic_validation, strong_validation,
		binary, bin_opt_info, compressed, debug_info, encrypt_debug_info,
		deterministic, makedep, makedep_side_effect, makedep_quote_target,
		makedep_add_missing, makedep_phony, 'P', 'E', 'S', recv_opt_info,
		report_errors, report_warnings, report, return_errors, return_warnings,
		warnings_as_errors, return, verbose, absolute_source, export_all,
		from_abstr, from_asm, from_core, no_spawn_compiler_process,
		no_strict_record_tests, no_error_module_mismatch, no_auto_import,
		no_line_info, no_lint, nowarn_bif_clash, nowarn_export_all,
		warn_export_vars, nowarn_shadow_vars, warn_keywords,
		nowarn_unused_function, nowarn_deprecated_function,
		nowarn_deprecated_type, nowarn_removed, nowarn_obsolete_guard,
		warn_unused_import, nowarn_underscore_match, nowarn_unused_vars,
		nowarn_unused_record, nowarn_unused_type, nowarn_nif_inline,
		warn_missing_spec, warn_missing_spec_all, nowarn_redefined_builtin_type,
		nowarn_opportunistic, nowarn_failed, nowarn_ignored, nowarn_nomatch ],


	lists:member( CompileInfoOpt, KnownOptionlessCompileOpts ) orelse
		begin
			Msg = io_lib:format( "Myriad-unknown compile option "
				"(yet still included): ~ts", [ CompileInfoOpt ] ),

			ast_utils:notify_warning( Msg, Context )

		end,

	?table:add_entry( CompileInfoOpt, _CompileOptValues=undefined,
					  CompileTable );


register_compile_attribute( Unexpected, _CompileTable, _Context ) ->
	throw( { unexpected_ast_compile_attribute, Unexpected } ).



-doc """
Processes the fields of a given record definition.

Note: field names could be full expressions here, but only atoms are allowed by
the parser (dixit the erl_id_trans parse transform).
""".
-spec scan_field_descriptions( [ ast_base:ast_element() ],
					ast_base:file_reference() ) -> ast_info:field_table().
scan_field_descriptions( FieldDescriptions, CurrentFileReference ) ->
	scan_field_descriptions( FieldDescriptions, CurrentFileReference,
							 _FieldTable=[] ).


% (helper)
scan_field_descriptions( _FieldDescriptions=[], _CurrentFileReference,
						 FieldTable ) ->
	% Preserve original field order:
	lists:reverse( FieldTable );

% Here no type or default value are specified for that field:
scan_field_descriptions( _FieldDescriptions=[
		{ 'record_field', FirstFileLoc,
		  { atom, SecondFileLoc, FieldName } } | T ],
		CurrentFileReference, FieldTable ) ->

	FieldDesc = { _FieldType=undefined, _DefaultValue=undefined, FirstFileLoc,
				  SecondFileLoc },

	NewFieldTable = [ { FieldName, FieldDesc } | FieldTable ],

	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );


% Here only a type is specified for that field:
scan_field_descriptions( _FieldDescriptions=[
		{ 'typed_record_field',
		  { 'record_field', FirstFileLoc,
			{ atom, SecondFileLoc, FieldName } },
		 FieldType } | T ], CurrentFileReference, FieldTable ) ->

	FieldDesc = { FieldType, _DefaultValue=undefined, FirstFileLoc,
				  SecondFileLoc },

	NewFieldTable = [ { FieldName, FieldDesc } | FieldTable ],

	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );


% Here only a default value is specified for that field:
scan_field_descriptions( _FieldDescriptions=[ { 'record_field', FirstFileLoc,
			{ atom, SecondFileLoc, FieldName }, DefaultValue } | T ],
						 CurrentFileReference, FieldTable ) ->

	FieldDesc = { _FieldType=undefined, DefaultValue, FirstFileLoc,
				  SecondFileLoc },

	NewFieldTable = [ { FieldName, FieldDesc } | FieldTable ],

	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );


% Here a type and a default, immediate value are specified for that field:
scan_field_descriptions( _FieldDescriptions=[
		{ 'typed_record_field',
		  { 'record_field', FirstFileLoc, { atom, SecondFileLoc, FieldName },
			DefaultValue }, FieldType } | T ],
						 CurrentFileReference, FieldTable ) ->

	%ast_utils:display_debug( "Field default value: ~p.", [ DefaultValue ] ),
	%ast_utils:display_debug( "Field type: ~p.", [ FieldType ] ),

	FieldDesc = { FieldType, DefaultValue, FirstFileLoc, SecondFileLoc },

	NewFieldTable = [ { FieldName, FieldDesc } | FieldTable ],

	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );


scan_field_descriptions( _FieldDescriptions=[ UnexpectedDesc | _T ],
						 CurrentFileReference, _FieldTable ) ->
	ast_utils:raise_error( [ unexpected_field_description, UnexpectedDesc ],
						   CurrentFileReference ).



-doc "Checks that the specified parse attribute name is legit.".
-spec check_parse_attribute_name( term(), form_context() ) ->
										parse_attribute_name().
check_parse_attribute_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_parse_attribute_name( Other, Context ) ->
	ast_utils:raise_error( [ invalid_parse_attribute_name, Other ], Context ).



-doc "Checks that the specified parse attribute name is legit.".
-spec check_parse_attribute_name( term() ) -> parse_attribute_name().
check_parse_attribute_name( Name ) ->
	check_parse_attribute_name( Name, _Context=undefined ).



-doc """
Finalizes the marker table, to ensure that, in all cases, after a scan all
markers are (adequately) defined (even if no clause in the AST triggered their
specific setting).
""".
-spec finalize_marker_table( ast_location(), marker_table() ) -> marker_table().
finalize_marker_table( EndMarkerLoc, MarkerTable ) ->

	% Useful for a later stack-based placement; we obtain a list of
	% {MarkerName,MarkerLoc} pairs, sorted by increasing locations:
	%
	OriginalPairs = get_ordered_marker_location_pairs( MarkerTable ),

	% No bound kept:
	[ { begin_marker, BeginMarkerLoc } | BoundlessPairs ] = OriginalPairs,

	% First a basic bound check:
	BeginMarkerLoc < EndMarkerLoc orelse
		throw( { end_before_begin, EndMarkerLoc, BeginMarkerLoc } ),


	% By design begin_marker has already been set in the table, and end_marker
	% will be registered now:
	%
	EndMarkerTable = ?table:add_new_entry( end_marker, EndMarkerLoc,
										   MarkerTable ),


	% We have now to handle all cases about whether each of the other,
	% intermediate markers is already available or not, so that ultimately, all
	% of them are set, and in the expected order.
	%
	% This can be done only in an approximate manner, as the vanilla AST may
	% rely on a different - yet, compilation-wise, still legit - section order.
	%
	% The main rule to abide is that exports and imports shall precede
	% definitions. The rest is mostly a matter of convention.
	%
	% For that, we proceed first per marker name in ascending order (as defined
	% in ast_info:section_marker/0), and for each of them, in turn, we simply
	% unstack location pairs accordingly.

	% Marker shorthands:

	ModMarker = module_marker,

	InterMarkers = [ export_types_marker, export_functions_marker,
					 import_functions_marker, definition_records_marker,
					 definition_types_marker ],

	F = _FunDefMarker = definition_functions_marker,

	% The newer, simpler algorithm supposes that the only order that matters
	% between markers is that:
	%
	% - the begin marker comes first
	% - then the module marker (ModMarker)
	% - then the intermediate markers (InterMarkers), in no specific order
	% - then the definition marker (FunDefMarker, a.k.a. as F)
	% - then the end marker

	ModMarkerLoc = case ?table:lookup_entry( ModMarker, EndMarkerTable ) of

		key_not_found ->
			% An AST without a module definition will probably be a problem;
			% trying to mitigate it as we can:
			%
			BeginMarkerLoc;

		{ value, ModLoc } ->
			ModLoc

	end,

	{ NewFLoc, DefaultLoc } = case ?table:lookup_entry( F, EndMarkerTable ) of

		key_not_found ->

			% F is not located yet, we will find a proper location then:
			case lists:reverse( BoundlessPairs ) of

				[] ->
					% Here no intermediate marker at all was defined, we thus
					% define two of them, a first (for the non-F markers) and a
					% second, at its right (for F):
					%
					NonFLoc = id_utils:get_sortable_id_between( ModMarkerLoc,
																EndMarkerLoc ),

					FLoc = id_utils:get_sortable_id_between( NonFLoc,
															 EndMarkerLoc ),

					{ FLoc, NonFLoc };

				[ { _LastMarker, LastMarkerLoc } | _Others ] ->
					% Here we have at least one non-F location, that will be
					% used for all non-F markers that are not located yet, and
					% also to establish the location of F:
					%
					FLoc = id_utils:get_sortable_id_between( LastMarkerLoc,
															 EndMarkerLoc ),

					{ FLoc, LastMarkerLoc }

			end;

		{ value, FLoc } ->
			DefLoc = id_utils:get_sortable_id_between( ModMarkerLoc, FLoc ),
			{ FLoc, DefLoc }

	end,

	NewFLoc > DefaultLoc orelse
		throw( { wrong_location_order, DefaultLoc, NewFLoc } ),

	MarkerEntries = [ { ModMarker, ModMarkerLoc }, { F, NewFLoc } ],

	UpdatedMarkerTable = ?table:add_entries( MarkerEntries, EndMarkerTable ),

	add_missing_markers( InterMarkers, DefaultLoc, UpdatedMarkerTable ).



% (helper)
add_missing_markers( _Markers=[], _DefaultLoc, MarkerTable ) ->
	MarkerTable;

add_missing_markers( _Markers=[ M | T ], DefaultLoc, MarkerTable ) ->

	NewMarkerTable = case ?table:has_entry( M, MarkerTable ) of

		true ->
			MarkerTable;

		false ->
			?table:add_entry( M, DefaultLoc, MarkerTable )

	end,
	add_missing_markers( T, DefaultLoc, NewMarkerTable ).



-doc """
Returns a list of {MarkerName,MarkerLoc} pairs, sorted by increasing locations.

(helper)
""".
-spec get_ordered_marker_location_pairs( marker_table() ) ->
					[ { ast_info:section_marker(), ast_location() } ].
get_ordered_marker_location_pairs( MarkerTable ) ->
	lists:keysort( _Index=2, ?table:enumerate( MarkerTable ) ).
