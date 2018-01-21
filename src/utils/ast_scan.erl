% Copyright (C) 2018-2018 Olivier Boudeville
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
% Creation date: Sunday, January 21, 2018.



% Module in charge of scanning an AST, notably in order to transform it
% (meta-programming).
%
-module(ast_scan).


% In-file reference, typically like:
% {"../data-management/simple_parse_transform_target.erl",1}.
%
-type file_reference() :: basic_utils:maybe( file_utils:file_path() ).


-type action_trigger() :: atom().

-type transformer() :: fun().


% Describes the various actions (ex: transformations) that are to be applied to
% an AST through callbacks.
%
-type action_table() :: table:table( action_trigger(),
									 basic_utils:maybe( transformer() ) ).

-export_type([ ast/0 ]).

-export([ scan/1, scan_forms/2 ]).


% Shorthands:

-type function_name() :: meta_utils:function_name().
-type function_arity() :: meta_utils:function_arity().
-type function_id() :: meta_utils:function_id().
-type function_clause() :: form().



% Implementation notes:
%
% The integral scanning of ASTs got inspiration from erl_id_trans.erl.
%
% Numbered sections refer to the ones in
% http://erlang.org/doc/apps/erts/absform.html, with additional subsections for
% each bullet.





% Scans the specified AST, expected to correspond to a module definition, and
% returns the corresponding module_info record.
%
-spec scan( ast() ) -> module_info().
scan( AST ) ->

	InitModuleInfo = meta_utils:init_module_info(),

	% Application of 7.1.1:
	%
	% "If D is a module declaration consisting of the forms F_1, ..., F_k, then
	% Rep(D) = [Rep(F_1), ..., Rep(F_k)]."
	%
	scan_forms( AST, InitModuleInfo, id_utils:get_initial_sortable_id(),
				_CurrentFileRef=undefined ).


%%
%% First part: top-level forms.
%%



% Main scanning function.
%
% Here all relevant parts of the specified AST (located forms) are matched in
% turn, and stored in the specified module_info once located using
% id_utils:sortable_id/0 identifiers, which allows easy insertions and
% reordering.
%
-spec scan_forms( ast(), module_info(), id_utils:sortable_id(),
				  file_reference() ) -> module_info().


% Overall structure (based on http://erlang.org/doc/apps/erts/absform.html):
%
% Section 7.1: Module Declarations and Forms:
%  - (7.1.1 already done)
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
%  - 7.1.15 : Enf of file

% Extra Section 7.9: Catch-all for unexpected forms



%%
%% Section 7.1: Module Declarations and Forms.
%%


% (7.1.1 already done)


% 7.1.2: Function export handling.
%
% "If F is an attribute -export([Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) =
% {attribute,LINE,export,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}."
%
scan_forms( _AST=[ _Form={ attribute, Line, export, FunctionIds } | T ],
			M=#module_info{ function_exports=ExportTable,
							functions=FunctionTable },
			NextLocation, CurrentFileReference ) ->

	%ast_utils:display_debug( "function export declaration for ~p",
	% [ FunctionIds ] ),

	Context = { CurrentFileReference, Line },

	% Records for each of these functions this export:
	NewFunctionTable = lists:foldl(

		fun( FunId, FunTableAcc ) ->

			{ Name, Arity } = ast_utils:check_function_id( FunId, Context ),

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

	scan_forms( T, M#module_info{ function_exports=NewExportTable,
								  functions=NewFunctionTable },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.3: Function import handling.
%
% "If F is an attribute -import(Mod,[Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) =
% {attribute,LINE,import,{Mod,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}}."
%
scan_forms( _AST=[ Form={ attribute, Line, import,
							{ ModuleName, FunIds } } | T ],
			 M=#module_info{ function_imports=ImportTable,
							 function_imports_defs=ImportDefs },
			NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line },

	ast_utils:check_module_name( ModuleName, Context ),
	ast_utils:check_function_ids( FunIds, Context ),

	NewImportTable = ?table:appendListToEntry( ModuleName, FunIds,
											   ImportTable ),

	NewImportDefs = [ { NextLocation, Form } | ImportDefs ],

	scan_forms( T, M#module_info{ function_imports=NewImportTable,
								  function_imports_defs=NewImportDefs },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.4: Module handling.

% "If F is an attribute -module(Mod), then Rep(F) =
% {attribute,LINE,module,Mod}."
%
scan_forms( _AST=[ Form={ attribute, Line, module, ModuleName } | T ],
			 M=#module_info{ module=undefined, module_def=undefined },
			 NextLocation, CurrentFileReference ) ->

	%ast_utils:display_debug( "module declaration for ~s", [ ModuleName ] ),

	Context = { CurrentFileReference, Line },

	ast_utils:check_module_name( ModuleName, Context ),

	% When processing X.beam, we should not remove the lines like:
	% {attribute,37,file,{"X.erl",37} as they allow to report errors
	% appropriately.

	LocForm = { NextLocation, Form },

	scan_forms( T, M#module_info{ module=ModuleName, module_def=LocForm },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );


% Any lacking, invalid or duplicated module declaration will be caught by
% the compiler anyway.
%
scan_forms( _AST=[ Form={ attribute, Line, module, ModuleName } | T ],
			 M=#module_info{ module=PreviousModuleName },
			 NextLocation, _CurrentFileReference ) ->
	throw( { multiple_module_definitions, PreviousModuleName, ModuleName,
			 { CurrentFileReference, Line } } );



% 7.1.5: File reference (include) handling.
%
% "If F is an attribute -file(File,Line), then Rep(F) =
% {attribute,LINE,file,{File,Line}}."
%
% Allows to keep track of when an included file begins and also ends.
%
scan_forms( _AST=[ Form={ attribute, Line, { FilePath, FileLine } } | T ],
			 M=#module_info{ includes=Inc, include_defs=IncDefs }, NextLocation,
			_CurrentFileReference ) ->

	%ast_utils:display_debug( "file declaration with ~s at #~B",
	% [ FilePath, FileLine ] ),

	% We used to normalise paths, however then 'file_utils' would have to be
	% bootstrapped as well, which does not seem desirable.

	%NormFilepath = text_utils:string_to_binary(
	%     file_utils:normalise_path( Filepath ) ),
	NormFilepath = text_utils:string_to_binary( Filepath ),

	% Avoids duplicates (in 'includes' only, not in definitions):
	%
	NewFilepaths = case lists:member( NormFilepath, Inc ) of

		true ->
			Inc;

		false ->
			[ NormFilepath | Inc ]

	end,

	LocForm = { NextLocation, Form },

	scan_forms( T, M#module_info{ includes=NewFilepaths,
								  include_defs=[ LocForm | IncDefs ] },
				id_utils:get_next_sortable_id( NextLocation ), FilePath );




% 7.1.6: Function definition handling.
%
% "If F is a function declaration Name Fc_1 ; ... ; Name Fc_k, where each Fc_i
% is a function clause with a pattern sequence of the same length Arity, then
% Rep(F) = {function,LINE,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)]}."
%
scan_forms( [ { function, Line, FunctionName, FunctionArity, Clauses } | T ],
			M=#module_info{ functions=FunctionTable }, NextLocation,
			CurrentFileReference ) ->

	%ast_utils:display_debug( "function definition for ~p/~p",
	% [ FunctionName, FunctionArity ] ),

	% The non-first clauses could be checked as well:
	%
	% (when adding a function, we may not check if ever there was a pre-existing
	% one - multiple definitions will be rejected by the compiler anyway)

	Context = { CurrentFileReference, Line },

	ast_utils:check_function_name( FunctionName, Context ),
	ast_utils:check_arity( FunctionArity, Context ),
	ast_utils:check_function_clauses( Clauses, FunctionArity, Context ),

	FunId = { FunctionName, FunctionArity },

	FunInfo = case ?table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->
			% New entry then:
			#function_info{ name=FunctionName,
							arity=FunctionArity,
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
			raise_error( { multiple_definition_for, FunId, Context } )

	end,

	NewFunctionTable = ?table:addEntry( _K=FunId, _V=FunInfo, FunctionTable ),

	%ast_utils:display_debug( "function ~s/~B with ~B clauses registered.",
	%			   [ FunctionName, FunctionArity, length( Clauses ) ] ),

	scan_forms( T, M#module_info{ functions=NewFunctionTable },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.7: Local function type specification handling.
%
% "If F is a function specification -Spec Name Ft_1; ...; Ft_k, where Spec is
% either the atom spec or the atom callback, and each Ft_i is a possibly
% constrained function type with an argument sequence of the same length Arity,
% then Rep(F) = {attribute,Line,Spec,{{Name,Arity},[Rep(Ft_1), ...,
% Rep(Ft_k)]}}."
%
scan_forms( [ Form={ attribute, Line, SpecAtom,
					 { FunId, FunctionTypes } } | T ],
			W=#module_info{ functions=FunctionTable },
			NextLocation, CurrentFileReference )
  when SpecAtom == spec orelse SpecAtom == callback ->

	Context = { CurrentFileReference, Line },

	{ FunctionName, FunctionArity } = ast_utils:check_function_id( FunId,
																   Context ),

	ast_utils:check_function_types( FunctionTypes, FunctionArity, Context ),

	%ast_utils:display_debug( "spec definition for ~p/~p",
	% [ FunctionName, FunctionArity ] ),

	LocatedSpec = { NextLocation, Form },

	FunInfo = case ?table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->

			% New entry then:
			#function_info{ name=FunctionName,
							arity=FunctionArity,
							% Implicit:
							%location=undefined,
							%line=undefined,
							%definition=[]
							spec=LocatedSpec };

		{ value, F=#function_info{ spec=undefined } } ->
			% Just add that spec form then:
			F#function_info{ spec=LocatedSpec };

		% Here a spec was already set:
		_ ->
			raise_error( { multiple_spec_for, FunId } )

	end,

	NewFunctionTable = ?table:addEntry( _K=FunId, _V=FunInfo, FunctionTable ),

	%ast_utils:display_debug( "spec for function ~s/~B registered.",
	%		   [ FunctionName, FunctionArity ] ),

	scan_forms( T, W#module_info{ functions=NewFunctionTable },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.8: Remote function type specification handling.
%
% "If F is a function specification -spec Mod:Name Ft_1; ...; Ft_k, where each
% Ft_i is a possibly constrained function type with an argument sequence of the
% same length Arity, then Rep(F) =
% {attribute,Line,spec,{{Mod,Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}."
%
scan_forms( [ Form={ attribute, Line, spec,
					 _MFA={ ModuleName, FunctionName, FunctionArity },
					 FunctionTypes } | T ],
			W=#module_info{ remote_spec_defs=RemoteSpecDefs },
			NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line },

	ast_utils:check_module_name( ModuleName, Context ),
	ast_utils:check_function_id( FunId, Context ),
	ast_utils:check_function_types( FunctionTypes, FunctionArity, Context ),

	%ast_utils:display_debug( "remote spec definition for ~p/~p",
	% [ FunctionName, FunctionArity ] ),

	% Specs for remote functions not specifically processed.

	LocatedSpec = { NextLocation, Form },

	NewRemoteSpecDefs = [ LocatedSpec | RemoteSpecDefs ],

	%ast_utils:display_debug( "remote spec for function ~s/~B registered.",
	%		   [ FunctionName, FunctionArity ] ),

	scan_forms( T, W#module_info{ remote_spec_defs=NewRemoteSpecDefs },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.9: Record definition handling.
%
% "If F is a record declaration -record(Name,{V_1, ..., V_k}), where each V_i is
% a record field, then Rep(F) = {attribute,LINE,record,{Name,[Rep(V_1), ...,
% Rep(V_k)]}}. For Rep(V), see below.
%
scan_forms( _AST=[ Form={ attribute, Line, record, { RecordName, DescFields } }
				   | T ],
			M=#module_info{ records=RecordTable, record_defs=RecordDefs },
			NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line },

	ast_utils:check_record_name( RecordName, Context ),

	FieldTable = scan_field_descriptions( DescFields, CurrentFileReference ),

	NewRecordTable = ?table:addNewEntry( RecordName, FieldTable, RecordTable ),

	NewRecordDefs = [ { NextLocation, Form } | RecordDefs ],

	scan_forms( T, W#module_info{ records=NewRecordTable,
								  record_defs=NewRecordDefs },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.10: Type definition handling.
%
% "If F is a type declaration -Type Name(V_1, ..., V_k) :: T, where Type is
% either the atom type or the atom opaque, each V_i is a variable, and T is a
% type, then Rep(F) = {attribute,LINE,Type,{Name,Rep(T),[Rep(V_1), ...,
% Rep(V_k)]}}."
%
scan_forms( _AST=[ Form={ attribute, Line, Type,
						   { TypeName, TypeDef, Variables } } | T ],
			 M=#module_info{ types=TypeTable },
			 NextLocation, CurrentFileReference )
  when Type == type orelse Type == opaque ->

	%ast_utils:display_debug( "type declaration for ~p: ~p", [
	%                        TypeName, Form ] ),

	Context = { CurrentFileReference, Line },

	ast_utils:check_type_name( Type, Context ),
	ast_utils:check_type_definition( TypeDef, Context ),
	ast_utils:check_variables( Variables, Context ),

	IsOpaque = case Type of

		type ->
			false;

		_Opaque ->
			true

	end,

	TypeArity = length( Variables ),

	TypeId = { TypeName, TypeArity },

	LocForm = { NextLocation, Form },

	NewTypeTable = case ?table:lookupEntry( TypeId, TypeTable ) of

		{ value, TypeInfo } ->
			ast_utils:raise_error( [ multiple_definitions_for_type, TypeId ],
								   Context );

		key_not_found ->
			TypeInfo = #type_info{ name=TypeName,
								   arity=TypeArity,
								   opaque=IsOpaque,
								   location=NextLocation,
								   line=Line,
								   definition=TypeDef,
								   %exported
								 },

			?table:addEntry( TypeId, TypeInfo, TypeTable )

	end,

	scan_forms( T, M#module_info{ types=NewTypeTable },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.11: Other, "wild" parse attributes.
%
scan_forms( [ Form={ attribute, Line, AttributeName, AttributeValue } | T ],
			M=#module_info{ parse_attributes=ParseAttributeTable,
							parse_attribute_defs=AttributeDefs },
			NextLocation, CurrentFileReference ) ->

	%ast_utils:display_debug( "attribute definition for ~p",
	% [ AttributeName ] ),

	Context = { CurrentFileReference, Line },

	ast_utils:check_parse_attribute_name( AttributeName, Context ),
	% No constraint on AttributeValue applies.

	LocForm = { NextLocation, Form },

	scan_forms( T, M#module_info{
				   parse_attributes=?table:addEntry( AttributeName,
										  AttributeValue, ParseAttributeTable ),
				   parse_attribute_defs=[ LocForm | AttributeDefs ] },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.12: Type export handling [lacking in reference page].
%
% Supposedly:
%
% "If F is an attribute -export_type([Type_1/A_1, ..., Type_k/A_k]), then Rep(F)
% = {attribute,LINE,export_type,[{Type_1,A_1}, ..., {Type_k,A_k}]}."
%
scan_forms( _AST=[ _Form={ attribute, Line, export_type, TypeIds } | T ],
			M=#module_info{ type_exports=ExportTable, types=TypeTable },
			NextLocation, CurrentFileReference ) ->

	%ast_utils:display_debug( "type export declaration for ~p", [ TypeIds ] ),

	Context = { CurrentFileReference, Line },

	% Records for each of these types this export:
	NewTypeTable = lists:foldl(

		fun( TypeId, TypeTableAcc ) ->

			{ Name, Arity } = ast_utils:check_type_id( TypeId, Context ),

			NewTypeInfo = case ?table:lookupEntry( TypeId, TypeTableAcc ) of

				 key_not_found ->

					% New entry then:
					#type_info{ name=Name,
								arity=Arity,
								% Implicit:
								%opaque=undefined
								%location=undefined
								%line=undefined
								%definition=[],
								exported=[ NextLocation ] };

				 % A type *might* be exported more than once:
				 { value, TypeInfo } -> % F=#type_info{ exported=false } } ->
					% Just add the fact that the type is exported then:
					NewExp = [ NextLocation | TypeInfo#type_info.exported ],
					TypeInfo#type_info{ exported=NewExp }

			end,

			?table:addEntry( TypeId, NewTypeInfo, TypeTableAcc )

		end,
		_Acc0=TypeTable,
		_List=TypeIds ),

	% Initially, exactly one export entry per location:
	NewExportTable = ?table:addNewEntry( NextLocation, { Line, TypeIds },
										 ExportTable ),

	scan_forms( T, M#module_info{ type_exports=NewExportTable,
								  types=NewTypeTable },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.13: Compilation option handling [lacking in reference page].
%
% We may have here full or regular inlining, and possibly other options.
%

% Full inlining:
scan_forms( _AST=[ Form={ attribute, Line, compile, inline } | T ],
			 M=#module_info{ compilation_options=CompileTable,
							 compilation_option_defs=CompileDefs },
			NextLocation, CurrentFileReference ) ->

	% Overrides any previously existing entry:
	NewCompileTable = ?table:addEntry( inline, all, CompileTable ),

	NewCompileDefs = [ { NextLocation, Form } | CompileDefs ],

	scan_forms( T, M#module_info{ compilation_options=NewCompileTable,
								  compilation_option_defs=NewCompileDefs },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );


% Regular inlining:
scan_forms( _AST=[ Form={ attribute, _Line, compile,
						  { inline, InlineOpts } } | T ],
			 M=#module_info{ compilation_options=CompileTable,
							 compilation_option_defs=CompileDefs },
			NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line },

	ast_utils:check_inline_options( InlineOpts, Context ),

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

	scan_forms( T, M#module_info{ compilation_options=NewCompileTable,
								  compilation_option_defs=NewCompileDefs },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );


scan_forms( _AST=[ Form={ attribute, _Line, compile,
							{ CompilationOption, Options } } | T ],
			 M=#module_info{ compilation_options=CompileTable,
							 compilation_option_defs=CompileDefs },
			NextLocation, CurrentFileReference ) ->

	NewCompileTable = ?table:appendListToEntry( CompilationOption, Options,
												CompileTable ),

	NewCompileDefs = [ { NextLocation, Form } | CompileDefs ],

	scan_forms( T, M#module_info{ compilation_options=NewCompileTable,
								  compilation_option_defs=NewCompileDefs },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );


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
% So we manage the errors by ourselves (losing the emacs error mode support).

% (for some reason, the reported lines are incremented, we have to decrement
% them)

% Preprocessor (eep) errors:


% eep include error:
scan_forms( _AST=[ _Form={ error,
	   { Line, epp, { include, file, FileName } } } | _T ], _ModuleInfo,
			 _NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line-1 },

	ast_utils:raise_error( [ include_file_not_found, FileName ], Context );


% eep undefined macro variable error:
scan_forms( _AST=[ _Form={ error,
	   { Line, epp, { undefined, VariableName, none } } } | _T ], _ModuleInfo,
			 _NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line-1 },

	ast_utils:raise_error( [ undefined_macro_variable, VariableName ],
						   Context );


% eep general errors:
scan_forms( _AST=[ _Form={ error, { Line, epp, Reason } } | _T ], _ModuleInfo,
			 _NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line-1 },

	ast_utils:raise_error( [ preprocessing_failed, Reason ], Context );


% Parser (erl_parse) errors:
scan_forms( _AST=[ _Form={ error, { Line, erl_parse, Reason } } | _T ],
			 _ModuleInfo, _NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line-1 },

	ast_utils:raise_error( [ parsing_failed, io_lib:format( Reason, [] ) ],
						   Context );


% Any kind of other error:
scan_forms( _AST=[ _Form={ error, ErrorTerm } | _T ],
			 _ModuleInfo, _NextLocation, CurrentFileReference ) ->

	ast_utils:raise_error( [ scan_error, ErrorTerm],
						   _Context=CurrentFileReference );

% Any kind of warning:
scan_forms( _AST=[ _Form={ warning, WarningTerm } | T ],
			ModuleInfo, NextLocation, CurrentFileReference ) ->

	ast_utils:notify_warning( [ scan_warning, WarningTerm ],
							  _Context=CurrentFileReference ),

	scan_forms( T, ModuleInfo, id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.15 : End of file handling.

% We expect the module name to be known when ending the processing:
scan_forms( _AST=[ _Form={ eof, Line } ],
			Infos=#module_info{ module=undefined }, _NextLocation,
			CurrentFileReference ) ->
	Context = { CurrentFileReference, Line },
	ast_utils:raise_error( [ eof_while_no_module, Infos ], Context );


% Form expected to be defined once, and to be the last one:
scan_forms( _AST=[ Form={ eof, _Line } ],
			M=#module_info{ last_line=undefined, module=Module, includes=Inc },
			_NextLocation, CurrentFileReference ) ->

	%ast_utils:display_debug( "eof declaration at ~p", [ Line ] ),

	% Surely not wanting anything to be able to go past it:
	LocForm = { id_utils:get_sortable_id_upper_bound(), Form },

	% End of file found, doing some housekeeping.

	% We just do not want to have the filename of the currently processed module
	% among the includes:

	% Reconstructs the supposedly deduced module filename:
	ModFilename = atom_to_list( Module ) ++ ".erl",

	% Due to the removal of include duplicates, can be listed only up to once:
	NoModInc = lists:delete( ModFilename, Inc ),

	% Only "normal", non-recursing exit of that function:
	M#module_info{ includes=NoModInc, last_line=LocForm };


%%
%% Section 7.9: Catch-all, to ensure that we captured all possible forms.
%%

scan_forms( _AST=[ UnhandledForm | T ],
			M=#module_info{ unhandled_forms=UnhandledForms }, NextLocation,
			CurrentFileReference ) ->

	% display_error( "unhandled form '~p' not managed.~n", [ Form ] ),
	%throw( { unhandled_form, UnhandledForm, { location, NextLocation },
	%		 { file, CurrentFileReference } } ).

	LocForm = { NextLocation, Form },

	NewUnhandledForms = [ LocForm | UnhandledForms ],

	scan_forms( T, M#module_info{ unhandled_forms=NewUnhandledForms },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );


% Scan termination expected to happen only when eof is found:
scan_forms( _AST=[], _ModuleInfo, _NextLocation, CurrentFileReference ) ->
	ast_utils:raise_error( [ no_eof_found ], CurrentFileReference ).





%%
%% Second part: sub-forms.
%%




% Processes the fields of a given record.
%
-spec scan_field_descriptions( [ ast_utils:ast_element() ],
							   file_reference() ) -> field_table().
scan_field_descriptions( FieldDescriptions, CurrentFileReference ) ->

	FieldTable = ?table:new(),

	scan_field_descriptions( FieldDescriptions, CurrentFileReference,
							 CurrentFileReference, FieldTable ).


scan_field_descriptions( _FieldDescriptions=[], _CurrentFileReference,
						 CurrentFileReference, FieldTable ) ->
	FieldTable;


% Here no type or default value are specified for that field:
scan_field_descriptions( _FieldDescriptions=[
		{ record_field, _Line1, { atom, _Line2, FieldName } } | T ],
		CurrentFileReference, FieldTable ) ->

	NewFieldTable = ?table:addNewEntry( FieldName,
		  { _FieldType=undefined, _DefaultValue=undefined }, FieldTable ),
	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );

% Here only a type is specified for that field:
scan_field_descriptions( _FieldDescriptions=[
	   { typed_record_field,
		  {record_field, _Line1, { atom, _Line2, FieldName } }, FieldType }
												| T ],
						 CurrentFileReference, FieldTable ) ->
	NewFieldTable = ?table:addNewEntry( FieldName,
					  { FieldType, _DefaultValue=undefined }, FieldTable ),
	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );

% Here only a default value is specified for that field:
scan_field_descriptions( _FieldDescriptions=[
	   { record_field, _Line1, { atom, _Line2, FieldName }, DefaultValue }
												| T ],
						 CurrentFileReference, FieldTable ) ->
	NewFieldTable = ?table:addNewEntry( FieldName,
		  { _FieldType=undefined, DefaultValue }, FieldTable ),
	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );

% Here a type and a default, immediate value are specified for that field:
scan_field_descriptions( _FieldDescriptions=[
	   { typed_record_field,
		  { record_field, _Line1,
		   { atom, _Line2, FieldName }, DefaultValue }, FieldType }
												| T ],
						 CurrentFileReference, FieldTable ) ->
	NewFieldTable = ?table:addNewEntry( FieldName, { FieldType, DefaultValue },
										FieldTable ),
	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );


scan_field_descriptions( _FieldDescriptions=[ UnexpectedDesc | _T ],
						 _CurrentFileReference, _FieldTable ) ->
	throw( { unexpected_field_description, UnexpectedDesc } ).




xxxxxxxxxxx previous


% Scans the specified forms.
%
-spec scan_forms( [ form() ], module_info() ) -> module_info().
scan_forms( Forms, ModuleInfo ) ->
	[ scan_form( F,  ) || F <- Forms ].



% Scans the specified form.
%
-spec scan_form( form(), action_table() ) -> form().
scan_form( F={ attribute, Line, module, Module }, _ActionTable ) ->
	F;

scan_form( F={ attribute, Line, file, { File, Line } }, _ActionTable ) ->
	F;

scan_form( _F={ attribute, Line, export, FunIds }, ActionTable ) ->
	ScannedFunIds = scan_function_ids( FunIds, ActionTable ),
	{ attribute, Line, export, ScannedFunIds };

scan_form( _F={ attribute, Line, import, { Module, FunIds }, ActionTable } ) ->
	ScannedFunIds = scan_function_ids( FunIds, ActionTable ),
	{ attribute, Line, import, { Module, ScannedFunIds } };

scan_form( _F={ attribute, Line, export_type, TypeIds }, ActionTable ) ->
	ScannedTypeIds = scan_type_ids( TypeIds, ActionTable ),
	{ attribute, Line, export_type, ScannedTypeIds };

scan_form( _F={ attribute, Line, optional_callbacks, FunIds }, ActionTable ) ->
	% Apparently not always scannable as is (try/catch could be added):
	ScannedFunIds = scan_function_ids( FunIds, ActionTable ),
	{ attribute, Line, optional_callbacks, ScannedFunIds };

scan_form( F={ attribute, Line, compile, C }, ActionTable ) ->
	F;

scan_form( _F={ attribute, Line, record, { RecordName, RecordDefs } },
		   ActionTable ) ->
	NewRecordDefs = scan_record_defs( RecordDefs, ActionTable ),
	{ attribute, Line, record, { RecordName, NewRecordDefs } };

scan_form( F={ attribute, Line, asm, { function, N, A, Code } },
		   _ActionTable ) ->
	F;

scan_form( _F={ attribute, Line, type, { TypeName, TypeDef, TypeVars } },
		   ActionTable ) ->
	ScannedTypeDef = scan_type_def( TypeDef, ActionTable ),
	ScannedTypeVars = scan_variables( TypeVars, ActionTable ),
	{ attribute, Line, type, { TypeName, ScannedTypeDef, ScannedTypeVars } };

scan_form( _F={ attribute, Line, opaque, { TypeName, TypeDef, TypeVars } },
		   ActionTable ) ->
	ScannedTypeDef = scan_type_def( TypeDef, ActionTable ),
	ScannedTypeVars = scan_variables( TypeVars, ActionTable ),
	{ attribute, Line, opaque, { TypeName, ScannedTypeDef, ScannedTypeVars } };


scan_form( _F={ attribute, Line, spec, { Fun={ FunctionName, Params },
										 FunctionTypes } }, ActionTable ) ->
	NewFunctionTypes = scan_function_types( FunctionTypes, ActionTable ),
	{ attribute, Line, spec, { Fun, NewFunctionTypes } };

scan_form( _F={ attribute, Line, spec,
				{ Fun={ ModuleName, FunctionName, Params }, FunctionTypes } },
		   ActionTable ) ->
	NewFunctionTypes = scan_function_types( FunctionTypes, ActionTable ),
	{ attribute, Line, spec, { Fun, NewFunctionTypes } };

scan_form( _F={ attribute, Line, callback, { Fun={ FunctionName, Params },
											 FunctionTypes } }, ActionTable ) ->
	NewFunctionTypes = scan_function_types( FunctionTypes, ActionTable ),
	{ attribute, Line, callback, { Fun, NewFunctionTypes } };


% Any other, general attribute:
scan_form( F={ attribute, Line, AttributeName, AttributeValue },
		   _ActionTable ) ->
	F;

% Functions:
scan_form( { function, Line, Name, Arity, Clauses }, ActionTable ) ->
	{ ScannedName, ScannedArity, ScannedClauses } = scan_function( Name, Arity,
													   Clauses, ActionTable ),
	{ function, Line, ScannedName, ScannedArity, ScannedClauses };


% Extra forms from the parser:
scan_form( F={ error, E }, _ActionTable ) ->
	F;

scan_form( F={ warning, W }, _ActionTable ) ->
	F;

scan_form( F={ eof, Line }, _ActionTable ) ->
	F.




% Function section.


-spec scan_function_ids( [ function_id() ], action_table() ) ->
							   [ function_id() ].
scan_function_ids( FunIds, ActionTable ) when is_list( FunIds ) ->
	[ scan_function_id( FunId, ActionTable ) || FunId <- FunIds ];

scan_function_ids( InvalidFunIds, _ActionTable ) ->
	throw( { invalid_function_identifiers, InvalidFunIds } ).


scan_function_id( FunId={ FunctionName, FunctionArity }, ActionTable ) ->
	check_function_name( FunctionName ),
	check_function_arity( FunctionArity ),
	FunId;

scan_function_id( InvalidFunId, _ActionTable ) ->
	throw( { invalid_function_id, InvalidFunId } ).



check_function_name( FunctionName ) when is_atom( FunctionName ) ->
	ok;

check_function_name( InvalidFunctionName ) ->
	throw( { invalid_function_name, InvalidFunctionName } ).



check_function_arity( FunctionArity ) when is_integer( FunctionArity )
										   andalso FunctionArity >= 0 ->
	FunctionArity;

check_function_arity( InvalidFunctionArity ) ->
	throw( { invalid_function_arity, InvalidFunctionArity } ).


check_function_name( FunctionName ) when is_atom( FunctionName ) ->
	ok;

check_function_name( InvalidFunctionName ) ->
	throw( { invalid_function_name, InvalidFunctionName } ).




% Type section.


-spec scan_type_ids( [ type_id() ], action_table() ) -> [ type_id() ].
scan_type_ids( FunIds, ActionTable ) when is_list( FunIds ) ->
	[ scan_type_id( FunId, ActionTable ) || FunId <- FunIds ];

scan_type_ids( InvalidFunIds, _ActionTable ) ->
	throw( { invalid_type_identifiers, InvalidFunIds } ).


scan_type_id( FunId={ TypeName, TypeArity }, ActionTable ) ->
	check_type_name( TypeName ),
	check_type_arity( TypeArity ),
	FunId;

scan_type_id( InvalidFunId, _ActionTable ) ->
	throw( { invalid_type_id, InvalidFunId } ).



check_type_name( TypeName ) when is_atom( TypeName ) ->
	ok;

check_type_name( InvalidTypeName ) ->
	throw( { invalid_type_name, InvalidTypeName } ).


check_type_arity( TypeArity ) when is_integer( TypeArity )
								   andalso TypeArity >= 0 ->
	TypeArity;

check_type_arity( InvalidTypeArity ) ->
	throw( { invalid_type_arity, InvalidTypeArity } ).



% Variable section.

-spec scan_variables( [ variable_ref() ], action_table() ) ->
							[ variable_ref() ].
scan_variables( VariableRefs, ActionTable ) ->
	[ scan_variable( VarRef, ActionTable ) || VarRef <- VariableRefs ];


-spec scan_variables( variable_ref(), action_table() ) -> variable_ref().
scan_variable( VarRef={ var, Line, VarName }, ActionTable )
  when is_atom( VarName ) ->
	F;

scan_variable( InvalidVarRef, _ActionTable ) ->
	throw( { invalid_variable_reference, InvalidVarRef } ).



% Record section.

-spec scan_record_defs( [ record_def() ], action_table() ) -> [ record_def() ].
scan_record_defs( RecordDefs, ActionTable ) ->
	[ scan_record_def( RecordDef, ActionTable ) || RecordDef <- RecordDefs ].



-spec scan_record_def( record_def(), action_table() ) -> record_def().
% With value and no type:
scan_record_def( { record_field, Line, Attr={ atom, AttrLine, AttrName },
				   AttrValue }, ActionTable ) ->
	ScannedAttrValue = scan_expression( AttrValue, ActionTable ),
	{ record_field, Line, Line, ScannedAttrValue };

% No value, no type:
scan_record_def( Def={ record_field, Line,
						_Attr={ atom, AttrLine, AttrName } }, ActionTable ) ->
	Def;

% With or without value and with type:
scan_record_def( { typed_record_field, RecordField, TypeDef }, ActionTable ) ->
	NewRecordField = scan_record_def( RecordField, ActionTable ),
	NewTypeDef = scan_type_def( TypeDef, ActionTable ),
	{ typed_record_field, NewRecordField, NewTypeDef }.



% Function section.


-spec scan_function( function_name(), function_arity(), [ function_clause() ],
					 action_table() ) ->
			  { function_name(), function_arity(), [ function_clause() ] }.
scan_function( Name, Arity, Clauses, ActionTable ) ->
	check_function_name( Name ),
	check_function_arity( Arity ),
	NewClauses = [ scan_function_clause( C, ActionTable ) || C <- Clauses ],
	{ Name, Arity, NewClauses }.


% Returns the function clause resulting from the processing of the input one.
%
-spec scan_function_clause( function_clause(), action_table() ) ->
								  function_clause().
scan_function_clause( { clause, Line, H0,G0,B0}, ActionTable ) ->
