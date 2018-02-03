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


% For the table macro for example:
-include("meta_utils.hrl").


% For the module_info record:
-include("ast_info.hrl").


% For the ast_transforms record:
-include("ast_scan.hrl").



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

-export_type([ ast/0, action_table/0 ]).


-export([ scan/1, scan_type/3, scan_expression/2, scan_term/4 ]).


% Shorthands:

-type ast() :: ast_utils:ast().

-type module_info() :: meta_utils:module_info().

%-type function_name() :: meta_utils:function_name().
%-type function_arity() :: meta_utils:function_arity().
%-type function_id() :: meta_utils:function_id().
%-type function_clause() :: ast_utils:form().



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

	InitModuleInfo = ast_info:init_module_info(),

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
scan_forms( _AST=[ _Form={ attribute, Line, module, ModuleName } | _T ],
			 #module_info{ module=PreviousModuleName },
			 _NextLocation, CurrentFileReference ) ->
	throw( { multiple_module_definitions, PreviousModuleName, ModuleName,
			 { CurrentFileReference, Line } } );



% 7.1.5: File reference (include) handling.
%
% "If F is an attribute -file(File,Line), then Rep(F) =
% {attribute,LINE,file,{File,Line}}."
%
% Allows to keep track of when an included file begins and also ends.
%
scan_forms( _AST=[ Form={ attribute, _Line, file,
						  { FilePath, _FileLine } } | T ],
			 M=#module_info{ includes=Inc, include_defs=IncDefs }, NextLocation,
			_CurrentFileReference ) ->

	%ast_utils:display_debug( "file declaration with ~s at #~B",
	% [ FilePath, FileLine ] ),

	% We used to normalise paths, however then 'file_utils' would have to be
	% bootstrapped as well, which does not seem desirable.

	%NormFilepath = text_utils:string_to_binary(
	%     file_utils:normalise_path( Filepath ) ),
	NormFilePath = text_utils:string_to_binary( FilePath ),

	% Avoids duplicates (in 'includes' only, not in definitions):
	%
	NewFilePaths = case lists:member( NormFilePath, Inc ) of

		true ->
			Inc;

		false ->
			[ NormFilePath | Inc ]

	end,

	LocForm = { NextLocation, Form },

	scan_forms( T, M#module_info{ includes=NewFilePaths,
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
			ast_utils:raise_error( [ multiple_definition_for, FunId ],
								   Context )

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
			ast_utils:raise_error( [ multiple_spec_for, FunId ], Context )

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

	FunId = { FunctionName, FunctionArity },

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

	scan_forms( T, M#module_info{ records=NewRecordTable,
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
scan_forms( _AST=[ _Form={ attribute, Line, TypeDesignator,
						   { TypeName, TypeDef, TypeVariables } } | T ],
			 M=#module_info{ types=TypeTable },
			 NextLocation, CurrentFileReference )
  when TypeDesignator == type orelse TypeDesignator == opaque ->

	%ast_utils:display_debug( "type declaration for ~p: ~p", [
	%                        TypeName, Form ] ),

	Context = { CurrentFileReference, Line },

	ast_utils:check_type_name( TypeName, Context ),
	ast_utils:check_type_definition( TypeDef, Context ),
	ast_utils:check_variables( TypeVariables, Context ),

	IsOpaque = case TypeDesignator of

		type ->
			false;

		_Opaque ->
			true

	end,

	TypeArity = length( TypeVariables ),

	TypeId = { TypeName, TypeArity },

	NewTypeInfo = case ?table:lookupEntry( TypeId, TypeTable ) of

		% If a TypeInfo is found for that type name, it must be only because it
		% has already been exported:

		{ value, #type_info{ exported=[] } } ->
			ast_utils:raise_error( [ multiple_definitions_for_type, TypeId ],
								   Context );

		{ value, ExportTypeInfo } ->
			ExportTypeInfo#type_info{ name=TypeName,
									  variables=TypeVariables,
									  opaque=IsOpaque,
									  location=NextLocation,
									  line=Line,
									  definition=TypeDef
									  %exported: already set
											   };

		% Usual case:
		key_not_found ->
			ast_utils:display_debug( "New type '~s' defined as:~n~p",
								   [ TypeName, TypeDef ] ),
			#type_info{ name=TypeName,
						variables=TypeVariables,
						opaque=IsOpaque,
						location=NextLocation,
						line=Line,
						definition=TypeDef
						%exported
					  }

	end,

	NewTypeTable = ?table:addEntry( TypeId, NewTypeInfo, TypeTable ),

	scan_forms( T, M#module_info{ types=NewTypeTable },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );


% 7.1.11: Other, "wild" parse attributes: this section comes later, so that it
% matches only if none other attribute-related one (such as for 'export_type')
% matched.


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

	%ast_utils:display_debug( "Type export declaration for ~p", [ TypeIds ] ),

	Context = { CurrentFileReference, Line },

	% Records for each of these types this export:
	NewTypeTable = lists:foldl(

		fun( TypeId, TypeTableAcc ) ->

			{ Name, _Arity } = ast_utils:check_type_id( TypeId, Context ),

			NewTypeInfo = case ?table:lookupEntry( TypeId, TypeTableAcc ) of

				 key_not_found ->

					% New entry then (arity known, but not yet the detailed
					% variables):
					#type_info{ name=Name,
								% Implicit:
								%variables=undefined,
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
scan_forms( _AST=[ Form={ attribute, _Line, compile, inline } | T ],
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
scan_forms( _AST=[ Form={ attribute, Line, compile,
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


% 7.1.11: Other, "wild" parse attributes.
%
% (section body is here, to match iff none of the other attribute-related
% sections matched)
%
scan_forms( [ Form={ attribute, Line, AttributeName, AttributeValue } | T ],
			M=#module_info{ parse_attributes=ParseAttributeTable,
							parse_attribute_defs=AttributeDefs },
			NextLocation, CurrentFileReference ) ->

	%ast_utils:display_debug( "Parse attribute definition for '~p'.",
	%						 [ AttributeName ] ),

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
			_NextLocation, _CurrentFileReference ) ->

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

	ast_utils:display_error( "Unhandled form '~p' not managed.~n",
							 [ UnhandledForm ] ),

	%throw( { unhandled_form, UnhandledForm, { location, NextLocation },
	%		 { file, CurrentFileReference } } ).

	LocForm = { NextLocation, UnhandledForm },

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





% Scanning types: traversing them recursively according to their specified
% structure.
%
% Currently not going for a fully specialised, strict and 'just sufficient'
% traversal as permitted by http://erlang.org/doc/apps/erts/absform.html; yet
% still getting inspiration from its section 7.7.
%
% We currently consider that all type definitions correspond to an
% ast_utils:ast_type(), i.e. one of:
%
% - ast_utils:ast_builtin_type(): { type, Line, TypeName, TypeVars },
% where TypeVars are often (not always) a list; ex: {type,LINE,union,[Rep(T_1),
% ..., Rep(T_k)]} or {type,LINE,map,any}; we manage specifically the most common
% type designators, and traverse generically the others
%
% - ast_utils:ast_remote_type(): { remote_type, Line, [ ModuleType, TypeName,
% TypeVars ] }
%
% - ast_utils:ast_user_type(): { user_type, Line, TypeName, TypeVars }
%
%
% Notes:
%
% - clauses ordered according to the first atom (all plain types, then all
% remote types, then all user types)
%
% - records like #type, #user_type, could be used instead
%
% (helper)
%
-spec scan_type( ast_utils:ast_type(),
		basic_utils:maybe( meta_utils:local_type_transform_table() ),
		basic_utils:maybe( meta_utils:remote_type_transform_table() ) ) ->
						   ast_utils:ast_type().

% Handling tuples:

% Fully-qualified tuple type found, ex:
% {type,42,tuple,[{type,42,integer,[]},{type,42,float,[]}]}
%
% "If T is a tuple type {T_1, ..., T_k}, then
% Rep(T) = {type,LINE,tuple,[Rep(T_1), ..., Rep(T_k)]}."
%
scan_type( _TypeDef={ type, Line, tuple, ElementTypes }, LocalTransformTable,
		   RemoteTransformTable ) when is_list( ElementTypes ) ->
	{ type, Line, tuple,
	  [ scan_type( Elem, LocalTransformTable, RemoteTransformTable )
		|| Elem <- ElementTypes ] };


% General tuple type found (i.e. tuple()):
%
% "If T is a tuple type tuple(), then Rep(T) = {type,LINE,tuple,any}."
%
scan_type( TypeDef={ type, _Line, tuple, any }, _LocalTransformTable,
		   _RemoteTransformTable ) ->
	TypeDef;

scan_type( TypeDef={ type, _Line, tuple, _Any }, _LocalTransformTable,
		   _RemoteTransformTable ) ->
	ast_utils:raise_error( { unexpected_typedef_tuple_form, TypeDef } );



% Handling lists:


% Fully-qualified list type found, ex:
% {type,43,list,[{type,43,boolean,[]}]}
%
% Lacking specification in the doc, extrapolated to:
%
% "If T is a list of elements of type A, then Rep(T) = {type,LINE,list,Rep(A)}."
%
scan_type( _TypeDef={ type, Line, list, [ ElementType ] },
		   LocalTransformTable, RemoteTransformTable ) ->

	NewElementType = scan_type( ElementType, LocalTransformTable,
								RemoteTransformTable ),

	{ type, Line, list, [ NewElementType ] };


% General list type found (i.e. list()):
%
% Lacking specification in the doc, extrapolated to:
%
% "If T is a list type list(), then Rep(T) = {type,LINE,list,any}."
%
scan_type( TypeDef={ type, _Line, list, any }, _LocalTransformTable,
		   _RemoteTransformTable ) ->
	TypeDef;


% Yes, at least in some cases list() may be translated as {type,LINE,list,[]}:
scan_type( TypeDef={ type, _Line, list, [] }, _LocalTransformTable,
		   _RemoteTransformTable ) ->
	TypeDef;


scan_type( TypeDef={ type, _Line, list, _Any }, _LocalTransformTable,
		   _RemoteTransformTable ) ->
	ast_utils:raise_error( { unexpected_typedef_list_form, TypeDef } );


% Empty list type found (i.e. []):
%
% "If T is the empty list type [], then Rep(T) = {type,Line,nil,[]}"
%
scan_type( TypeDef={ type, _Line, nil, [] }, _LocalTransformTable,
			   _RemoteTransformTable ) ->
	TypeDef;



% Handling binaries:

% "If T is a bitstring type <<_:M,_:_*N>>, where M and N are singleton integer
% types, then Rep(T) = {type,LINE,binary,[Rep(M),Rep(N)]}."
%
scan_type( TypeDef={ type, _Line, binary, [ M, N ] }, _LocalTransformTable,
			   _RemoteTransformTable ) ->

	% To be removed once ever seen displayed:
	ast_utils:display_warning( "Not transforming binary elements ~p and ~p.",
							   [ M, N ] ),
	TypeDef;


% "If T is an integer range type L .. H, where L and H are singleton integer
% types, then Rep(T) = {type,LINE,range,[Rep(L),Rep(H)]}."
%
scan_type( TypeDef={ type, _Line, range, [ L, H ] }, _LocalTransformTable,
			   _RemoteTransformTable ) ->

	% To be removed once ever seen displayed:
	ast_utils:display_warning( "Not transforming range bound ~p and ~p.",
							   [ L, H ] ),
	TypeDef;


% Handling maps:

% "If T is a map type map(), then Rep(T) = {type,LINE,map,any}."
%
scan_type( TypeDef={ type, _Line, map, any }, _LocalTransformTable,
			   _RemoteTransformTable ) ->
	TypeDef;


% "If T is a map type #{A_1, ..., A_k}, where each A_i is an association type,
% then Rep(T) = {type,LINE,map,[Rep(A_1), ..., Rep(A_k)]}."
%
scan_type( _TypeDef={ type, Line, map, AssocTypes }, LocalTransformTable,
			   RemoteTransformTable ) ->

	NewAssocTypes = [ scan_association_type( T, LocalTransformTable,
						RemoteTransformTable ) || T <- AssocTypes ],

	{ type, Line, map, NewAssocTypes };



% Handling lambda functions:


% "If T is a fun type fun(), then Rep(T) = {type,LINE,'fun',[]}."
scan_type( TypeDef={ type, _Line, 'fun', [] }, _LocalTransformTable,
			   _RemoteTransformTable ) ->
	TypeDef;


% "If T is a fun type fun((...) -> T_0), then Rep(T) =
% {type,LINE,'fun',[{type,LINE,any},Rep(T_0)]}."
%
scan_type( _TypeDef={ type, Line1, 'fun', [ Any={ type, _Line2, any },
											 ResultType ] },
						 LocalTransformTable, RemoteTransformTable ) ->

	NewResultType = scan_type( ResultType, LocalTransformTable,
								   RemoteTransformTable ),

	{ type, Line1, 'fun', [ Any, NewResultType ] };



% Handling union types:
%
% "If T is a type union T_1 | ... | T_k, then Rep(T) =
% {type,LINE,union,[Rep(T_1), ..., Rep(T_k)]}."
%
scan_type( _TypeDef={ type, Line, union, UnifiedTypes },
			   LocalTransformTable, RemoteTransformTable ) ->

	NewUnifiedTypes = [ scan_type( T, LocalTransformTable,
									   RemoteTransformTable )
						|| T <- UnifiedTypes ],

	{ type, Line, union, NewUnifiedTypes };


% Simple built-in type, like 'boolean()', translating in '{ type, 57, boolean,
% [] }':
%
scan_type( TypeDef={ type, _Line, BuiltinType, _TypeVars=[] },
			   _LocalTransformTable, _RemoteTransformTable ) ->

	case lists:member( BuiltinType, type_utils:get_simple_builtin_types() ) of

		true ->
			TypeDef;

		false ->
			ast_utils:display_warning( "Not expecting type '~s', assuming "
									   "simple builtin type (in ~p).",
									   [ BuiltinType, TypeDef ] ),
			TypeDef

	end;


% Like '-type my_record() :: #my_record{}.', translating in { type, 89, record,
% [ {atom, 89, my_record } ] }:
%
scan_type( _TypeDef={ type, Line, record, _TypeVars=[ ElementType ] },
		   LocalTransformTable, RemoteTransformTable ) ->

	NewElementType = scan_type( ElementType, LocalTransformTable,
								RemoteTransformTable ),

	{ type, Line, record, [ NewElementType ] };


% Known other built-in types (catch-all for all remaining 'type'):
%
%
scan_type( TypeDef={ type, Line, BuiltinType, TypeVars },
		   LocalTransformTable, RemoteTransformTable )
  when is_list( TypeVars ) ->

	ast_utils:display_warning( "Not expecting type '~s', assuming unknown "
							   "parametrized builtin type (in ~p).",
							   [ BuiltinType, TypeDef ] ),

	NewTypeVars = [ scan_type( T, LocalTransformTable,
								   RemoteTransformTable ) || T <- TypeVars ],

	{ type, Line, BuiltinType, NewTypeVars };




% Handling user type (necessary a local one):


scan_type( _TypeDef={ user_type, Line, TypeName, TypeVars },
			   LocalTransformTable, RemoteTransformTable ) ->

	TypeArity = length( TypeVars ),

	NewTypeVars = [ scan_type( T, LocalTransformTable,
								   RemoteTransformTable ) || T <- TypeVars ],

	% Note: no user-to-local type rewriting deemed useful.

	Outcome = case LocalTransformTable of

		undefined ->
			unchanged;

		_ ->

			% Returning the new type information:
			case ?table:lookupEntry( { TypeName, TypeArity },
									  LocalTransformTable ) of

				% Module *and* type overridden:
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
											 LocalTransformTable ) of

						{ value, E={ _NewModuleName, _NewTypeName } } ->
							E;

						% Same type, only module overridden:
						% (was commented-out out, but may happen?)
						%
						{ value, NewModuleName }
						  when is_atom( NewModuleName ) ->
							{ NewModuleName, TypeName };

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
			% TypeDef with only updated TypeVars:
			{ user_type, Line, TypeName, NewTypeVars };

		{ SetModuleName, SetTypeName } ->
			ast_utils:forge_remote_type( SetModuleName, SetTypeName,
										 NewTypeVars, Line )

	end;



% Handling remote user type:


% "If T is a remote type M:N(T_1, ..., T_k), then Rep(T) =
% {remote_type,LINE,[Rep(M),Rep(N),[Rep(T_1), ..., Rep(T_k)]]}."
%
% First, the special (yet most common) case of immediate values specified for
% module and type:
%
scan_type( _TypeDef={ remote_type, Line1,
						 [ M={ atom, Line2, ModuleName },
						   T={ atom, Line3, TypeName }, TypeVars ] },
			   LocalTransformTable, RemoteTransformTable ) ->

	NewTypeVars = [ scan_type( TypeVar, LocalTransformTable,
							   RemoteTransformTable ) || TypeVar <- TypeVars ],

	TypeArity = length( TypeVars ),

	% Returning the new type information:
	Outcome = case RemoteTransformTable of

		undefined ->
			unchanged;

		_ ->

			case ?table:lookupEntry( { ModuleName, TypeName, TypeArity },
									 RemoteTransformTable ) of

				 % Module *and* type overridden:
				{ value, E={ _NewModuleName, _NewTypeName } } ->
					E;

				 % Same type; only the module is overridden:
				{ value, NewModuleName } when is_atom( NewModuleName ) ->
					{ NewModuleName, TypeName };

				{ value, TransformFun } when is_function( TransformFun ) ->
					TransformFun( ModuleName, TypeName, TypeArity );

				key_not_found ->

					% Maybe a wildcard arity was defined for that type then?

					AnyArity = '_',

					case ?table:lookupEntry( { ModuleName, TypeName, AnyArity },
											 RemoteTransformTable ) of

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

							% Nope; maybe a wildcard type (and arity) then?
							case ?table:lookupEntry( { ModuleName, _AnyType='_',
										 AnyArity }, RemoteTransformTable ) of

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


% Second, the case where at least either the module or the type name is not
% immediate:
%
scan_type( _TypeDef={ remote_type, Line1, [ Mod, Typ, TypeVars ] },
			   LocalTransformTable, RemoteTransformTable ) ->

	% Wondering what these could be:
	ast_utils:display_debug( "Scanning a remote type whose module and type "
							 "information are ~p and ~p.", [ Mod, Typ ] ),

	[ NewMod, NewTyp ] = [ scan_type( T, LocalTransformTable,
							   RemoteTransformTable ) || T <- [ Mod, Typ ] ],

	NewTypeVars = [ scan_type( T, LocalTransformTable,
								   RemoteTransformTable ) || T <- TypeVars ],

	{ remote_type, Line1, [ NewMod, NewTyp, NewTypeVars ] };


% Variable declaration, possibly obtained through declarations like:
% -type my_type( T ) :: other_type( T ).
% or:
% -opaque tree( T ) :: { T, [ tree(T) ] }.
scan_type( TypeDef={ var, _Line, _TypeName }, _LocalTransformTable,
		   _RemoteTransformTable ) ->
	TypeDef;


% Annotated type, most probably obtained from the field of a record like:
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
scan_type( _TypeDef={ ann_type, Line, [ Var, InternalTypeDef ] },
		   LocalTransformTable, RemoteTransformTable ) ->

	[ NewVar, NewInternalTypeDef ] = [ scan_type( T, LocalTransformTable,
			  RemoteTransformTable ) || T <- [ Var, InternalTypeDef ] ],

	{ ann_type, Line, [ NewVar, NewInternalTypeDef ] };


% Immediate values like {atom,42,foobar}, possibly obtained through
% declarations like: -type my_type() :: integer() | 'foobar'.
%
scan_type( TypeDef={ TypeName, _Line, _Value }, _LocalTransformTable,
		   _RemoteTransformTable ) ->
	case lists:member( TypeName, type_utils:get_immediate_types() ) of

		true ->
			TypeDef;

		false ->
			ast_utils:raise_error( { unexpected_immediate_value, TypeDef } )

	end;


scan_type( TypeDef, _LocalTransformTable, _RemoteTransformTable ) ->
	ast_utils:raise_error( { unhandled_typedef, TypeDef } ).







% Scanning association types (from maps).


% "If A is an association type K => V, where K and V are types, then Rep(A) =
% {type,LINE,map_field_assoc,[Rep(K),Rep(V)]}."
%
-spec scan_association_type( ast_utils:ast_type(),
		basic_utils:maybe( meta_utils:local_type_transform_table() ),
		basic_utils:maybe( meta_utils:remote_type_transform_table() ) ) ->
						   ast_utils:ast_type().
scan_association_type( { type, Line, map_field_assoc, Types=[ _K, _V ] },
						   LocalTransformTable, RemoteTransformTable ) ->
	NewTypes = [ scan_type( T, LocalTransformTable, RemoteTransformTable )
				 || T <- Types ],
	{ type, Line, map_field_assoc, NewTypes };


% "If A is an association type K := V, where K and V are types, then Rep(A) =
% {type,LINE,map_field_exact,[Rep(K),Rep(V)]}.
%
scan_association_type( { type, Line, map_field_exact, Types=[ _K, _V ] },
						   LocalTransformTable, RemoteTransformTable ) ->
	NewTypes = [ scan_type( T, LocalTransformTable, RemoteTransformTable )
				 || T <- Types ],
	{ type, Line, map_field_exact, NewTypes }.




% Processes the fields of a given record.
%
-spec scan_field_descriptions( [ ast_utils:ast_element() ],
							   file_reference() ) -> meta_utils:field_table().
scan_field_descriptions( FieldDescriptions, CurrentFileReference ) ->

	FieldTable = ?table:new(),

	scan_field_descriptions( FieldDescriptions, CurrentFileReference,
							 FieldTable ).


scan_field_descriptions( _FieldDescriptions=[], _CurrentFileReference,
						 FieldTable ) ->
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
scan_expression( E={ 'case', Line, TestExpression, Clauses },
					 Replacements ) ->

	ast_utils:display_debug( "Intercepting case expression ~p...", [ E ] ),

	NewTestExpression = scan_expression( TestExpression, Replacements ),

	NewClauses = [ scan_expression( C, Replacements ) || C <- Clauses ],

	Res = { 'case', Line, NewTestExpression, NewClauses },

	ast_utils:display_debug( "... returning case expression ~p", [ Res ] ),
	Res;



% Remote call found, with an immediate name for both the module and the
% function:
%
scan_expression( E={ call, Line1, { remote, _Line2,
			_M={ atom, _Line3, ModuleName }, _F={ atom, Line4, FunctionName } },
			Params }, Replacements ) ->

	ast_utils:display_debug( "Intercepting remote call ~p...", [ E ] ),

	Arity = length( Params ),

	% First recurses:
	NewParams = [ scan_expression( Param, Replacements )
				  || Param <- Params ],

	Outcome = case Replacements#ast_transforms.remote_calls of

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
			ast_utils:display_debug( "... returning remote call (case R1) ~p",
									 [ Res ] ),
			Res;

		{ SetModuleName, SetFunctionName } ->
			Res = ast_utils:forge_remote_call( SetModuleName, SetFunctionName,
											   NewParams, Line1, Line4 ),
			ast_utils:display_debug( "... returning remote call (case R2) ~p",
									 [ Res ] ),
			Res

	end;



% Here, at least one name (module and/or function) is not immediate:
%
% (note: we do not manage yet the case where for example the function name
% results from an expression yet a wildcard has been defined for it)
%
scan_expression( _E={ call, Line1,
						  { remote, Line2, ModuleExpr, FunctionExpr }, Params },
					 Replacements ) ->

	NewModuleExpr = scan_expression( ModuleExpr, Replacements ),

	NewFunctionExpr = scan_expression( FunctionExpr, Replacements ),

	NewParams = [ scan_expression( Param, Replacements )
				  || Param <- Params ],

	% Cannot use ast_utils:forge_remote_call, we have not atoms:
	%
	Res = { call, Line1, { remote, Line2, NewModuleExpr, NewFunctionExpr },
			NewParams },

	ast_utils:display_debug( "... returning remote call (case R3) ~p",
							 [ Res ] ),

	Res;



% Local call found:
scan_expression( E={ call, Line1, F={ atom, Line2, FunName }, Params },
					 Replacements ) ->

	ast_utils:display_debug( "Intercepting local call ~p...", [ E ] ),

	Arity = length( Params ),

	% First recurses:
	NewParams = [ scan_expression( Param, Replacements )
				  || Param <- Params ],

	Outcome = case Replacements#ast_transforms.local_calls of

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
			ast_utils:display_debug( "... returning local call ~p", [ Res ] ),
			Res;

		{ SetModuleName, SetFunctionName } ->
			Res = ast_utils:forge_remote_call( SetModuleName, SetFunctionName,
											   NewParams, Line1, Line2 ),
			ast_utils:display_debug( "... returning remote call ~p", [ Res ] ),
			Res

	end;



% Match expression found:
scan_expression( E={ match, Line, LeftExpr, RightExpr }, Replacements ) ->

	ast_utils:display_debug( "Intercepting match expression ~p...", [ E ] ),

	NewLeftExpr = scan_expression( LeftExpr, Replacements ),

	NewRightExpr = scan_expression( RightExpr, Replacements ),

	Res = { match, Line, NewLeftExpr, NewRightExpr },

	ast_utils:display_debug( "... returning match expression ~p", [ Res ] ),

	Res;

% Receive expression found:
scan_expression( E={ 'receive', Line, Clauses }, Replacements ) ->

	ast_utils:display_debug( "Intercepting receive expression ~p...", [ E ] ),

	NewClauses = [ scan_expression( C, Replacements ) || C <- Clauses ],

	Res = { 'receive', Line, NewClauses },

	ast_utils:display_debug( "... returning receive expression ~p", [ Res ] ),

	Res;


% Clause (belonging to an expression such as top-level function clause, or
% 'case', 'receive' clauses, etc.) found:
%
scan_expression( Clause={ clause, Line, ValueExpr, Guards, ResultExpr },
					  Replacements ) ->

	ast_utils:display_debug( "Intercepting clause ~p...", [ Clause ] ),

	% Rather complete, out of safety:

	NewValueExpr = [ scan_expression( E, Replacements ) || E <- ValueExpr ],

	% Guard example: {call,102, {atom,102,is_integer}, [{var,102,'X'}]}
	NewGuards = scan_expression( Guards, Replacements ),

	NewResultExpr = scan_expression( ResultExpr, Replacements ),

	Res = { clause, Line, NewValueExpr, NewGuards, NewResultExpr },

	ast_utils:display_debug( "... returning clause ~p", [ Res ] ),

	Res;


% List of expressions found:
%
% (note: this clause may be removed in the future, once all AST elements will
% have been specifically intercepted by a dedicated clause, and when the nature
% of their elements will be established and thus traversed specifically, rather
% than opening the possibility that each element may be a list)
%
scan_expression( ExprList, Replacements ) when is_list( ExprList ) ->

	ast_utils:display_debug( "Intercepting expression list ~p...",
							 [ ExprList ] ),

	NewExprList = [ scan_expression( E, Replacements ) || E <- ExprList ],

	ast_utils:display_debug( "... returning expression list ~p",
							 [ NewExprList ] ),

	NewExprList;



% Other expression found:
scan_expression( E, _Replacements ) ->
	ast_utils:display_debug( "Letting expression ~p as is.", [ E ] ),
	E.






% In this section, a term is traversed generically: as opposed to the scans
% above, no assumption is made about the underlying structure of the term to
% scan.


% Scans "blindly" (i.e. with no a-priori knowledge about its strucuture) the
% specified arbitrary term (possibly with nested subterms, as the function
% recurses in lists and tuples), calling specified transformer function on each
% instance of the specified type, in order to replace that instance by the
% result of that function.
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
-spec scan_term( term(), type_utils:primitive_type_description(),
				 meta_utils:term_transformer(), basic_utils:user_data() ) ->
						   { term(), basic_utils:user_data() }.

% Here the term is a list and this is the type we want to intercept:
scan_term( TargetTerm, _TypeDescription=list, TermTransformer, UserData )
  when is_list( TargetTerm ) ->

	{ TransformedTerm, NewUserData } = TermTransformer( TargetTerm, UserData ),

	scan_transformed_term( TransformedTerm, _TypeDescription=list,
						   TermTransformer, NewUserData );


% Here the term is a list and we are not interested in them:
scan_term( TargetTerm, TypeDescription, TermTransformer, UserData )
  when is_list( TargetTerm ) ->

	scan_list( TargetTerm, TypeDescription, TermTransformer, UserData );


% Here the term is a tuple (or a record...), and we want to intercept them:
scan_term( TargetTerm, TypeDescription, TermTransformer, UserData )
  when is_tuple( TargetTerm )
	andalso ( TypeDescription =:= tuple orelse TypeDescription =:= record ) ->

	{ TransformedTerm, NewUserData } = TermTransformer( TargetTerm, UserData ),

	scan_transformed_term( TransformedTerm, TypeDescription,
						   TermTransformer, NewUserData );


% Here the term is a tuple (or a record...), and we are not interested in them:
scan_term( TargetTerm, TypeDescription, TermTransformer, UserData )
  when is_tuple( TargetTerm ) ->

	scan_tuple( TargetTerm, TypeDescription, TermTransformer, UserData );


% Base case (current term is not a binding structure, it is a leaf of the
% underlying syntax tree):
%
scan_term( TargetTerm, TypeDescription, TermTransformer, UserData ) ->

	case type_utils:get_type_of( TargetTerm ) of

		TypeDescription ->
			TermTransformer( TargetTerm, UserData );

		_ ->
			% Unchanged:
			{ TargetTerm, UserData }

	end.



% Helper to traverse a list.
%
scan_list( TargetList, TypeDescription, TermTransformer, UserData ) ->

	{ NewList, NewUserData } = lists:foldl(
								 fun( Elem, { AccList, AccData } ) ->

			{ TransformedElem, UpdatedData } = scan_term( Elem,
							TypeDescription, TermTransformer, AccData ),

			% New accumulator, produces a reversed element list:
			{ [ TransformedElem | AccList ], UpdatedData }

								 end,

								 _Acc0={ _Elems=[], UserData },

								 TargetList ),

	{ lists:reverse( NewList ), NewUserData }.



% Helper to traverse a tuple.
%
scan_tuple( TargetTuple, TypeDescription, TermTransformer, UserData ) ->

	% We do exactly as with lists:
	TermAsList = tuple_to_list( TargetTuple ),

	{ NewList, NewUserData } = scan_list( TermAsList, TypeDescription,
										  TermTransformer, UserData ),

	{ list_to_tuple( NewList ), NewUserData }.



% Helper to traverse a transformed term (ex: if looking for a { user_id, String
% } pair, we must recurse in nested tuples like: { 3, { user_id, "Hello" }, 1 }.
%
scan_transformed_term( TargetTerm, TypeDescription, TermTransformer,
					   UserData ) ->

	case TermTransformer( TargetTerm, UserData ) of

		{ TransformedTerm, NewUserData } when is_list( TransformedTerm ) ->
			scan_list( TransformedTerm, TypeDescription, TermTransformer,
					   NewUserData );

		{ TransformedTerm, NewUserData } when is_tuple( TransformedTerm ) ->
			scan_tuple( TransformedTerm, TypeDescription, TermTransformer,
						NewUserData );

		% { ImmediateTerm, NewUserData } ->
		Other ->
			Other

	end.
