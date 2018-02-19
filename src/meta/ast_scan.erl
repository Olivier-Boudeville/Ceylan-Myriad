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



% Module in charge of scanning an AST, a prerequisite notably for later
% transformations (see ast_transform.erl for that).
%
-module(ast_scan).


% For the table macro for example:
-include("meta_utils.hrl").


% For the module_info record:
-include("ast_info.hrl").



% Name of any parse attribute:
%
% (typically in Form={ attribute, Line, AttributeName, AttributeValue })
%
-type parse_attribute_name() :: atom().


-export_type([ parse_attribute_name/0 ]).


-export([ scan/1, check_parse_attribute_name/1, check_parse_attribute_name/2 ]).



% Shorthands:

-type ast() :: ast_base:ast().
-type form_context() :: ast_base:form_context().
-type module_info() :: ast_info:module_info().


%-type function_name() :: meta_utils:function_name().
%-type function_arity() :: meta_utils:function_arity().
%-type function_id() :: meta_utils:function_id().
%-type function_clause() :: ast_base:form().




% Implementation notes:
%
% The integral scanning of ASTs got inspiration from erl_id_trans.erl.
%
% Numbered sections refer to the ones in
% http://erlang.org/doc/apps/erts/absform.html, with additional subsections for
% each bullet.
%
% Note: we used to traverse the AST recursively, "blindly", i.e. without
% expecting the intended structure of AST elements (ex: the structure of a tuple
% corresponding to a 'receive' statement).
%
% Then, for a more complete control, we match the AST against its intended,
% "official" structure, as defined in
% http://erlang.org/doc/apps/erts/absform.html; however, for a mere scan, this
% is mostly useless, a more basic traversal is sufficient, and moreover
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
% We try to order clauses roughly in decreasing average frequency of appearance
% in actual code.




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
	% So iterating in the list of forms that make for the AST:
	%
	scan_forms( AST, InitModuleInfo,
				_NextLocation=id_utils:get_initial_sortable_id(),
				_CurrentFileRef=undefined ).


%%
%% First part: top-level forms.
%%



% Main scanning function.
%
% Here all relevant parts of the specified AST (located forms) are matched in
% turn, and stored in the specified module_info once located using
% ast_base:form_location/0 identifiers, which allow easy insertions and
% reordering.
%
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
%  - 7.1.15 : Enf of file

% Extra Section 7.9: Catch-all for unexpected forms



%%
%% Section 7.1: Module Declarations and Forms.
%%



% (7.1.1: Module declaration, already done)

% 7.1.2: Function export handling.
%
% "If F is an attribute -export([Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) =
% {attribute,LINE,export,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}."
%
scan_forms( _AST=[ _Form={ 'attribute', Line, 'export', FunctionIds } | T ],
			M=#module_info{ function_exports=ExportTable,
							functions=FunctionTable },
			NextLocation, CurrentFileReference ) ->

	%ast_utils:display_debug( "function export declaration for ~p",
	% [ FunctionIds ] ),

	Context = { CurrentFileReference, Line },

	% Records this export regarding each of these functions:
	NewFunctionTable = lists:foldl(

		fun( FunId, FunTableAcc ) ->

			{ Name, Arity } = ast_function:check_function_id( FunId, Context ),

			NewFunInfo = case ?table:lookupEntry( FunId, FunTableAcc ) of

				 key_not_found ->

					% New entry then:
					#function_info{ name=Name,
									arity=Arity,
									% Implicit:
									%location=undefined
									%line=undefined
									%clauses=[],
									%spec=undefined,
									%callback=undefined,
									exported=[ NextLocation ] };

				 % A function *might* be exported more than once:
				 { value, FunInfo } -> % F=#function_info{ exported=[] } } ->
					% Just add the fact that the function is exported then:
					NewExp = [ NextLocation | FunInfo#function_info.exported ],
					FunInfo#function_info{ exported=NewExp }

			end,

			?table:addEntry( FunId, NewFunInfo, FunTableAcc )

		end,
		_Acc0=FunctionTable,
		_List=FunctionIds ),

	% At least initially, exactly one export entry per location:
	NewExportTable = ?table:addNewEntry( NextLocation, { Line, FunctionIds },
										 ExportTable ),

	% And then recurses with the next forms:
	scan_forms( T, M#module_info{ function_exports=NewExportTable,
								  functions=NewFunctionTable },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.3: Function import handling.
%
% "If F is an attribute -import(Mod,[Fun_1/A_1, ..., Fun_k/A_k]), then Rep(F) =
% {attribute,LINE,import,{Mod,[{Fun_1,A_1}, ..., {Fun_k,A_k}]}}."
%
scan_forms( _AST=[ Form={ 'attribute', Line, 'import',
							{ ModuleName, FunIds } } | T ],
			 M=#module_info{ function_imports=ImportTable,
							 function_imports_defs=ImportDefs },
			NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line },

	ast_utils:check_module_name( ModuleName, Context ),
	ast_function:check_function_ids( FunIds, Context ),

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
scan_forms( _AST=[ Form={ 'attribute', Line, 'module', ModuleName } | T ],
			 M=#module_info{ module=undefined, module_def=undefined },
			 NextLocation, CurrentFileReference ) ->

	%ast_utils:display_debug( "module declaration for ~s", [ ModuleName ] ),

	Context = { CurrentFileReference, Line },

	ast_utils:check_module_name( ModuleName, Context ),

	% Note: when processing X.beam, we should not remove the lines like:
	% {attribute,37,file,{"X.erl",37}} as they allow to report errors
	% appropriately.

	LocForm = { NextLocation, Form },

	scan_forms( T, M#module_info{ module=ModuleName, module_def=LocForm },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );


% Any lacking, invalid or duplicated module declaration will be caught by
% the compiler anyway.
%
scan_forms( _AST=[ _Form={ 'attribute', Line, 'module', ModuleName } | _T ],
			 #module_info{ module=PreviousModuleName },
			 _NextLocation, CurrentFileReference ) ->
	throw( { multiple_module_definitions, PreviousModuleName, ModuleName,
			 { CurrentFileReference, Line } } );



% 7.1.5: File reference (include) handling.
%
% "If F is an attribute -file(File,Line), then Rep(F) =
% {attribute,LINE,file,{File,Line}}."
%
% Allows to keep track of when an included file begins and also ends, i.e. to
% determine the current file to which any new line number corresponds.
%
scan_forms( _AST=[ Form={ 'attribute', _Line, 'file',
						  { FilePath, _FileLine } } | T ],
			 M=#module_info{ includes=Inc, include_defs=IncDefs }, NextLocation,
			_CurrentFileReference ) ->

	%ast_utils:display_debug( "file declaration with ~s at #~B",
	% [ FilePath, FileLine ] ),

	% We used to normalise paths, however then 'file_utils' would have to be
	% bootstrapped as well, which does not seem desirable.

	%NewCurrentFileReference = text_utils:string_to_binary(
	%     file_utils:normalise_path( Filepath ) ),
	NewCurrentFileReference = text_utils:string_to_binary( FilePath ),

	% Avoids duplicates (in 'includes' only, not in definitions):
	%
	NewFilePaths = case lists:member( NewCurrentFileReference, Inc ) of

		true ->
			Inc;

		false ->
			[ NewCurrentFileReference | Inc ]

	end,

	LocForm = { NextLocation, Form },

	scan_forms( T, M#module_info{ includes=NewFilePaths,
								  include_defs=[ LocForm | IncDefs ] },
				id_utils:get_next_sortable_id( NextLocation ),
				NewCurrentFileReference );




% 7.1.6: Function definition handling.
%
% "If F is a function declaration Name Fc_1 ; ... ; Name Fc_k, where each Fc_i
% is a function clause with a pattern sequence of the same length Arity, then
% Rep(F) = {function,LINE,Name,Arity,[Rep(Fc_1), ...,Rep(Fc_k)]}."
%
scan_forms( [ { 'function', Line, FunctionName, FunctionArity, Clauses } | T ],
			M=#module_info{ functions=FunctionTable }, NextLocation,
			CurrentFileReference ) ->

	%ast_utils:display_debug( "function definition for ~p/~p",
	% [ FunctionName, FunctionArity ] ),

	% The non-first clauses could be checked as well:
	%
	% (when adding a function, we may not check if ever there was a pre-existing
	% one - multiple definitions will be rejected by the compiler anyway)

	Context = { CurrentFileReference, Line },

	ast_function:check_function_name( FunctionName, Context ),
	ast_utils:check_arity( FunctionArity, Context ),
	ast_clause:check_function_clauses( Clauses, FunctionArity, Context ),

	FunId = { FunctionName, FunctionArity },

	FunInfo = case ?table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->
			% New entry then:
			#function_info{ name=FunctionName,
							arity=FunctionArity,
							location=NextLocation,
							line=Line,
							clauses=Clauses
							% Implicit:
							%spec=undefined,
							%callback=undefined,
							%exported=[]
						  };

		{ value, F=#function_info{ clauses=[] } } ->
				% Already here because of an export; just add the missing
				% information then:
				F#function_info{ location=NextLocation,
								 line=Line,
								 clauses=Clauses };

		% Here a definition was already set:
		_ ->
			ast_utils:raise_error( [ multiple_definition_for, FunId ], Context )

	end,

	NewFunctionTable = ?table:addEntry( _K=FunId, _V=FunInfo, FunctionTable ),

	%ast_utils:display_debug( "function ~s/~B with ~B clauses registered.",
	%			   [ FunctionName, FunctionArity, length( Clauses ) ] ),

	scan_forms( T, M#module_info{ functions=NewFunctionTable },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.7: Local function type specification handling (including callbacks).
%
% "If F is a function specification -Spec Name Ft_1; ...; Ft_k, where Spec is
% either the atom spec or the atom callback, and each Ft_i is a possibly
% constrained function type with an argument sequence of the same length Arity,
% then Rep(F) = {attribute,Line,Spec,{{Name,Arity},[Rep(Ft_1), ...,
% Rep(Ft_k)]}}."
%
scan_forms( [ Form={ 'attribute', Line, SpecType,
					 { FunId, FunctionTypes } } | T ],
			M=#module_info{ functions=FunctionTable },
			NextLocation, CurrentFileReference )
  when SpecType == 'spec' orelse SpecType == 'callback' ->

	Context = { CurrentFileReference, Line },

	{ FunctionName, FunctionArity } = ast_function:check_function_id(
										FunId, Context ),

	ast_function:check_function_types( FunctionTypes, FunctionArity, Context ),

	%ast_utils:display_debug( "~s definition for ~p/~p",
	% [ SpecType, FunctionName, FunctionArity ] ),

	LocatedSpec = { NextLocation, Form },

	IsCallback = case SpecType of

		callback ->
			true;

		spec ->
			false

	end,

	FunInfo = case ?table:lookupEntry( FunId, FunctionTable ) of

		key_not_found ->

			% New entry then:
			#function_info{ name=FunctionName,
							arity=FunctionArity,
							% Implicit:
							%location=undefined,
							%line=undefined,
							%clauses=[]
							spec=LocatedSpec,
							callback=IsCallback };

		{ value, F=#function_info{ spec=undefined,
								   callback=undefined } } ->
			% Just add that spec form and callback status then:
			F#function_info{ spec=LocatedSpec,
							 callback=IsCallback };

		% Here a spec was already set:
		_ ->
			ast_utils:raise_error( [ multiple_spec_for, FunId ], Context )

	end,

	NewFunctionTable = ?table:addEntry( _K=FunId, _V=FunInfo, FunctionTable ),

	%ast_utils:display_debug( "spec for function ~s/~B registered.",
	%		   [ FunctionName, FunctionArity ] ),

	scan_forms( T, M#module_info{ functions=NewFunctionTable },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );


% (optional callbacks, not specified in the spec yet known of the id parse
% transform)
%
scan_forms( [ Form={ 'attribute', Line, _AttributeName='optional_callbacks',
					 _AttributeValue=FunIds } | T ],
			M=#module_info{ optional_callbacks_defs=LocatedDefs },
			NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line },

	% Surprisingly, in erl_id_trans.erl, the corresponding check may fail (as is
	% in a try/catch clause), and in this case is replaced by its original value.
	%
	ast_function:check_function_ids( FunIds, Context ),

	LocForm = { NextLocation, Form },

	scan_forms( T, M#module_info{
					 optional_callbacks_defs=[ LocForm | LocatedDefs ] },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );


% (asm attribute, not specified in the spec yet known of the id parse
% transform; checked and then treated as any wild parse attribute)
%
scan_forms( [ Form={ 'attribute', _Line, AttributeName='asm',
					 Def={ 'function', _N, _A, _Code } } | T ],
			  M=#module_info{ parse_attributes=ParseAttributeTable,
							  parse_attribute_defs=AttributeDefs },
			NextLocation, CurrentFileReference ) ->

	%ast_utils:display_debug( "Asm attribute definition: '~p'.",
	%						 [ Def ] ),

	LocForm = { NextLocation, Form },

	scan_forms( T, M#module_info{
				   parse_attributes=?table:addEntry( AttributeName,
							_AttributeValue=Def, ParseAttributeTable ),
				   parse_attribute_defs=[ LocForm | AttributeDefs ] },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );


% 7.1.8: Remote function type specification handling.
%
% "If F is a function specification -spec Mod:Name Ft_1; ...; Ft_k, where each
% Ft_i is a possibly constrained function type with an argument sequence of the
% same length Arity, then Rep(F) =
% {attribute,Line,spec,{{Mod,Name,Arity},[Rep(Ft_1), ..., Rep(Ft_k)]}}."
%
scan_forms( [ Form={ 'attribute', Line, 'spec',
					 _MFA={ ModuleName, FunctionName, FunctionArity },
					 FunctionTypes } | T ],
			M=#module_info{ remote_spec_defs=RemoteSpecDefs },
			NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line },

	FunId = { FunctionName, FunctionArity },

	ast_utils:check_module_name( ModuleName, Context ),
	ast_function:check_function_id( FunId, Context ),
	ast_function:check_function_types( FunctionTypes, FunctionArity, Context ),

	%ast_utils:display_debug( "remote spec definition for ~p/~p",
	% [ FunctionName, FunctionArity ] ),

	% Specs for remote functions not specifically processed.

	LocatedSpec = { NextLocation, Form },

	NewRemoteSpecDefs = [ LocatedSpec | RemoteSpecDefs ],

	%ast_utils:display_debug( "remote spec for function ~s/~B registered.",
	%		   [ FunctionName, FunctionArity ] ),

	scan_forms( T, M#module_info{ remote_spec_defs=NewRemoteSpecDefs },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.9: Record definition handling.
%
% "If F is a record declaration -record(Name,{V_1, ..., V_k}), where each V_i is
% a record field, then Rep(F) = {attribute,LINE,record,{Name,[Rep(V_1), ...,
% Rep(V_k)]}}. For Rep(V), see below.
%
scan_forms( _AST=[ _Form={ 'attribute', Line, 'record',
						   { RecordName, DescFields } } | T ],
			M=#module_info{ records=RecordTable },
			NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line },

	ast_utils:check_record_name( RecordName, Context ),

	FieldTable = scan_field_descriptions( DescFields, CurrentFileReference ),

	NewRecordDef = { FieldTable, NextLocation, Line },

	NewRecordTable = ?table:addNewEntry( RecordName, NewRecordDef, RecordTable ),

	scan_forms( T, M#module_info{ records=NewRecordTable },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.10: Type definition handling.
%
% "If F is a type declaration -Type Name(V_1, ..., V_k) :: T, where Type is
% either the atom type or the atom opaque, each V_i is a variable, and T is a
% type, then Rep(F) = {attribute,LINE,Type,{Name,Rep(T),[Rep(V_1), ...,
% Rep(V_k)]}}."
%
scan_forms( _AST=[ _Form={ 'attribute', Line, TypeDesignator,
						   { TypeName, TypeDef, TypeVariables } } | T ],
			 M=#module_info{ types=TypeTable },
			 NextLocation, CurrentFileReference )
  when TypeDesignator == 'type' orelse TypeDesignator == 'opaque' ->

	%ast_utils:display_debug( "type declaration for ~p: ~p", [
	%                        TypeName, Form ] ),

	Context = { CurrentFileReference, Line },

	ast_type:check_type_name( TypeName, Context ),
	ast_type:check_type_definition( TypeDef, Context ),
	ast_type:check_type_variables( TypeVariables, Context ),

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
			%ast_utils:display_debug( "New type '~s' defined as:~n~p",
			%						 [ TypeName, TypeDef ] ),
			#type_info{ name=TypeName,
						variables=TypeVariables,
						opaque=IsOpaque,
						location=NextLocation,
						line=Line,
						definition=TypeDef
						%exported=[]
					  }

	end,

	NewTypeTable = ?table:addEntry( TypeId, NewTypeInfo, TypeTable ),

	scan_forms( T, M#module_info{ types=NewTypeTable },
				id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );


% 7.1.11: Other, "wild" parse attributes: this section comes later, so that it
% matches only if none of the other attribute-related ones (such as for
% 'export_type') matched.


% 7.1.12: Type export handling [lacking in reference page].
%
% Supposedly:
%
% "If F is an attribute -export_type([Type_1/A_1, ..., Type_k/A_k]), then Rep(F)
% = {attribute,LINE,export_type,[{Type_1,A_1}, ..., {Type_k,A_k}]}."
%
scan_forms( _AST=[ _Form={ 'attribute', Line, 'export_type', TypeIds } | T ],
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
					%
					#type_info{ name=Name,
								% Implicit:
								%variables=undefined,
								%opaque=undefined
								%location=undefined
								%line=undefined
								%definition=[],
								exported=[ NextLocation ] };

				 % A type *might* be exported more than once:
				 { value, TypeInfo } -> % F=#type_info{ exported=[] } } ->
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
scan_forms( _AST=[ Form={ 'attribute', _Line, 'compile', 'inline' } | T ],
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
scan_forms( _AST=[ Form={ 'attribute', Line, 'compile',
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


% Non-inlining compile options:
scan_forms( _AST=[ Form={ 'attribute', _Line, 'compile',
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
scan_forms( [ Form={ 'attribute', Line, AttributeName, AttributeValue } | T ],
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
scan_forms( _AST=[ _Form={ 'error',
	   { Line, 'epp', { 'include', 'file', FileName } } } | _T ], _ModuleInfo,
			 _NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line-1 },

	ast_utils:raise_error( [ include_file_not_found, FileName ], Context );


% eep undefined macro variable error:
scan_forms( _AST=[ _Form={ 'error',
	   { Line, 'epp', { 'undefined', VariableName, 'none' } } } | _T ], _ModuleInfo,
			 _NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line-1 },

	ast_utils:raise_error( [ undefined_macro_variable, VariableName ],
						   Context );


% eep general errors:
scan_forms( _AST=[ _Form={ 'error', { Line, 'epp', Reason } } | _T ], _ModuleInfo,
			 _NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line-1 },

	ast_utils:raise_error( [ preprocessing_failed, Reason ], Context );


% Parser (erl_parse) errors:
scan_forms( _AST=[ _Form={ 'error', { Line, 'erl_parse', Reason } } | _T ],
			 _ModuleInfo, _NextLocation, CurrentFileReference ) ->

	Context = { CurrentFileReference, Line-1 },

	ast_utils:raise_error( [ parsing_failed, io_lib:format( Reason, [] ) ],
						   Context );


% Any kind of other error:
scan_forms( _AST=[ _Form={ 'error', ErrorTerm } | _T ],
			 _ModuleInfo, _NextLocation, CurrentFileReference ) ->

	ast_utils:raise_error( [ scan_error, ErrorTerm],
						   _Context=CurrentFileReference );


% Any kind of warning:
scan_forms( _AST=[ _Form={ 'warning', WarningTerm } | T ],
			ModuleInfo, NextLocation, CurrentFileReference ) ->

	ast_utils:notify_warning( [ scan_warning, WarningTerm ],
							  _Context=CurrentFileReference ),

	scan_forms( T, ModuleInfo, id_utils:get_next_sortable_id( NextLocation ),
				CurrentFileReference );



% 7.1.15 : End of file handling.

% We expect the module name to be known when ending the processing (so that we
% can remove its corresponding file from the includes):
%
scan_forms( _AST=[ _Form={ 'eof', Line } ],
			Infos=#module_info{ module=undefined }, _NextLocation,
			CurrentFileReference ) ->
	Context = { CurrentFileReference, Line },
	ast_utils:raise_error( [ eof_while_no_module, Infos ], Context );


% Form expected to be defined once, and to be the last one:
scan_forms( _AST=[ Form={ 'eof', Line } ],
			M=#module_info{ last_line=undefined, module=Module, includes=Inc },
			_NextLocation, _CurrentFileReference ) ->

	ast_utils:display_debug( "eof declaration at ~p.", [ Line ] ),

	% Surely not wanting anything to be able to go past it:
	LocForm = { id_utils:get_sortable_id_upper_bound(), Form },

	% End of file found, doing some housekeeping.

	% We just do not want to have the filename of the currently processed module
	% among the includes:

	% Reconstructs the supposedly deduced module filename:
	BinModFilename = text_utils:string_to_binary(
					atom_to_list( Module ) ++ ".erl" ),

	% Due to the removal of include duplicates, can be listed only up to once:
	NoModInc = lists:delete( BinModFilename, Inc ),

	% Only "normal", non-recursing exit of that longer function:
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


% The scan termination is expected to happen *only* when eof is found:
scan_forms( _AST=[], _ModuleInfo, _NextLocation, CurrentFileReference ) ->
	ast_utils:raise_error( [ no_eof_found ], CurrentFileReference ).





% Processes the fields of a given record definition.
%
% Note: field names could be full expressions here, but only atoms are allowed
% by the parser (dixit the id parse transform).
%
-spec scan_field_descriptions( [ ast_base:ast_element() ],
					   ast_base:file_reference() ) -> meta_utils:field_table().
scan_field_descriptions( FieldDescriptions, CurrentFileReference ) ->

	FieldTable = ?table:new(),

	scan_field_descriptions( FieldDescriptions, CurrentFileReference,
							 FieldTable ).


% (helper)
%
scan_field_descriptions( _FieldDescriptions=[], _CurrentFileReference,
						 FieldTable ) ->
	FieldTable;

% Here no type or default value are specified for that field:
%
scan_field_descriptions( _FieldDescriptions=[
		{ 'record_field', _Line1, { atom, _Line2, FieldName } } | T ],
		CurrentFileReference, FieldTable ) ->

	NewFieldTable = ?table:addNewEntry( FieldName,
		  { _FieldType=undefined, _DefaultValue=undefined }, FieldTable ),
	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );

% Here only a type is specified for that field:
scan_field_descriptions( _FieldDescriptions=[
	   { 'typed_record_field',
		  { 'record_field', _Line1, { atom, _Line2, FieldName } }, FieldType }
												| T ],
						 CurrentFileReference, FieldTable ) ->
	NewFieldTable = ?table:addNewEntry( FieldName,
					  { FieldType, _DefaultValue=undefined }, FieldTable ),
	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );

% Here only a default value is specified for that field:
scan_field_descriptions( _FieldDescriptions=[
	   { 'record_field', _Line1, { atom, _Line2, FieldName }, DefaultValue }
												| T ],
						 CurrentFileReference, FieldTable ) ->
	NewFieldTable = ?table:addNewEntry( FieldName,
		  { _FieldType=undefined, DefaultValue }, FieldTable ),
	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );

% Here a type and a default, immediate value are specified for that field:
scan_field_descriptions( _FieldDescriptions=[
	   { 'typed_record_field',
		  { 'record_field', _Line1,
		   { atom, _Line2, FieldName }, DefaultValue }, FieldType }
												| T ],
						 CurrentFileReference, FieldTable ) ->
	NewFieldTable = ?table:addNewEntry( FieldName, { FieldType, DefaultValue },
										FieldTable ),
	scan_field_descriptions( T, CurrentFileReference, NewFieldTable );


scan_field_descriptions( _FieldDescriptions=[ UnexpectedDesc | _T ],
						 CurrentFileReference, _FieldTable ) ->
	ast_utils:raise_error( [ unexpected_field_description, UnexpectedDesc ],
						   CurrentFileReference ).




% Checks that specified parse attribute name is legit.
%
-spec check_parse_attribute_name( term(), form_context() ) ->
										parse_attribute_name().
check_parse_attribute_name( Name, _Context ) when is_atom( Name ) ->
	Name;

check_parse_attribute_name( Other, Context ) ->
	ast_utils:raise_error( [ invalid_parse_attribute_name, Other ], Context ).


% Checks that specified parse attribute name is legit.
%
-spec check_parse_attribute_name( term() ) ->
										basic_utils:parse_attribute_name().
check_parse_attribute_name( Name ) ->
	check_parse_attribute_name( Name, _Context=undefined ).
