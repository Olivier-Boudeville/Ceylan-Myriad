% Copyright (C) 2022-2023 Olivier Boudeville
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
% Creation date: Sunday, September 11, 2022.


% @doc This module allows to generate <b>modules sharing read-only two-way
% (bijective) associative tables whose stored pairs can be read from any number
% (potentially extremely large) of readers very efficiently</b> (possibly the
% most efficient way in Erlang).
%
% They are especially useful in order to share a single, larger table (specific
% to the generated module), moreover with a low overhead. If multiple tables
% ("topics") are to be shared, consider relying on the modules generated by the
% const_bijective_topics module instead, offering a similar API.
%
% These pairs can be decided at runtime, from any source, and their elements can
% be of any permanent (non-transient) type (not atoms only; the first elements
% can be heterogeneous, they do not have to be of the same type, and the same of
% course applies for the second elements). Specifying a transient type will
% result in a badarg. As two-way conversions are to be performed, for a given
% set of values (first ones or second ones), there must be no duplicated
% elements (otherwise no bijectivity can exist).
%
% These tables may be kept in-memory only (hence with the corresponding modules
% being generated and used at runtime) and/or be generated and stored in an
% actual BEAM file, for a later direct (re)loading thereof.
%
% Two ways of performing element look-up are provided:
% - the 'strict' one, based on get_{first,second}_for/1 functions, for which an
% element (first or second) must be found in the table (otherwise an exception
% is thrown)
% - the 'maybe' one, which adds to the 'strict' get_{first,second}_for/1
% functions, the get_maybe_{first,second}_for/1, which return 'undefined' should
% the specified element not be found in the table
%
% (the 'maybe' one is not enabled unconditionally as it slows down very slightly
% the strict functions, whereas it may be useless)
%
% No ETS table, replication (ex: per-user table copy) or message sending is
% involved: thanks to metaprogramming (parse transform), a module is generated
% on-the-fly, exporting two functions designed to access either of the elements
% of the entries of interest.
%
% More precisely, a module name (ex: 'foobar') and a list of `{any(), any()}'
% entries must be provided to the `const_bijective_table:generate*/*' functions;
% any element E of any pair in the resulting table (e.g. 42.0 in `{'baz',
% 42.0}') can then be accessed thanks to foobar:get_{first|second}_for/1
% (e.g. baz = foobar:get_first_for(42.0)).
%
% For that these two 1-arity functions are generated and exported in that
% module, as if we were using a bijective_table() underneath.
%
% No restriction applies to the elements stored (notably they may be all of
% different types), except the absence of duplicates between first values and
% between second ones..
%
% This is presumably the most efficient way of sharing constants in Erlang.
%
% However generating a module of the same name more than once should be done
% with care, as if a given module is generated three times (hence updated/loaded
% twice), the initial module would become 'current', then 'old', and then be
% removed. Any process that would linger in it would then be terminated (see
% [http://www.erlang.org/doc/reference_manual/code_loading.html]). However, due
% to the nature of these modules (just one-shot fully-qualified calls, no
% recursion or message-waiting construct), this is not expected to happen.
%
% Refer to:
% - const_bijective_table_test.erl for an usage example and testing thereof
% - bijective_table.erl for a runtime, mutable, term-based bijective table
% - const_bijective_topics.erl, to generate modules able to share multiple
% tables (not a single one like this one)
% - const_table.erl, for a constant, "simple" (oneway) associative table
%
-module(const_bijective_table).


-export([ generate_in_memory/2, generate_in_memory/3,
		  generate_in_file/2, generate_in_file/3, generate_in_file/4 ]).


-type first_type() :: permanent_term().
% Designates the first elements of the table pairs.
% A module-based storage cannot hold transient terms.

-type second_type() :: permanent_term().
% Designates the second elements of the table pairs.
% A module-based storage cannot hold transient terms.


-type entry() :: { first_type(), second_type() }.
% An entry to be fed to a const-bijective table.

-type entries() :: [ entry() ].
% Entries that can be fed to a const-bijective table.

-export_type([ first_type/0, second_type/0, entry/0, entries/0 ]).


% Implementation notes:
%
% We suppose that pattern-matched function calls (going through potentially many
% 'foobar:get_first_for(Sn) -> Fn;' clauses) are both quicker and more compact /
% less duplicated in memory than using any table in the generated module, for
% all numbers of entries - but as long as this is not tested this remains an
% assumption.
%
% We also used to rely on const_bijective_topics:generate_forms/4 but this
% proved unappropriate, as it generates for example get_first_for_TOPIC/1
% whereas we want here to generate get_first_for/1 (note the absence of a last
% '_'); we cannot drop this '_' in const_bijective_topics, and specify '_TOPIC'
% instead of 'TOPIC', as errors shall be reported with 'TOPIC', not '_TOPIC'.



% Shorthands:

-type module_name() :: basic_utils:module_name().
-type error_type() :: basic_utils:error_type().

-type file_name() :: file_utils:file_name().
-type any_directory_path() :: file_utils:any_directory_path().

-type permanent_term() :: type_utils:permanent_term().

-type form() :: ast_base:form().

-type file_loc() :: ast_base:file_loc().

-type element_lookup() :: const_bijective_topics:element_lookup().



% @doc Generates in memory (only) and loads a module sharing bijectively the
% specified entries by exporting suitably-generated get_{first,second}_for/1
% functions ('strict' look-up) in order to access either element of the recorded
% pairs.
%
% Note that no actual module file is generated (ex: no 'foobar.beam'), the
% operation remains fully in-memory.
%
-spec generate_in_memory( module_name(), entries() ) -> void().
generate_in_memory( ModuleName, Entries ) ->
	generate_in_memory( ModuleName, Entries, _ElementLookup=strict ).



% @doc Generates in memory (only) and loads a module sharing bijectively the
% specified entries by exporting suitably-generated functions in order to access
% either element of the recorded pairs.
%
% If the 'strict' element look-up is selected, the get_{first,second}_for/1
% functions will be available.
%
% If the 'maybe' element look-up is selected, additionally to the 'strict' case
% the get_maybe_{first,second}_for/1 functions will be available. Selecting that
% 'maybe' element look-up is not recommended if either of the first and second
% sets contains the 'undefined' atom, as it leads to ambiguity.
%
% Note that no actual module file is generated (ex: no 'foobar.beam'), the
% operation remains fully in-memory.
%
-spec generate_in_memory( module_name(), entries(), element_lookup() ) ->
											void().
generate_in_memory( ModuleName, Entries, ElementLookup ) ->

	cond_utils:if_defined( myriad_debug_code_generation,
		% list_table cannot be used, as "keys" (first) are not necessarily
		% atoms:
		%
		trace_utils:debug_fmt( "Generating pseudo-module '~ts', "
			"with the '~ts' element look-up, from following entries:~n ~p",
			[ ModuleName, ElementLookup, Entries ] ) ),

	Forms = generate_table_forms( ModuleName, Entries, ElementLookup ),
	%trace_utils:debug_fmt( "Generated forms:~p", [ Forms ] ),

	% Not wanting an actual file:
	CompileOpts = [ binary | meta_utils:get_compile_base_opts() ],

	BinaryObjectCode = case compile:forms( Forms, CompileOpts ) of

		% Matches the module name:
		{ ok, ModuleName, Binary } ->
			Binary;

		Error ->
			throw( { module_generation_failed, ModuleName, Error } )

	end,

	% Just a name here, not designating any actual file:
	ModulePseudoFilename = get_generated_beam_filename_for( ModuleName ),

	code:load_binary( ModuleName, ModulePseudoFilename, BinaryObjectCode ).

	% Contains for example '{foobar,
	% "const_bijective_table_generated_foobar.beam"}':
	%
	%trace_utils:debug_fmt( "Loaded modules:~n~p", [ code:all_loaded() ] ),

	% We loaded this new module also, as otherwise any previous various version
	% of it would still be used instead.



% @doc Generates in-file (a BEAM file created in the current directory) a module
% sharing bijectively the specified entries by exporting suitably-generated
% get_{first,second}_for/1 functions ('strict' look-up) in order to access
% either element of the recorded pairs.
%
% For a clearer setting, generated modules may be named as such
% (e.g. 'foobar_generated').
%
% The resulting module is not loaded by this function.
%
% Returns the generated filename (not path), for any further reference.
%
-spec generate_in_file( module_name(), entries() ) -> file_name().
generate_in_file( ModuleName, Entries ) ->
	generate_in_file( ModuleName, Entries, _ElementLookup=strict ).



% @doc Generates in-file (a BEAM file created in the current directory) a module
% sharing bijectively the specified entries by exporting suitably-generated
% functions in order to access either element of the recorded pairs.
%
% If the 'strict' element look-up is selected, the get_{first,second}_for/1
% functions will be available.
%
% If the 'maybe' element look-up is selected, additionally to the 'strict' case
% the get_maybe_{first,second}_for/1 functions will be available. Selecting that
% 'maybe' element look-up is not recommended if either of the first and second
% sets contains the 'undefined' atom, as it leads to ambiguity.
%
% For a clearer setting, generated modules may be named as such
% (e.g. 'foobar_generated').
%
% The resulting module is not loaded by this function.
%
% Returns the generated filename (not path), for any further reference.
%
-spec generate_in_file( module_name(), entries(), element_lookup() ) ->
												file_name().
generate_in_file( ModuleName, Entries, ElementLookup ) ->
	generate_in_file( ModuleName, Entries, ElementLookup,
					  _TargetDir=file_utils:get_current_directory() ).



% @doc Generates in-file (a BEAM file created in the specified directory) a
% module sharing bijectively the specified entries by exporting
% suitably-generated functions in order to access either element of the recorded
% pairs.
%
% If the 'strict' element look-up is selected, the get_{first,second}_for/1
% functions will be available.
%
% If the 'maybe' element look-up is selected, additionally to the 'strict' case
% the get_maybe_{first,second}_for/1 functions will be available. Selecting that
% 'maybe' element look-up is not recommended if either of the first and second
% sets contains the 'undefined' atom, as it leads to ambiguity.
%
% For a clearer setting, generated modules may be named as such
% (e.g. 'foobar_generated').
%
% The resulting module is not loaded by this function.
%
% Returns the generated filename (not path), for any further reference.
%
-spec generate_in_file( module_name(), entries(), element_lookup(),
						any_directory_path() ) -> file_name().
generate_in_file( ModuleName, Entries, ElementLookup, TargetDir ) ->

	file_utils:is_existing_directory_or_link( TargetDir ) orelse
		throw( { non_existing_output_directory, TargetDir } ),

	ModuleFilename = get_generated_beam_filename_for( ModuleName ),

	cond_utils:if_defined( myriad_debug_code_generation,
		% list_table cannot be used, as "keys" (first) are not necessarily
		% atoms:
		%
		trace_utils:debug_fmt( "Generating module '~ts' in file '~ts', in the "
			"'~ts' directory, with the '~ts' element look-up, "
			"from following entries:~n ~p",
			[ ModuleName, ModuleFilename, TargetDir, ElementLookup,
			  Entries ] ) ),

	Forms = generate_table_forms( ModuleName, Entries, ElementLookup ),
	%trace_utils:debug_fmt( "Generated forms:~p", [ Forms ] ),

	CompileOpts =
		[ { outdir, TargetDir } | meta_utils:get_compile_base_opts() ],

	BinaryObjectCode = case compile:forms( Forms, CompileOpts ) of

		% Matches the module name; apparently 'binary' is implicit and thus no
		% file is written:
		%
		{ ok, ModuleName, Binary } ->
			Binary;

		Error ->
			throw( { module_generation_failed, ModuleName, Error } )

	end,

	% So we do it by ourselves:
	TargetFilePath = file_utils:join( TargetDir, ModuleFilename ),
	file_utils:write_whole( TargetFilePath, BinaryObjectCode ),

	cond_utils:if_defined( myriad_check_code_generation,
		file_utils:is_existing_file( TargetFilePath ) orelse
			throw( { no_module_file_generated, TargetFilePath } ) ),

	ModuleFilename.




% Helper section.


% @doc Returns a filename corresponding to the specified BEAM module to be
% generated.
%
-spec get_generated_beam_filename_for( module_name() ) -> file_name().
get_generated_beam_filename_for( ModName ) ->

	% Clearer, but longer, and anyway the runtime will expect ModName, not
	% another atom:
	%
	%"const_bijective_table_generated_"
	%    ++ code_utils:get_beam_filename( ModName ).

	code_utils:get_beam_filename( ModName ).



% Generates the forms corresponding to the specified entries and module.
-spec generate_table_forms( module_name(), entries(), element_lookup() ) ->
												[ form() ].
generate_table_forms( ModuleName, Entries, ElementLookup ) ->

	FileLoc = ast_utils:get_generated_code_location(),

	% We prefer defining get_first_for/1 then get_second_for/1, and respecting
	% the order of the specified entries; preferably ends with end of file:
	%
	% (refer to https://www.erlang.org/doc/apps/erts/absform.html)

	RevEntries = lists:reverse( Entries ),

	[ const_bijective_topics:generate_header_form( ModuleName, FileLoc ) |
	  generate_fun_forms( RevEntries, ElementLookup, FileLoc ) ] ++
		  [ const_bijective_topics:generate_footer_form( FileLoc ) ].



% Generates the forms corresponding to foobar:get_{first,second}_for/1 and
% possibly foobar:get_maybe_{first,second}_for/1.
%
generate_fun_forms( Entries, _ElementLookup=strict, FileLoc ) ->

	% Like const_bijective_topics:generate_forms/4:

	RevEntries = lists:reverse( Entries ),

	% We have here to generate first the 'foobar:get_first_for(Sn) -> Fn;'
	% clauses:
	%
	FirstFunForm = generate_strict_fun_form_for_first( RevEntries, FileLoc ),

	SecondFunForm = generate_strict_fun_form_for_second( RevEntries, FileLoc ),

	[ FirstFunForm, SecondFunForm ];


generate_fun_forms( Entries, _ElementLookup=maybe, FileLoc ) ->

	% Like const_bijective_topics:generate_forms/4:

	RevEntries = lists:reverse( Entries ),

	% We have here to generate first the 'foobar:get_first_for(Sn) -> Fn;'
	% clauses:
	%
	FirstFunForms = generate_maybe_fun_forms_for_first( RevEntries, FileLoc ),

	SecondFunForms = generate_maybe_fun_forms_for_second( RevEntries, FileLoc ),

	FirstFunForms ++ SecondFunForms.




% Generates the strict form corresponding to foobar:get_first_for/1.
generate_strict_fun_form_for_first( Entries, FileLoc ) ->

	% We have here to generate first the 'foobar:get_first_for(Sn) -> Fn;'
	% clauses:
	%
	Clauses = const_bijective_topics:generate_first_clauses( Entries, FileLoc,
		_Acc=[ catch_all_clause( second_not_found, _Lookup=strict,
								 FileLoc ) ] ),

	{ function, FileLoc, _FirstFunName=get_first_for, _Arity=1, Clauses }.



% Generates the strict form corresponding to foobar:get_second_for/1.
generate_strict_fun_form_for_second( Entries, FileLoc ) ->

	% We have here to generate second the 'foobar:get_second_for(Sn) -> Fn;'
	% clauses:
	%
	Clauses = const_bijective_topics:generate_second_clauses( Entries, FileLoc,
		_Acc=[ catch_all_clause( first_not_found, _Lookup=strict, FileLoc ) ] ),

	{ function, FileLoc, _SecondFunName=get_second_for, _Arity=1, Clauses }.



% Generates both forms (strict and maybe) corresponding to
% foobar:get_first_for/1 and foobar:get_maybe_first_for/1.
%
generate_maybe_fun_forms_for_first( Entries, FileLoc ) ->

	% We generate first a full maybe function, then derive its strict
	% counterpart from it (the strict one calling the maybe one):

	Arity = 1,

	MaybeFunName = get_maybe_first_for,

	MaybeClauses = const_bijective_topics:generate_first_clauses( Entries,
		FileLoc,
		_MAcc=[ catch_all_clause( second_not_found, _Lookup='maybe',
								  FileLoc ) ] ),

	MaybeFunForm = { function, FileLoc, MaybeFunName, Arity, MaybeClauses },


	StrictClauses = generate_strict_calling_clauses(
		_ErrorAtom=second_not_found, MaybeFunName, FileLoc ),

	%trace_utils:debug_fmt( "Strict clauses:~n ~p", [ StrictClauses ] ),

	StrictFunForm = { function, FileLoc, get_first_for, Arity, StrictClauses },

	[ MaybeFunForm, StrictFunForm ].




% Generates both forms (strict and maybe) corresponding to
% foobar:get_second_for/1 and foobar:get_maybe_second_for/1.
%
generate_maybe_fun_forms_for_second( Entries, FileLoc ) ->

	Arity = 1,

	MaybeFunName = get_maybe_second_for,

	MaybeClauses = const_bijective_topics:generate_second_clauses( Entries,
		FileLoc,
		_MAcc=[ catch_all_clause( first_not_found, _Lookup='maybe',
								  FileLoc ) ] ),

	MaybeFunForm = { function, FileLoc, MaybeFunName, Arity, MaybeClauses },


	StrictClauses = generate_strict_calling_clauses(
		_ErrorAtom=first_not_found, MaybeFunName, FileLoc ),

	StrictFunForm = { function, FileLoc, get_second_for, Arity, StrictClauses },

	[ MaybeFunForm, StrictFunForm ].



% Quite the same as its const_bijective_topics counterpart, except there is no
% topic.
%
% (helper)
generate_strict_calling_clauses( ErrorAtom, MaybeFunName, FileLoc ) ->

	% Corresponds to a clause:
	%  FUNC( X ) ->
	%    case MaybeFunName( X ) of
	%
	%        undefined ->
	%            throw( { ErrorAtom, X } );
	%
	%        V ->
	%            V
	%
	%    end

	XVar = {var,FileLoc,'X'},

	VVar = {var,FileLoc,'V'},

	ThrowTuple = { tuple, FileLoc, [ {atom,FileLoc,ErrorAtom}, XVar ] },

	% Single clause:
	[ { clause, FileLoc, _PatternSeq=[ XVar ], _GuardSeq=[],
		[ { 'case', FileLoc,
			{ call, FileLoc, {atom,FileLoc,MaybeFunName}, [ XVar ] },
			[ { clause, FileLoc, [{atom,FileLoc,undefined}], [],
				[ { call, FileLoc, _Fun={atom,FileLoc,throw},
				  _Args=[ ThrowTuple ] } ] },
			  { clause, FileLoc, [VVar], [],
				[ VVar ] } ] } ] } ].



% @doc Returns a catch-all clause throwing an (hopefully) informative
% {ErrorAtom, Value} exception, like {first_not_found, MyUnexpectedValue}
% (rather than a {my_generated_module, '-inlined-get_second_for/1-',
% ... function_clause).
%
% Thus results in { {nocatch, {first_not_found, MyUnexpectedValue} },
% [{my_generated_module, get_second_for,1,[]}, ...
%
-spec catch_all_clause( error_type(), element_lookup(), file_loc() ) -> form().
catch_all_clause( ErrorAtom, _Lookup=strict, FileLoc ) ->

	% Corresponds to a clause:
	%  FUNC( NotMatched ) ->
	%    throw( { ErrorAtom, NotMatched } )

	NotMatchedVar = { var, FileLoc, 'NotMatched' },

	% Not {remote, FileLoc, _Mod={atom,FileLoc,erlang}, _FunThrow...
	ThrowCall = { call, FileLoc, _Fun={atom,FileLoc,throw},
		_Args=[ { tuple, FileLoc,
					[ {atom,FileLoc,ErrorAtom}, NotMatchedVar ] } ] },

	{ clause, FileLoc, _PatternSeq=[ NotMatchedVar ], _GuardSeq=[],
		_Body=[ ThrowCall ] };


catch_all_clause( _ErrorAtom, _Lookup=maybe, FileLoc ) ->

	% Corresponds to a clause:
	%  FUNC( _NotMatched ) ->
	%    undefined.

	NotMatchedVar = { var, FileLoc, '_' },

	{ clause, FileLoc, _PatternSeq=[ NotMatchedVar ], _GuardSeq=[],
		_Body=[ {atom,FileLoc,'undefined'} ] }.
