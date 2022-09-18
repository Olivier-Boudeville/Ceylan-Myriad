% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Wednesday, September 14, 2022.


% @doc This module allows to generate <b>modules sharing read-only, per-topic
% two-way (bijective) associative tables whose stored pairs can be read from any
% number (potentially extremely large) of readers very efficiently</b> (possibly
% the most efficient way in Erlang), for each topic.
%
% Such modules gathering topic-based conversion tables are especially useful
% when implementing conversions between external constants (e.g. the ones of a
% graphical interface, or of a protocol) and ones defined by the current program
% (possibly as convenient atoms), moreover with a low overhead. Consider relying
% on the const_bijective_table module is a single (possibly large) table is to
% be shared.
%
% Any number of topics (e.g. 'color', 'reference', 'city') can be declared,
% whose pairs can be decided at runtime, from any source. Their elements can be
% of any permanent (non-transient) type (not atoms only; the first elements can
% be heterogeneous, they do not have to be of the same type, and the same of
% course applies for the second elements). Using a transient type is bound to
% result in a badarg. As two-way conversions are to be performed, for a given
% set of values (first ones or second ones), there must be no duplicated
% elements (otherwise no bijectivity can exist).
%
% These tables may be kept in-memory only (hence with the corresponding modules
% being generated and used at runtime) and/or be generated and stored in an
% actual BEAM file, for a later direct (re)loading thereof.
%
% No ETS table, replication (ex: per-user table copy) or message sending is
% involved: thanks to metaprogramming, a module is generated on-the-fly,
% exporting two functions designed to access either of the elements of the
% entries of interest.
%
% More precisely, a module name (ex: 'foobar'), a list of topics (atoms)
% together with their respective list of `{any(), any()}' entries must be
% provided to the `const_bijective_topics:generate*/*' functions; for any given
% topic (e.g. 'packet_type'), any element E of any pair in the resulting table
% (e.g. 'discover_packet') in `{'discover_packet', , <<42,11>>}') can then be
% accessed thanks to foobar:get_{first|second}_for_TOPIC/1 (e.g. discover_packet
% = foobar:get_first_for_packet_type(<<42,11>>)).
%
% For that, for each topic, two corresponding 1-arity functions are generated
% and exported in that module, as if we were using a bijective_table()
% underneath.
%
% No restriction applies to the elements stored (notably they may be all of
% different types), except, at the leve of each topicn the absence of duplicates
% between its first values and between its second ones.
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
% - const_bijective_topics_test.erl for an usage example and testing thereof
% - const_bijective_table.erl for a "single-topic" constant bijective table
% - bijective_table.erl for a runtime, mutable, term-based bijective table
% - const_table.erl, for a constant, "simple" (oneway) associative table
%
-module(const_bijective_topics).


-export([ generate_in_memory/2, generate_in_file/2, generate_in_file/3 ]).


% For re-used in other modules (ex: const_bijective_table:
-export([ generate_header_form/2, generate_footer_form/1, generate_forms/4 ]).


-type topic() :: atom().
% A topic name, designating a specific table shared by a generated module.
% Example of such topic names: colour, bar_identifier, font_style.


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


-type topic_spec() :: { topic(), entries() }.
% The specified of a topic: name and entries.

-export_type([ topic/0, first_type/0, second_type/0, entry/0, entries/0,
			   topic_spec/0 ]).


% Implementation notes:
%
% We suppose pattern-matched function calls (going through potentially many
% 'foobar:get_first_for(Sn) -> Fn;' clauses) to be both quicker and more compact
% / less duplicated in memory than using any table in the generated module, for
% all numbers of entries, but as long as not tested this remains an assumption.


% Shorthands:

-type module_name() :: basic_utils:module_name().

-type file_name() :: file_utils:file_name().
-type any_directory_path() :: file_utils:any_directory_path().

-type permanent_term() :: type_utils:permanent_term().

-type function_name() :: meta_utils:function_name().

-type form() :: ast_base:form().

-type line() :: ast_base:line().



% @doc Generates in memory (only) and loads a module sharing bijectively the
% specified entries for the specified topic by exporting suitably-generated
% get_first_for_TOPIC/1 and get_second_for_TOPIC/1 functions in order to access
% either element of the recorded pairs.
%
% Note that no actual module file is generated (ex: no 'foobar.beam'), the
% operation remains fully in-memory.
%
-spec generate_in_memory( module_name(), [ topic_spec() ] ) -> void().
generate_in_memory( ModuleName, TopicSpecs ) ->

	cond_utils:if_defined( myriad_debug_code_generation,
		% list_table cannot be used, as "keys" (first) are not necessarily
		% atoms:
		%
		trace_utils:debug_fmt( "Generating pseudo-module '~ts' from following "
			"topic specs:~n ~p", [ ModuleName, TopicSpecs ] ) ),

	% Just a name here, not designating any actual file:
	ModulePseudoFilename = get_generated_beam_filename_for( ModuleName ),

	Forms = generate_topic_forms( ModuleName, TopicSpecs ),

	%trace_utils:debug_fmt( "Generated forms:~n ~p", [ Forms ] ),

	% Not wanting an actual file:
	CompileOpts = [ binary | meta_utils:get_compile_base_opts() ],

	BinaryObjectCode = case compile:forms( Forms, CompileOpts ) of

		% Matches the module name:
		{ ok, ModuleName, Binary } ->
			Binary;

		Error ->
			throw( { module_generation_failed, ModuleName, Error } )

	end,

	code:load_binary( ModuleName, ModulePseudoFilename, BinaryObjectCode ).

	% Contains for example '{foobar,
	% "const_bijective_table_generated_foobar.beam"}':
	%
	%trace_utils:debug_fmt( "Loaded modules:~n~p", [ code:all_loaded() ] ),

	% We loaded this new module also, as otherwise any previous various version
	% of it would still be used instead.



% @doc Generates in-file (a BEAM file created in the current directory) a module
% sharing the specified entries by exporting suitably-generated get_first_for/1
% and get_second_for/1 functions in order to access either element of the
% recorded pairs.
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
	generate_in_file( ModuleName, Entries,
					  file_utils:get_current_directory() ).


% @doc Generates in-file (a BEAM file created in the specified directory) a
% module sharing the specified entries by exporting suitably-generated
% get_first_for/1 and get_second_for/1 functions in order to access either
% element of the recorded pairs.
%
% For a clearer setting, generated modules may be named as such
% (e.g. 'foobar_generated').
%
% The resulting module is not loaded by this function.
%
% Returns the generated filename (not path), for any further reference.
%
-spec generate_in_file( module_name(), [ topic_spec() ],
						any_directory_path() ) -> file_name().
generate_in_file( ModuleName, TopicSpecs, TargetDir ) ->

	file_utils:is_existing_directory_or_link( TargetDir ) orelse
		throw( { non_existing_output_directory, TargetDir } ),

	ModuleFilename = get_generated_beam_filename_for( ModuleName ),

	cond_utils:if_defined( myriad_debug_code_generation,
		% list_table cannot be used, as "keys" (first) are not necessarily
		% atoms:
		%
		trace_utils:debug_fmt( "Generating module '~ts' in file '~ts', in the "
			"'~ts' directory, for following topics and entries:~n ~p.",
			[ ModuleName, ModuleFilename, TargetDir, TopicSpecs ] ) ),

	Forms = generate_topic_forms( ModuleName, TopicSpecs ),

	%trace_utils:debug_fmt( "Generated forms:~n ~p", [ Forms ] ),

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



% @doc Returns the header forms corresponding to the specified module, declared
% at the specified line.
%
-spec generate_header_form( module_name(), line() ) -> form().
generate_header_form( ModuleName, Line ) ->
	{ attribute, Line, module, ModuleName }.


% @doc Returns suitable footer forms.
-spec generate_footer_form( line() ) -> form().
generate_footer_form( Line ) ->
	{ eof, Line }.



% @doc Generates the forms corresponding to the specified topic.
-spec generate_topic_forms( module_name(), [ topic_spec() ] ) -> [ form() ].
generate_topic_forms( ModuleName, TopicSpecs ) ->

	Line = 0,

	Topics = type_utils:check_atoms( [ T || { T, _S } <- TopicSpecs ] ),

	case list_utils:get_duplicates( Topics ) of

		[] ->
			ok;

		Dups ->
			throw( { duplicated_bijective_topics, Dups } )

	end,

	[ generate_header_form( ModuleName, Line ) | list_utils:flatten_once(
		[ begin

			FirstFunName = text_utils:atom_format( "get_first_for_~ts",
												   [ TP ] ),
			SecondFunName = text_utils:atom_format( "get_second_for_~ts",
													[ TP ] ),

			generate_forms( FirstFunName, SecondFunName, ET, Line )

		  end || { TP, ET } <- TopicSpecs ] ) ]
		++ [ generate_footer_form( Line ) ].



% @doc Generates the forms corresponding to the specified first/second function
% names, entries and module.
%
-spec generate_forms( function_name(), function_name(), [ entries() ],
					  line() ) -> [ form() ].
generate_forms( FirstFunName, SecondFunName, Entries, Line ) ->

	% We prefer defining get_first_for/1 then get_second_for/1, and respecting
	% the order of the specified entries; preferably ends with end of file:
	%
	% (refer to https://www.erlang.org/doc/apps/erts/absform.html)

	RevEntries = lists:reverse( Entries ),

	FirstFunForm = generate_fun_form_for_first( RevEntries, FirstFunName,
												Line ),

	SecondFunForm = generate_fun_form_for_second( RevEntries, SecondFunName,
												  Line ),

	[ FirstFunForm, SecondFunForm ].



% Generates the form corresponding to foobar:FirstFunName/1.
generate_fun_form_for_first( Entries, FirstFunName, Line ) ->

	% We have here to generate first the 'foobar:get_first_for(Sn) -> Fn;'
	% clauses:
	%
	Clauses = generate_first_clauses( Entries, Line, _Acc=[] ),

	{ function, Line, FirstFunName, _Arity=1, Clauses }.


% (helper)
generate_first_clauses( _Entries=[], _Line, Acc ) ->
	% Already reversed:
	Acc;

generate_first_clauses( _Entries=[ _E={ F, S } | T ], Line, Acc ) ->

	ASTForF = ast_utils:term_to_form( F ),
	ASTForS = ast_utils:term_to_form( S ),

	NewAcc = [ { clause, Line, _PatternSeq=[ ASTForS ], _GuardSeq=[],
				 _Body=[ ASTForF ] } | Acc ],

	generate_first_clauses( T, Line, NewAcc ).


% Generates the form corresponding to foobar:SecondFunName/1.
generate_fun_form_for_second( Entries, SecondFunName, Line ) ->

	% We have here to generate second the 'foobar:get_second_for(Sn) -> Fn;'
	% clauses:
	%
	Clauses = generate_second_clauses( Entries, Line, _Acc=[] ),

	{ function, Line, SecondFunName, _Arity=1, Clauses }.


% (helper)
generate_second_clauses( _Entries=[], _Line, Acc ) ->
	% Already reversed:
	Acc;

generate_second_clauses( _Entries=[ _E={ F, S } | T ], Line, Acc ) ->

	ASTForF = ast_utils:term_to_form( F ),
	ASTForS = ast_utils:term_to_form( S ),

	% Note the F/S swapping compared to generate_first_clauses/3:
	NewAcc = [ { clause, Line, _PatternSeq=[ ASTForF ], _GuardSeq=[],
				 _Body=[ ASTForS ] } | Acc ],

	generate_second_clauses( T, Line, NewAcc ).
