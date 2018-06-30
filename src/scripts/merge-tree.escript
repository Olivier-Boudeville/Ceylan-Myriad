#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable +A 16

% Commented out: -pa ../utils


% Copyright (C) 2016-2018 Olivier Boudeville
% [olivier (dot) boudeville (at) esperide (dot) com]


% Released as LGPL software.

% Directly using the module-based version now, in merge_utils.erl, for an easier
% debugging (ex: with proper stack traces, comprising line numbers).

% This script depends on the 'Myriad' layer, and only on that code (that shall
% be recompiled beforehand).

% For testing, one shall prefer using merge_utils:run/0.



% Entry point of this escript.
%
main( ArgList ) ->

	% First, enable all possible helper code (hence to be done first of all):
	update_code_path_for_myriad(),

	ArgTable = script_utils:get_arguments( ArgList ),
	merge_utils:main( ArgTable ).




% Verbatim section.


% Copied verbatim from Ceylan-Myriad/src/utils/script_utils.erl, for bootstrap:


% Returns the base directory of that script, i.e. where it is stored (regardless
% of the possibly relative path whence it was launched).
%
% Note: useful to locate resources (ex: other modules) defined with that script
% and needed by it.
%
-spec get_script_base_directory() -> file_utils:path().
get_script_base_directory() ->

	% filename:absname/1 could be used instead:
	FullPath = case escript:script_name() of

		ScriptPath=( "/" ++ _ ) ->
			% Is already absolute here:
			ScriptPath;

		RelativePath ->
			% Let's make it absolute then:
			{ ok, CurrentDir } = file:get_cwd(),
			 filename:join( CurrentDir, RelativePath )

	end,

	filename:dirname( FullPath ).


% Returns the root directory of the Myriad layer.
%
% (note that a double path conversion between root and script directories can
% hardly be avoided)
%
-spec get_myriad_base_directory() -> file_utils:path().
get_myriad_base_directory() ->

	% We cannot use file_utils:normalise_path/1 here: Myriad not usable from
	% that point yet!
	%
	filename:join( [ get_script_base_directory(), "..", ".." ] ).



% Updates the VM code path so that all modules of the 'Myriad' layer can be
% readily used from an escript.
%
% Note: this function and its helpers might be copied verbatim to the target
% escript so that it can really be used from anywhere (not only from the
% directory it is stored).
%
% (original version located in script_utils.erl, copied verbatim here)
%
-spec update_code_path_for_myriad() -> basic_utils:void().
update_code_path_for_myriad() ->

	MyriadRootDir = get_myriad_base_directory(),

	%trace_utils:debug_fmt( "Root of 'Myriad': ~s.", [ MyriadRootDir ] ),

	MyriadSrcDir = filename:join( MyriadRootDir, "src" ),

	MyriadBeamSubDirs = [ "data-management", "maths", "meta",
						  "user-interface/src", "user-interface/src/textual",
						  "user-interface/src/graphical", "utils" ],

	MyriadBeamDirs = [ filename:join( MyriadSrcDir, D )
					   || D <- MyriadBeamSubDirs ],

	%trace_utils:debug_fmt( "'Myriad' beam dirs: ~p.", [ MyriadBeamDirs ] ),

	ok = code:add_pathsa( MyriadBeamDirs ).
