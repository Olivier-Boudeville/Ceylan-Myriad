% Copyright (C) 2016-2018 Olivier Boudeville
%
% Include file meant to simplify the writing of Myriad-using escripts.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Released as LGPL software.



% To silence unused warnings:
-export([ get_script_base_directory/0,
		  get_myriad_base_directory/0,
		  update_code_path_for_myriad/0 ]).


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
	update_code_path_for_myriad( get_myriad_base_directory() ).



% Updates the VM code path so that all modules of the 'Myriad' layer can be
% readily used from a module run as an escript.
%
-spec update_code_path_for_myriad_from_module() -> basic_utils:void().
update_code_path_for_myriad_from_module() ->
	{ ok, CurrentDir } = file:get_cwd(),
	MyriadBaseDirectory = filename:join( [ CurrentDir, "..", ".." ] ),
	update_code_path_for_myriad( MyriadBaseDirectory ).


% Updates the VM code path so that all modules of the 'Myriad' layer can be
% readily used from an escript.
%
% Note: this function and its helpers might be copied verbatim to the target
% escript so that it can really be used from anywhere (not only from the
% directory it is stored).
%
% (original version located in script_utils.erl, copied verbatim here)
%
-spec update_code_path_for_myriad( file_utils:directory_name() ) ->
										 basic_utils:void().
update_code_path_for_myriad( MyriadRootDir ) ->

	%trace_utils:debug_fmt( "Root of 'Myriad': ~s.", [ MyriadRootDir ] ),

	MyriadSrcDir = filename:join( MyriadRootDir, "src" ),

	MyriadBeamSubDirs = [ "data-management", "maths", "meta",
						  "user-interface/src", "user-interface/src/textual",
						  "user-interface/src/graphical", "utils" ],

	MyriadBeamDirs = [ filename:join( MyriadSrcDir, D )
					   || D <- MyriadBeamSubDirs ],

	%trace_utils:debug_fmt( "'Myriad' beam dirs: ~p.", [ MyriadBeamDirs ] ),

	ok = code:add_pathsa( MyriadBeamDirs ).
