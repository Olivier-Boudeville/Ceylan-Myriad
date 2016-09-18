#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable +A 16

% Commented out: -pa ../utils


% Copyright (C) 2016-2016 Olivier Boudeville (olivier.boudeville@esperide.com)

% Released as LGPL software.


% The vast majority of the code has been moved to a real Erlang module
% (merge_utils) that can be run separately from this escript (see the run, scan
% and merge make targets); otherwise the direct debugging from an escript was
% awful.


% Entry point of the script.
%
main( [ "-h" ] ) ->
	io:format( "~s", [ get_usage() ] );

main( [ "--help" ] ) ->
	io:format( "~s", [ get_usage() ] );

% Here we scan a tree:
main( [ "--scan", TreePath ] ) ->
	merge_utils:scan( TreePath );

% Here we merge the (supposedly more up-to-date) source tree into the target
% one:
%
main( [ SourceTreePath, TargetTreePath ] ) ->
	merge_utils:merge( SourceTreePath, TargetTreePath );

main( _ ) ->
	io:format( "~n   Error, exactly two parameters should be specified.~n~n~s",
			   [ get_usage() ] ).


% Verbatim section.


% Copied verbatim from common/src/utils/script_utils.erl:

% Updates the VM code path so that all modules of the 'Common' layer can be
% readily used.
%
% Note: this function and its helpers might be copied verbatim to the target
% escript so that it can really be used from anywhere (not only from the
% directory it is stored).
%
-spec update_code_path_for_common() -> basic_utils:void().
update_code_path_for_common() ->

	CommonRootDir = get_root_of_common(),

	CommonSrcDir = filename:join( CommonRootDir, "src" ),

	CommonBeamSubDirs = [ "utils", "user-interface", "maths",
						  "data-management" ],

	CommonBeamDirs = [ filename:join( CommonSrcDir, D )
					   || D <- CommonBeamSubDirs ],

	%io:format( "'Common' beam dirs: ~p~n", [ CommonBeamDirs ] ),

	ok = code:add_pathsa( CommonBeamDirs ).



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



% Returns the root directory of the Common layer.
%
-spec get_root_of_common() -> file_utils:path().
get_root_of_common() ->
	filename:join( [ get_script_base_directory(), "..", ".." ] ).
