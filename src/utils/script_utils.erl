% Copyright (C) 2016-2018 Olivier Boudeville
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
% Creation date: Wednesday, October 24, 2012.



% Gathering helper for the development and use of all kinds of scripts (Erlang
% escripts and shell scripts alike).
%
% Intended use for escripts: add, in the script directory, a symbolic link to
% this module so that the script can readily call it and thus bootstrap the use
% of all others.
%
-module(script_utils).


% Implementation notes:
%
% The code path is not supposed to be updated with the one for 'Myriad', so
% extra care must be taken not to call Myriad helper modules for implementations
% here meant to be run before the update of the code path.

-export([ is_running_as_escript/0, update_code_path_for_myriad/0,
		  get_script_base_directory/0, get_myriad_base_directory/0,
		  get_arguments/1 ]).


% Tells whether the currently running Erlang code is executed as an escript or
% as a regular Erlang program.
%
-spec is_running_as_escript() -> boolean().
is_running_as_escript() ->

	% escript:script_name/0 only meant to succeed from an escript:

	try

		case escript:script_name() of

			_Any ->
				true

		end

			% typically {badmatch,[]} from escript.erl:
			catch error:_Error ->
					false

	end.



% Note: see also src/scripts/myriad_script_include.hrl for an include directly
% comprising the services below (hence with no need for a verbatim copy of
% them).


% Updates the VM code path so that all modules of the 'Myriad' layer can be
% readily used from an escript.
%
% Note: this function and its helpers might be copied verbatim to the target
% escript so that it can really be used from anywhere (not only from the
% directory it is stored).
%
% (original version located in script_utils.erl)
%
-spec update_code_path_for_myriad() -> void().
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



% Returns the base directory of that script, i.e. where it is stored (regardless
% of the possibly relative path whence it was launched).
%
% Note: useful to locate resources (ex: other modules) defined with that script
% and needed by it.
%
-spec get_script_base_directory() -> file_utils:path().
get_script_base_directory() ->

	case is_running_as_escript() of

		true ->

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

			filename:dirname( FullPath );


		false ->
			CodePath = code_utils:get_code_path(),

			MyriadPath = get_myriad_path_from( CodePath ),

			% We cannot use file_utils:normalise_path/1 here: Myriad not usable
			% from that point yet!
			%
			file_utils:join( [ MyriadPath, "src", "scripts" ] )

	end.



% (helper)
%
get_myriad_path_from( _Paths=[] ) ->
	throw( unable_to_determine_myriad_root );

get_myriad_path_from( [ Path | T ] ) ->

	LayerName = "Ceylan-Myriad",

	case string:split( Path, LayerName ) of

		[ Prefix, _Suffix ] ->
			file_utils:join( Prefix, LayerName );

		% Layer name not found:
		_ ->
			get_myriad_path_from( T )

	end.



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




% Returns the specified arguments (simply a list of the corresponding strings,
% typically obtained by the main/1 function of an escript) in the same
% "canonical" form, similar to the one used by init:get_arguments/1 (not
% available for escripts), for which options start with a start, may have any
% number of arguments, and may be specified more than once in the command-line.
%
% Allows to write code that can be seamlessly triggered by a erl interpreter or
% by an escript, by putting them in the latter case in this "canonical" form.
%
-spec get_arguments( [ string() ] ) -> executable_utils:argument_table().
get_arguments( Args ) ->
	get_arguments( Args, _OptionTable=list_table:new() ).


% (helper)
get_arguments( _Args=[], OptionTable ) ->
	OptionTable;

% The first option is detected, removing its initial dash:
get_arguments( _Args=[ [ $- | Option ] | T ], OptionTable ) ->
	manage_option( Option, _RemainingArgs=T, OptionTable );

% Here an initial argument does not start with a dash, hence is dropped, like
% done by init:get_arguments/0:
%
get_arguments( _Args=[ Dropped | T ], OptionTable ) ->

	trace_utils:warning_fmt( "Dropping non-option initial argument '~s'.",
							 [ Dropped ] ),

	get_arguments( T, OptionTable ).



% (helper)
manage_option( Option, RemainingArgs, OptionTable ) ->

	OptionAtom = text_utils:string_to_atom( Option ),

	{ OptValues, NextOptionInfo } = collect_values_for_option( RemainingArgs,
															_AccValues=[] ),

	% This option may already be registered in the table:
	NewOptionTable = list_table:appendListToEntry( _K=OptionAtom, OptValues,
												   OptionTable ),

	case NextOptionInfo of

		none ->
			NewOptionTable;

		{ NextOption, NextArgs } ->
			manage_option( NextOption, NextArgs, NewOptionTable )

	end.



% (helper)
% All arguments processed here:
collect_values_for_option( _Args=[], AccValues ) ->
	{ lists:reverse( AccValues ), _NextOption=none };

% New option detected:
collect_values_for_option( _Args=[ [ $- | Option ] | T ], AccValues ) ->
	{ lists:reverse( AccValues ), _NextOption={ Option, T } };

% Still accumulating arguments for the current option:
collect_values_for_option( _Args=[ OptValue | T ], AccValues ) ->
	collect_values_for_option( T, [ OptValue | AccValues ] ).
