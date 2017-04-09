% Copyright (C) 2016-2017 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
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
% Creation date: Wednesday, October 24, 2012.


% Gathering helper for the development and use of Erlang scripts (escripts).
%
% Intended use: add, in the script directory, a symbolic link to this module so
% that the script can readily call it and thus bootstrap the use of all others.
%
% This module should be called only from the context of an escript, not from a
% normal Erlang program.
%
-module(script_utils).


% Implementation notes:
%
% The code path is not supposed to be updated with the one for 'Common', so
% extra care must be tken not to call Common helper modules for implementations
% here meant to be run before the update of the code path.

-export([ update_code_path_for_common/0 ]).


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

	%io:format( "'Common' beam dirs: ~s~n", [ CommonBeamDirs ] ),

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
