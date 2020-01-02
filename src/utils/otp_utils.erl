% Copyright (C) 2019-2020 Olivier Boudeville
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
% Creation date: Monday, July 15, 2019.



% Various helpers for OTP applications, releases, etc.
-module(otp_utils).


% Name of an OTP application:
-type application_name() :: atom().

% The PID of an OTP supervisor:
-type supervisor_pid() :: pid().


-export_type([ application_name/0, supervisor_pid/0 ]).


-export([ prepare_app_context/2, prepare_myriad_context/1 ]).


% Prepares a relevant context for specified OTP application: ensures that its
% .app file can be found, from the specified parent of its root directory.
%
-spec prepare_app_context( application_name(), file_utils:directory_path() ) ->
								 void().
prepare_app_context( AppName,AppParentRootDir  ) ->

	EBinPath = file_utils:join( [ AppParentRootDir, "_build", "default", "lib",
				  text_utils:atom_to_string( AppName ), "ebin" ] ),

	case file_utils:is_existing_directory_or_link( EBinPath ) of

		true ->
			ok;

		false ->
			trace_utils:error_fmt( "The ebin path of application '~s' could "
								   "not  be found (searched for '~s'); "
								   "run 'make rebar3-compile' from the root "
								   "to generate it.", [ AppName, EBinPath ] ),
			throw( { no_app_ebin_path, AppName, AppParentRootDir } )

	end,

	% So that .app file can be found:
	code_utils:declare_beam_directory( EBinPath ).



% Prepares a relevant context for the Myriad OTP application: ensures that the
% myriad.app file can be found, from the specified parent of its root directory.
%
-spec prepare_myriad_context( file_utils:directory_path() ) -> void().
prepare_myriad_context( AppParentRootDir ) ->
	prepare_app_context( _AppName=myriad,
						 file_utils:join( AppParentRootDir, "Ceylan-Myriad" ) ).
