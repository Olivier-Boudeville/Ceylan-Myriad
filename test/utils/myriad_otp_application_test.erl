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
% Creation date: Friday, July 19, 2019.


% Testing of Myriad as an OTP library application.
-module(myriad_otp_application_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Actual test:
test_myriad_application( EBinPath ) ->

	code_utils:declare_beam_directory( EBinPath ),

	test_facilities:display( "Starting the Myriad application." ),
	ok = application:start( myriad ),

	test_facilities:display( "Myriad version: ~p.",
				 [ system_utils:get_application_version( myriad ) ] ),

	test_facilities:display( "Current user name: '~s'.",
							 [ system_utils:get_user_name() ] ),

	test_facilities:display( "Stopping the Myriad application." ),
	ok = application:stop( myriad ),

	test_facilities:display(
	  "Successful end of test of the Myriad application." ).



% Note that the ebin application directory must be in the code path for the
% myriad.app file to be found and used, and for this test to succeed.
%
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Supposing here that the application is built, in the usual _build
	% directory, with the default rebar3 profile:
	%
	EBinPath = "../../_build/default/lib/myriad/ebin/",

	case file_utils:is_existing_directory_or_link( EBinPath ) of

		true ->
			test_myriad_application( EBinPath ) ;

		false ->
			trace_utils:warning_fmt( "No build directory found for the Myriad "
				"application (searched for '~s'), stopping this test "
				"(run beforehand 'make rebar3-compile' at the root of the "
				"source tree for a more relevant testing).", [ EBinPath ] )

	end,

	test_facilities:stop().
