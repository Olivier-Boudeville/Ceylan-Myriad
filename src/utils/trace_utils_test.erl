% Copyright (C) 2017-2017 Olivier Boudeville
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


% Unit tests for the trace_utils toolbox.
%
% See the trace_utils.erl tested module.
%
-module(trace_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Testing the default, "
							 "very basic trace subsystem." ),

	trace_utils:debug( "I am a debug simple message." ),
	trace_utils:debug_fmt( "I am a debug ~s message.", [ "formatted" ] ),

	trace_utils:trace( "I am a trace simple message." ),
	trace_utils:trace_fmt( "I am a trace ~s message.", [ "formatted" ] ),

	trace_utils:info( "I am a info simple message." ),
	trace_utils:info_fmt( "I am a info ~s message.", [ "formatted" ] ),

	trace_utils:warning( "I am a warning simple message." ),
	trace_utils:warning_fmt( "I am a warning ~s message.", [ "formatted" ] ),

	trace_utils:error( "I am a error simple message." ),
	trace_utils:error_fmt( "I am a error ~s message.", [ "formatted" ] ),

	trace_utils:fatal( "I am a fatal simple message." ),
	trace_utils:fatal_fmt( "I am a fatal ~s message.", [ "formatted" ] ),

	test_facilities:stop().
