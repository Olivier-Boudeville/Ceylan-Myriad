% Copyright (C) 2014-2020 Olivier Boudeville
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
% Creation date: Tuesday, December 25, 2018


% Unit tests for the cond_utils services.
%
% See the cond_utils.erl tested module.
%
-module(cond_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Tokens may only be defined on the command-line (ex: see ERLANG_COMPILER_TOKEN_OPT
% in GNUmakevars.inc for that).
%
% Based on the settings specified in GNUmakevars.inc, we expect:
% - my_first_test_token to be defined, yet with no associated value
% - my_second_test_token to be defined, set to 200
% - my_third_test_token to be defined, set to some_text
% (and no other token to be defined)


% Note: we use here the process dictionary in order to detect more easily any
% unexpected, non-legit code execution; we want not only to detect whenever a
% right branch is executed, but also when a wrong one is, or both, or none.


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),


	test_facilities:display( "Testing code whose execution is conditioned by "
							 "the definition of a token." ),

	A = 1,
	B = 2,

	% To silence a warning about A and B being unused should the token not be
	% defined:
	%
	basic_utils:ignore_unused( [ A, B ] ),


	test_facilities:display( "Testing cond_utils:if_defined/2." ),

	cond_utils:if_debug( [ io:format( "We are in debug mode!~n" ),
						   trace_utils:info( "And we like it!" ) ] ),

	test_facilities:display( "Testing cond_utils:if_defined/2." ),


	process_dictionary:put( process_test_key, 1 ),

	%cond_utils:if_defined( non_existing_token,
	cond_utils:if_defined( my_first_test_token,
						   [ A = 1,
							 io:format( "Conditional code executed!~n" ),
							 B = A + 1,
							 process_dictionary:put( process_test_key, 2 ) ] ),

	% {badmatch,1} would imply that no -Dmy_first_test_token was specified:
	2 = process_dictionary:get( process_test_key ),


	test_facilities:display( "Testing cond_utils:if_defined/3." ),

	% A single expression is used here:
	cond_utils:if_defined( my_first_test_token,
			   process_dictionary:put( process_test_key, 3 ),
			   [ process_dictionary:put( process_test_key, 4 ),
				 trace_utils:error( "Wrong branch selected (4)." ) ] ),

	3 = process_dictionary:get( process_test_key ),


	test_facilities:display( "Testing cond_utils:if_set_to/3." ),

	cond_utils:if_set_to( my_second_test_token, 200,
		[ trace_utils:info(
			"Test token detected and set as expected (5)." ),
		  process_dictionary:put( process_test_key, 5 ) ] ),

	5 = process_dictionary:get( process_test_key ),


	test_facilities:display( "Testing cond_utils:if_set_to/4." ),

	cond_utils:if_set_to( another_non_existing_token, some_different_text,
		[ trace_utils:error( "Wrong branch selected (6)." ),
		  process_dictionary:put( process_test_key, 6 ) ],
		[ process_dictionary:put( process_test_key, 7 ),
		  trace_utils:info(
			"Other test token detected and managed as expected (7)." ) ] ),

	7 = process_dictionary:get( process_test_key ),

	cond_utils:if_set_to( my_third_test_token, some_text,
		[ process_dictionary:put( process_test_key, 8 ),
		  trace_utils:info(
			"Other test token detected and managed as expected (8)." ) ],
		[ process_dictionary:put( process_test_key, 9 ),
		  trace_utils:error( "Wrong branch selected (9)." ) ] ),

	8 = process_dictionary:get( process_test_key ),

	cond_utils:if_set_to( my_third_test_token, some_different_text,
		[ trace_utils:error( "Wrong branch selected (10)." ),
		  process_dictionary:put( process_test_key, 10 ) ],
		[ process_dictionary:put( process_test_key, 11 ),
		  trace_utils:info(
			"Other test token detected and managed as expected (11)." ) ] ),

	11 = process_dictionary:get( process_test_key ),


	test_facilities:display( "Testing cond_utils:assert/1." ),

	cond_utils:assert( true ),
	cond_utils:assert( not false ),
	% Would fail in debug mode: cond_utils:assert( false ),

	cond_utils:assert( A =:= basic_utils:identity( A ) ),
	%cond_utils:assert( A =/= basic_utils:identity( A ) ),


	test_facilities:display( "Testing cond_utils:assert/2." ),

	cond_utils:assert( my_first_test_token, not ( B =:= 1 ) ),
	cond_utils:assert( my_first_test_token, B =:= 2 ),
	%cond_utils:assert( my_first_test_token, not ( B =:= 2 ) ),

	cond_utils:assert( non_existing_token, true ),
	cond_utils:assert( non_existing_token, false ),


	test_facilities:display( "Testing cond_utils:assert/3." ),

	% Injected, as token value matches:
	cond_utils:assert( my_second_test_token, 200, true ),
	%cond_utils:assert( my_second_test_token, 200, false ),

	% Not injected, as token value does not match:
	cond_utils:assert( my_third_test_token, some_different_text, true ),
	cond_utils:assert( my_third_test_token, some_different_text, false ),

	test_facilities:stop().
