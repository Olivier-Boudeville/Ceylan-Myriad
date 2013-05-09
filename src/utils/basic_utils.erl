% Copyright (C) 2003-2013 Olivier Boudeville
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
% Creation date: July 1, 2007.


% Gathering of various convenient facilities.
% See basic_utils_test.erl for the corresponding test.
-module(basic_utils).




% Timestamp-related functions.
-export([ get_timestamp/0,
		 get_textual_timestamp/0, get_textual_timestamp/1,
		 get_textual_timestamp_for_path/0, get_textual_timestamp_for_path/1,
		 timestamp_to_string/1, get_duration/2, get_textual_duration/2,
		 get_precise_timestamp/0, get_precise_duration/2 ]).



% Registration functions.
-export([ register_as/2, register_as/3, unregister/2,
		 get_registered_pid_for/1, get_registered_pid_for/2,
		 wait_for_global_registration_of/1, wait_for_local_registration_of/1 ]).



% Random-related functions.
-export([ start_random_source/3, start_random_source/1, stop_random_source/0,
		 get_random_value/0, get_random_value/1, get_random_module_name/0,
		 get_random_seed/0, random_permute/1, generate_uuid/0
		 ]).



% List management functions.
-export([ get_element_at/2, remove_element_at/2, uniquify/1,
		 subtract_all_duplicates/2, append_at_end/2 ]).



% Notification-related functions.
-export([ speak/1, notify_user/1, notify_user/2 ]).



% Code-related functions.
-export([ get_code_for/1, deploy_modules/2, deploy_modules/3 ]).


% Miscellaneous functions.
-export([ flush_pending_messages/0, checkpoint/1, compare_versions/2,
		sum_probabilities/1, draw_element/1, draw_element/2,
		get_process_specific_value/0, get_process_specific_value/2 ]).





% Timestamp-related functions.


% Returns a tuple describing the current time.
% Ex: { {Year,Month,Day}, {Hour,Minute,Second} } = basic_utils:get_timestamp()
% may return '{{2007,9,6},{15,9,14}}'.
get_timestamp() ->
	% Was: {erlang:date(),erlang:time()}.
	% Better:
	erlang:localtime().



% Returns a string corresponding to the current timestamp, like:
% "2009/9/1 11:46:53".
get_textual_timestamp() ->
	get_textual_timestamp( get_timestamp() ).


% Returns a string corresponding to the specified timestamp, like:
% "2009/9/1 11:46:53".
get_textual_timestamp( { {Year,Month,Day}, {Hour,Minute,Second} } ) ->
	io_lib:format( "~p/~p/~p ~B:~2..0B:~2..0B",
		[Year,Month,Day,Hour,Minute,Second] ).



% Returns a string corresponding to the current timestamp and able to be a part
% of a path, like: "2009/9/1 11:46:53".
get_textual_timestamp_for_path() ->
	get_textual_timestamp_for_path( get_timestamp() ).


% Returns a string corresponding to the specified timestamp and able to be a
% part of a path, like: "2009/9/1 11:46:53".
get_textual_timestamp_for_path( { {Year,Month,Day}, {Hour,Minute,Second} } ) ->
	io_lib:format( "~p-~p-~p-at-~Bh-~2..0Bm-~2..0Bs",
		[Year,Month,Day,Hour,Minute,Second] ).



% Alias of get_textual_timestamp.
timestamp_to_string(Timestamp) ->
	get_textual_timestamp(Timestamp).





% Returns the (signed) duration in seconds between the two specified timestamps,
% using the first one as starting time and the second one as stopping time.
get_duration(FirstTimestamp,SecondTimestamp) ->
	First  = calendar:datetime_to_gregorian_seconds(FirstTimestamp),
	Second = calendar:datetime_to_gregorian_seconds(SecondTimestamp),
	Second - First.


% Returns a textual description of the duration between the two specified
% timestamps.
%
% See also: text_utils:duration_to_string/1, which is smarter.
get_textual_duration(FirstTimestamp,SecondTimestamp) ->
	{Days,{Hour, Minute, Second}} = calendar:seconds_to_daystime(
		get_duration(FirstTimestamp,SecondTimestamp) ),

	lists:flatten( io_lib:format( "~B day(s), ~B hour(s), ~B minute(s) "
		"and ~B second(s)", [Days, Hour, Minute, Second] ) ).




% Returns a timestamp that is as precise as possible: {MegaSecs,Secs,MicroSecs},
% where:
%
%  - MegaSecs is an integer number of millions of seconds
%  - Secs is an integer number of second which is less than one million
%  - MicroSecs is an integer number of microseconds
%
get_precise_timestamp() ->
	erlang:now().



% Returns the (signed) duration in milliseconds between the two specified
% precise timestamps (as obtained thanks to get_precise_duration/0), using the
% first one as starting time and the second one as stopping time.
get_precise_duration( _FirstTimestamp={A1,A2,A3},
					 _SecondTimestamp={B1,B2,B3} ) ->

	% Seconds to be converted in milliseconds:
	1000 * ( (B1-A1) * 1000000 + B2-A2 ) + round( (B3-A3)/1000 ).




% Registration functions.


% Registers the current process under specified name, which must be an atom.
% Declaration is register_as(Name,RegistrationType) with
% RegistrationType in 'local_only', 'global_only', 'local_and_global', 'none'
% depending on what kind of registration is requested.
% Returns ok on success, otherwise throws an exception.
register_as( Name, RegistrationType ) ->
	register_as( self(), Name, RegistrationType ).



% Registers specified PID under specified name, which must be an atom.
% Declaration is: register_as(Pid,Name,RegistrationType) with
% RegistrationType in 'local_only', 'global_only', 'local_and_global',
% 'none', depending on what kind of registration is requested.
% Returns ok on success, otherwise throws an exception.
register_as( Pid, Name, local_only ) when is_atom(Name) ->

	try erlang:register( Name, Pid ) of

		true ->
			ok

	catch

		ExceptionType:Exception ->
			throw( {local_registration_failed,Name,{ExceptionType,Exception}} )

	end;

register_as( Pid, Name, global_only ) when is_atom(Name) ->
	case global:register_name( Name, Pid ) of

		yes ->
			ok;

		no ->
			throw( {global_registration_failed,Name} )

	end;

register_as( Pid, Name, local_and_global ) when is_atom(Name) ->
	ok = register_as(Pid,Name,local_only),
	ok = register_as(Pid,Name,global_only);

register_as(_Pid,_Name,none) ->
	ok.




% Unregisters specified name from specified registry.
% Throws an exception in case of failure.
unregister( Name, local_only ) ->
	try erlang:unregister( Name ) of

		true ->
			ok

	catch

		ExceptionType:Exception ->
			throw(
				{local_unregistration_failed,Name,{ExceptionType,Exception}} )

	end;

unregister( Name, global_only ) ->
	% Documentation says it returns "void" (actually 'ok'):
	try

		global:unregister_name( Name )

	catch

		ExceptionType:Exception ->
			throw(
				{global_unregistration_failed,Name,{ExceptionType,Exception}} )

	end;

unregister( Name, local_and_global ) ->
	ok = unregister( Name, local_only ),
	ok = unregister( Name, global_only );

unregister(_Name,none) ->
	ok.



% Returns the Pid that should be already registered, as specified name.
% Local registering will be requested first, if not found global one will
% be tried.
% No specific waiting for registration will be performed, see
% wait_for_*_registration_of instead.
get_registered_pid_for( Name ) ->
	get_registered_pid_for( Name, local_otherwise_global ).



get_registered_pid_for( Name, local_otherwise_global ) ->
	try

		get_registered_pid_for( Name, local )

	catch

		{not_registered_locally,_Name} ->

			try

				get_registered_pid_for( Name, global )


			catch

				{not_registered_globally,Name} ->
					throw( {neither_registered_locally_nor_globally,Name} )

			end

	end;

get_registered_pid_for( Name, local ) ->
	case erlang:whereis( Name ) of

		undefined ->
			throw( {not_registered_locally,Name} );

		Pid ->
			Pid

	end;

get_registered_pid_for( Name, global ) ->
	case global:whereis_name( Name ) of

		undefined ->
			throw( {not_registered_globally,Name} );

		Pid ->
			Pid

	end;

% So that the atom used for registration can be used for look-up as well,
% notably in static methods (see the registration_type defines).
get_registered_pid_for( Name, local_and_global ) ->
	get_registered_pid_for( Name, local_otherwise_global ).





% Waits (up to 10 seconds) until specified name is globally registered.
%
% Returns the resolved Pid, or throws
% {global_registration_waiting_timeout,Name}.
wait_for_global_registration_of(Name) ->
	wait_for_global_registration_of(Name,10).


wait_for_global_registration_of(Name,0) ->
	throw({global_registration_waiting_timeout,Name});

wait_for_global_registration_of(Name,SecondsToWait) ->
	case global:whereis_name( Name ) of

		undefined ->
			timer:sleep(1000),
			wait_for_global_registration_of(Name,SecondsToWait-1);

		Pid ->
			Pid

	end.




% Waits (up to 5 seconds) until specified name is locally registered.
% Returns either the resolved Pid or {,Name}.
% Returns the resolved Pid, or throws
% {local_registration_waiting_timeout,Name}.
wait_for_local_registration_of(Name) ->
	wait_for_local_registration_of(Name,5).


wait_for_local_registration_of(Name,0) ->
	throw({local_registration_waiting_timeout,Name});

wait_for_local_registration_of(Name,SecondsToWait) ->
	case erlang:whereis( Name ) of

		undefined ->
			timer:sleep(1000),
			wait_for_local_registration_of(Name,SecondsToWait-1);

		Pid ->
			Pid

	end.




% Random functions.

% If use_crypto_module is defined, the crypto module will be used, otherwise
% the random module will be used instead.
%
% Currently the crypto module is not used, as:
%  - not all Erlang VM can be built with the proper SSH support
%  - it is unclear whether the crypto module can be seeded like the random
% module can be (probably it cannot be)
%  - there is no crypto function returning a random float uniformly
% distributed between 0.0 and 1.0, and it may not be easy to implement it
% from what is available
%
% Therefore the two modules are not completely interchangeable.
%
%-define(use_crypto_module,).


-ifdef(use_crypto_module).


% crypto module used here.
% The seed and state management is presumably global (not per-process).

start_random_source(_A,_B,_C) ->
	throw(crypto_module_cannot_be_seeded).


start_random_source( default_seed ) ->
	ok = crypto:start();

start_random_source( time_based_seed ) ->
	throw(crypto_module_cannot_be_seeded).


stop_random_source() ->
	ok = crypto:stop().


% Returns an integer random value generated from an uniform distribution.
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
get_random_value(N) ->
	crypto:rand_uniform(1,N+1).


% Returns a random float uniformly distributed between 0.0 and 1.0, updating
% the random state in the process dictionary.
get_random_value() ->
	% Not available: crypto:rand_uniform(0.0,1.0).
	not_available_with_crypto.


% Returns the name of the module managing the random generation.
get_random_module_name() ->
	crypto.


-else. % use_crypto_module


% Default random module used here.
% The seed and state management is per-process (stored in the process
% dictionary).

start_random_source(A,B,C) ->
	random:seed(A,B,C).


% Seeds the random number generator, either with specified seed, or with a
% default seed (if wanting to obtain the same random series at each run) or with
% current time (if wanting "real" non-reproducible randomness).
start_random_source( {A,B,C} ) ->
	start_random_source(A,B,C);

start_random_source( default_seed ) ->
	% Use default (fixed) values in the process dictionary:
	random:seed();

start_random_source( time_based_seed ) ->
	% Each run will result in different random series:
	{A, B, C} = erlang:now(),
	start_random_source(A,B,C).


stop_random_source() ->
	ok.


% Returns an integer random value generated from an uniform distribution.
%
% Given an integer N >= 1, returns a random integer uniformly distributed
% between 1 and N (both included), updating the random state in the process
% dictionary.
get_random_value(N) ->
	random:uniform(N).


% Returns a random float uniformly distributed between 0.0 and 1.0, updating the
% random state in the process dictionary.
get_random_value() ->
	random:uniform().


% Returns the name of the module managing the random generation.
get_random_module_name() ->
	random.


-endif. % use_crypto_module



-define(seed_upper_bound,65500).


% Returns a seed obtained from the random source in use.
% This is a randomly-determined seed, meant to be used to create another
% random generator.
get_random_seed() ->
	{   get_random_value( ?seed_upper_bound ),
		get_random_value( ?seed_upper_bound ),
		get_random_value( ?seed_upper_bound ) }.



% Returns a random uniform permutation of the specified list.
%
% Inspired from http://paste.lisp.org/display/74804.
%
% All these algorithms would need random access to a list, which is not readily
% possible here, hence must be emulated.
%
% See also the 'Speedy unsort:shuffle/1,2' thread in the erlang-questions
% mailing list for counterparts.
random_permute( List ) ->
	random_permute( List, length(List) ).


random_permute( _List, _RemainingLen=0 ) ->
	[];

random_permute( List, RemainingLen ) ->

	% Cheking is commented-out:
	%RemainingLen = length(List),

	% Uses the 'random' basic random source:

	% (using remove_element_at/2 should be quicker than using
	% proplists:delete/2, as we stop at the first matching element found)
	Index = random:uniform( RemainingLen ),
	[ get_element_at( List, Index )
		| random_permute( remove_element_at( List, Index ), RemainingLen-1 ) ].




% Returns a string containing a new universally unique identifier (UUID), based
% on the system clock plus the system's ethernet hardware address, if present.
generate_uuid() ->
	Res = os:cmd( "uuidgen -t" ),
	% Removes the final end-of-line:
	tl( lists:reverse(Res) ).



% List management functions.


% Index start at position #1, not #0.

% Returns the element in the list at the specified index, in [1..length(List)].
%
% If the index is out of bounds, a function_clause is raised.
%
% Note: usually these kinds of functions should not be used, recursive
% algorithms are a lot more effective, when applicable.
%
get_element_at( List, Index ) ->
	lists:nth( Index, List ).


%% get_element_at( List, 1 ) ->
%% 	hd(List);

%% get_element_at( [_H|T], Index ) ->
%% 	get_element_at( T, Index-1 ).



% Returns a list corresponding to the specified one with the element at
% specified index removed.
%
% If the index is out of bounds, a function_clause like
% '[{basic_utils,remove_element_at,...}]' is triggered.
%
% Note: usually these kinds of functions should not be used, recursive
% algorithms are a lot more effective, when applicable.
%
% Signature: remove_element_at(List,Index).
%
% Curiously lists:nth exists, but no function to remove an element specified by
% its index seems to be available in the lists module.
%
% Not tail recursive version:
%remove_element_at( [_H|T], 1 ) ->
%	T;
%
%remove_element_at( [H|T], N ) ->
%	[H|remove_element_at(T,N-1)].
% Tail recursive version:
remove_element_at( List, Index ) ->
	remove_element_at( List, Index, [] ).

remove_element_at( [_H|RemainingList], 1, Result ) ->
	lists:reverse( Result ) ++ RemainingList;

remove_element_at( [H|RemainingList], Index, Result ) ->
	remove_element_at( RemainingList, Index-1, [H|Result] ).


% Returns a list whose elements are the ones of the specified list, except that
% they are unique (all their duplicates have been removed).
%
% No specific order is respected in the returned list.
%
% Ex: if L = [1,2,3,2,2,4,5,5,4,6,6,5], then basic_utils:uniquify(L) is:
% [3,6,2,5,1,4].
uniquify( List ) ->
	% There is probably a more efficient way of doing the same:
	sets:to_list( sets:from_list(List) ).


% Returns a list equal to L1 except that all elements found in L2 have been
% removed, even if in L1 they were duplicated.
% Note: like lists:subtract, except that all occurences from L2 (not only
% the first one) are removed.
% Example: [1,4] = basic_utils:subtract_all_duplicates( [1,2,3,4,2], [2,3] )
% Taken from
% http://www.trapexit.org/Finding_Elements_in_One_Array_but_Not_Another
subtract_all_duplicates( L1, L2 ) ->
	lists:filter( fun(E) -> not lists:member(E,L2) end, L1).



% Appends specified element at the end of specified list, without changing the
% order of the list.
% Ex: append_at_end( d, [a,b,c] ) returns [a,b,c,d].
append_at_end( Elem, L ) when is_list(L) ->
	% Should be more efficient than lists:reverse( [Elem|lists:reverse(L)] ):
	L ++ [Elem].



% Notification-related functions.


% Speaks the specified message, using espeak.
speak(Message) ->
	[] = os:cmd("espeak -s 140 \"" ++ Message ++ "\" &" ).


% Notifies the user of the specified message, with log output and synthetic
% voice.
notify_user(Message) ->
	io:format(Message),
	speak(Message).



% Notifies the user of the specified message, with log output and synthetic
% voice.
% Example: 'basic_utils:notify_user( "Hello ~w", [ Name ]).'
notify_user(Message,FormatList) ->
	ActualMessage = io_lib:format(Message,FormatList),
	io:format(ActualMessage),
	speak(ActualMessage).





% Code-related functions.


% Returns a {ModuleBinary,ModuleFilename} pair for the module specified as an
% atom, or throws an exception.
get_code_for( ModuleName ) ->

	case code:get_object_code( ModuleName ) of

		{ModuleName,ModuleBinary,ModuleFilename} ->
			{ModuleBinary,ModuleFilename};

		error ->
			throw( {module_code_lookup_failed,ModuleName} )

	end.




% RPC default time-out, in milliseconds:
% (30s, could be infinity)
-define( rpc_timeout, 30*1000 ).


% Deploys the specified list of modules on the specified list of nodes (atoms):
% sends them these modules (as a binary), and load them so that they are ready
% for future use.
deploy_modules( Modules, Nodes ) ->
	deploy_modules( Modules, Nodes, _Timeout=?rpc_timeout ).


% Deploys the specified list of modules on the specified list of nodes (atoms):
% sends them these modules (as a binary), and load them so that they are ready
% for future use.
%
% Timeout is the time-out duration, either an integer number of milliseconds, or
% the infinity atom.
deploy_modules( Modules, Nodes, Timeout ) ->
	% For each module in turn, contact each and every node in parallel:
	[ deploy_module( M, get_code_for(M), Nodes, Timeout ) || M <- Modules ].



% (helper function)
deploy_module( ModuleName, {ModuleBinary,ModuleFilename}, Nodes, Timeout ) ->

	{ResList,BadNodes} = rpc:multicall( Nodes, code,
				load_binary,
				[ ModuleName, ModuleFilename, ModuleBinary ],
				Timeout ),

	ReportedErrors = [ E || {error,E} <- ResList ],

	case BadNodes of

		[] ->
			case ReportedErrors of

				[] ->
					ok;

				_ ->
					throw( {module_deployment_failed,ModuleName,
						ReportedErrors} )

			end;

		_ ->
			throw( {module_deployment_failed,ModuleName,
					{ReportedErrors,BadNodes}} )

	end.



% Miscellaneous functions.


% Flushes all the messages still in the mailbox of this process.
flush_pending_messages() ->
	receive

		_ ->
			flush_pending_messages()

	after 0 ->
		ok

	end.



% Displays a numbered checkpoint.
% Useful for debugging purposes.
checkpoint(Number) ->
	io:format( "----- CHECKPOINT #~B -----~n", [Number] ).



% Compares the two triplets, which describes two version numbers (ex: {0,1,0})
% and returns either first_bigger, second_bigger, or equal.
% Note: the default term order is already what we needed.
compare_versions( {A1,A2,A3}, {B1,B2,B3} ) ->
	case {A1,A2,A3} > {B1,B2,B3} of

		true ->
			first_bigger;

		false ->

			case {A1,A2,A3} =:= {B1,B2,B3} of

				true ->
					equal;

				false ->
					second_bigger

			end
	end.



% Draws one element at random of the specified list, which is a list of
% {Element,Probability} pairs: returns the drawn element, knowing that it will
% be choosen according to its probability.
%
% Probabilities are managed as relative values, they do not have to sum up to
% 1.0; they must be positive or null integers, and their sum must not be null.
%
% Ex: ElementList = [{first,1},{second,2},{third,1}] is excepted to return
% 'second' twice as frequently as 'first' or 'third'.
%
% Using [{first,1},{second,0},{third,1}] instead would mean that 'second' would
% never be drawn.
draw_element( ElementList ) ->
	draw_element( ElementList, sum_probabilities( ElementList ) ).


sum_probabilities( ElementList ) ->
	sum_probabilities( ElementList, 0 ).


sum_probabilities( [], Acc ) ->
	Acc;

sum_probabilities( [ {_Element,Probability} | T ], Acc ) ->
	sum_probabilities( T, Acc+Probability ).



% Sum must be equal to the sum of all probabilities in ElementList.
draw_element( _ElementList, 0 ) ->
	throw( null_total_probability );

draw_element( ElementList, Sum ) ->
	DrawnValue = get_random_value( Sum ),
	%io:format( "draw_element: drawn ~B.~n", [DrawnValue] ),
	select_element( ElementList, DrawnValue, _CurrentSum = 0 ).



select_element( [ {Element,Probability} | _T ], DrawnValue, CurrentSum )
		when Probability + CurrentSum >= DrawnValue ->
	% Just gone past the probability range:
	Element;

select_element( [ {_Element,Probability} | T ], DrawnValue, CurrentSum ) ->
	% Drawn value still not reached, continuing:
	select_element( T, DrawnValue, CurrentSum+Probability ).



% Returns a value (a strictly positive integer) expected to be as much as
% possible specific to the current process.
%
% Mostly based on its PID.
%
% Useful for example when a large number of similar processes try to access to
% the same resource (ex: a set of file descriptors) over time: they can rely on
% some random waiting based on that process-specific value in order to smooth
% the accesses over time.
%
% We could imagine taking into account as well the current time, the process
% reductions, etc. or generating a reference.
get_process_specific_value() ->
	% PID are akin to <X.Y.Z>.
	PidAsText = lists:flatten( io_lib:format( "~w", [self()] ) ),
	%io:format( "PID: ~w.~n", [self()] ) ,
	% Ex: ["<0","33","0>"]:
	[ [$<|First],Second,Third ] = string:tokens(PidAsText,"."),
	% We add 1 to x and z as they might be null:
	{F,[]} = string:to_integer(First),
	{S,[]} = string:to_integer(Second),
	[$>|ExtractedThird] = lists:reverse(Third),
	{T,[]} = string:to_integer( ExtractedThird ),
	X = F+1,
	Y = S,
	Z = T+1,
	%io:format( "Res = ~w.~n", [X*Y*Z] ),
	X*Y*Z.


% Returns a process-specific value in [Min,Max[.
get_process_specific_value( Min, Max ) ->
	Value = get_process_specific_value(),
	{H,M,S} = erlang:time(),
	( ((H+M+S+1)*Value) rem (Max-Min) ) + Min.
