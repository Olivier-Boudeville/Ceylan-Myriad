-module(simple_parse_transform_target).


-export([ wooper_get_instance_description/1, f/1, g/0, synchronous_new/2 ]).


-type foo() :: { integer(), table:table() }.

-bar( hello ).


-table_type( list_hashtable ).

% Uncomment to test the trigger of 'table type defined more than once':
%-table_type( foo_hashtable ).


-export_type([ foo/0 ]).

-spec wooper_get_instance_description( wooper:state() ) ->
							 { wooper:state(), text_utils:ustring() }.
wooper_get_instance_description( State ) ->
	{ State, wooper:instance_to_string( State ) }.


%-spec f( integer() ) -> table:table().
%f( _Int ) ->
%	table:new().


f( _ ) ->
	aa,
	bb,
	cc.


-spec g() -> pair:pair().
g() ->
	A = foobar,
	{ A, A }.


%new( A, B, C ) ->

	%io:format("new operator: spawning ~w:wooper_construct_and_run "
	% "with parameters ~w.~n", [ ?MODULE, [ ?wooper_construct_parameters ] ] ),

%	spawn( fun() -> wooper_construct_and_run( [ A, B, C ] )
%						end ).


%-spec destruct( wooper:state() ) -> wooper:state().
%destruct( State ) ->
%	State.


%wooper_construct_and_run( _, _ ) -> ok.

%% -spec remote_new( net_utils:atom_node_name(), x:a(), x:b() ) ->
%%						wooper:instance_pid().
%% remote_new( TargetNode, A, B ) ->
%%	spawn( TargetNode, fun() -> wooper_construct_and_run( A, B ) end ).


synchronous_new( A, B ) ->

	%io:format("synchronous_new operator: spawning ~w:wooper_construct_and_run "
	% "with parameters ~w.~n", [ ?MODULE, [ ?wooper_construct_parameters ] ] ),

	CreatorPid = self(),

	SpawnedPid = spawn( fun() -> wooper:construct_and_run_synchronous(
		[ A, B ], CreatorPid ) end ),

	% Blocks until the spawned process answers:
	%
	% (no risk of synchronous spawns mismatch, as each synchronous call is
	% waited for)
	%
	receive

		{ spawn_successful, SpawnedPid } ->
			SpawnedPid

	after 500 ->

		throw( { synchronous_time_out, toto } )

	end.
