-module(simple_parse_transform_target).


-export([ f/1 ]).


-type foo() :: { integer(), table:table() }.

-bar( hello ).

% Test to trigger 'table type defined more than once':
%-table_type( foo_hashtable ).
-table_type( list_hashtable ).


-export_type([ foo/0 ]).


-spec f( integer() ) -> table:table().
f( _Int ) ->
	table:new().
