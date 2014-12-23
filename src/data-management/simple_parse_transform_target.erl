-module(simple_parse_transform_target).


-export([ f/1 ]).


-type foo() :: { integer(), table:table() }.


-export_type([ foo/0 ]).


-spec f( integer() ) -> table:table().
f( _Int ) ->
	table:new().
