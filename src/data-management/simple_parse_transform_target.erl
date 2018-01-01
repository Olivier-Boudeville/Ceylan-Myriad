% Copyright (C) 2015-2018 Olivier Boudeville
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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% A simple target module in order to test how parse transforms can operate.
%
-module(simple_parse_transform_target).


-export([ f/1, g/0 ]).

-type my_type() :: pid().
%-type my_type() :: boolean().


-type foo_tuple() :: { integer(), float() }.
-type foo_user() :: { my_type(), float() }.

-type foo_table_remote() :: { integer(), table:table() }.
-type foo_table_remote_complex() :: { integer(),
									  table:table( float(), list( any() ) ) }.

-type foo_table_local() :: { integer(), table() }.
-type foo_table_local_complex() :: { integer(), table( float(), list() ) }.

-type foo_table_other_type() :: [ table:value() ].

-type foo_void() :: void().
-type foo_maybe() :: maybe( float() ).

-type foo_list() :: [ boolean() ].
-type foo_union() :: integer() | atom().

-bar( hello ).


-table_type( list_table ).

% Uncomment to test the trigger of 'table type defined more than once':
%-table_type( foo_hashtable ).


%-export_type([ my_type/0, foo/0 ]).
-export_type([ foo_tuple/0, foo_user/0,
			   foo_table_remote/0, foo_table_remote_complex/0,
			   foo_table_local/0, foo_table_local_complex/0,
			   foo_void/0, foo_maybe/0, foo_list/0, foo_union/0 ]).



%-spec f( integer() ) -> table:table().
%f( _Int ) ->
%	table:new().

f( 1 ) ->
	2;

f( _ ) ->
	aa,
	bb,
	cc.


% To check that 'function h/0 is unused' is indeed reported as a warning (and
% then trated as an error):
%
%h() ->
%	ok.

-spec g() -> basic_utils:void().
%-spec g() -> void().
g() ->
	A = foobar,
	U = { A, A },
	io:format( "Hello U: ~p", [ U ] ).
