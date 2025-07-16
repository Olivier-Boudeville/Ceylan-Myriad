% Copyright (C) 2019-2025 Olivier Boudeville
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
% Creation date: Sunday, November 17, 2019.

-module(type_utils_test).

-moduledoc """
Unit tests for the `type_utils` services.

See the `type_utils` tested module.
""".



% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),


	test_facilities:display( "Testing type interpretion." ),

	FirstTerm = "Hello",

	test_facilities:display( "Interpreting '~ts': ~ts",
		[ FirstTerm, type_utils:interpret_type_of( FirstTerm ) ] ),

	SecondTerm = [ my_atom, 4,
		{ a_tag, 2.0, maps:from_list( [ { self(), [ FirstTerm ] } ] ) } ],

	%MaxLevel = 0,
	%MaxLevel = 1,
	%MaxLevel = 2,
	MaxLevel = infinite,

	test_facilities:display( "Interpreting '~p': ~ts",
		[ SecondTerm, type_utils:interpret_type_of( SecondTerm, MaxLevel ) ] ),


    % To test various (valid) type strings:
    %TypeStr = "[float()]",
    %TypeStr = "[T]",
    TypeStr = "{bar,integer()}",

    % To test various invalid type strings:
    %TypeStr = "list(float())",
    %TypeStr = "list(T)",


    %TypeStr = "list(tuple(float(), table(integer(), option(string())), "
    %    "list(union(foo,bar))))",

    test_facilities:display( "Parsing the type of '~ts'.", [ TypeStr ] ),

    ParsedType = type_utils:parse_type( TypeStr ),

    test_facilities:display( "Type parsing resulted in contextual type ~w.~n"
        "It is described as '~ts'.",
        [ ParsedType, type_utils:type_to_string( ParsedType ) ] ),


    FooTypeStr = "table(tuple(V,float()),union(bar,integer(),option(U)))",

    test_facilities:display( "Declaring now a parametrised type foo(U,V) "
                             "based on type ~ts.", [ FooTypeStr ] ),

    % Building it from a string:
    FooParsedType = type_utils:parse_type( FooTypeStr ),

    % As the order of type variables matters when using such type afterwards:
    WithFooTypedefTable = type_utils:declare_type( foo, [ 'U', 'V' ],
                                                   FooParsedType, table:new() ),

    test_facilities:display( "Once foo/2 has been declared, having: ~ts.",
        [ type_utils:typedef_table_to_string( WithFooTypedefTable ) ] ),

	test_facilities:stop().
