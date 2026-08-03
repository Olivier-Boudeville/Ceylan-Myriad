% Copyright (C) 2026-2026 Olivier Boudeville
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
% Creation date: Wednesday, July 8, 2026.

-module(record_parse_test).

-moduledoc """
Tests for the support of **records** by the Myriad parse transform.

All related constructs shall be correctly parsed and managed by such a
transform.
""".


% For run/0 export and al:
-include("test_facilities.hrl").

-define( default_y, 1.0 ).
-define( default_z, 2.0 ).

-define( x_for_r1, 3 ).

-record #my_native_record{ x,
                           y = ?default_y,
                           z = ?default_z :: pos_integer() }.

% Not detected at build or run time:
-import_record( non_existing_module, [ some_record ]).

-export_record([ my_native_record ]).


test_native_records() ->

    test_facilities:display(
        "Testing the support of native records, first with their creation." ),

    XForR1 = ?x_for_r1,
    R1 = #my_native_record{ x=XForR1 },
    test_facilities:display( "  Constructed: ~w.", [ R1 ] ),

    XForR2 = 4,
    ZForR2 = undefined, % Not even a legit value.
    R2 = #?MODULE:my_native_record{ x=XForR2, z=undefined },
    test_facilities:display( "  Constructed: ~p.", [ R2 ] ),

    %ModExpr = ?MODULE,

    % syntax error before: ':':
    %R3 = #ModExpr:my_native_record{ x=5 },
    %test_facilities:display( "Constructed: ~p.", [ R3 ] ),

    % Must be initialised: #my_native_record{},
    R3 = #my_native_record{ x=hello},


    test_facilities:display( "~nTesting record field access." ),

    XForR1 = R1#my_native_record.x,

    ZForR2 = R2#?MODULE:my_native_record.z,

    % Not specifying the actual record type:
    XForR2 = R2#_.x,


    test_facilities:display(
        "~nTesting (anonymous) record field update (and matching)." ),

    R1 = R2#_{ x=XForR1, z=?default_z },


    test_facilities:display( "~nTesting BIFs and head matching." ),

    f( R1 ), g( R2 ), h( R3 ),

    i ( R1 ), j( R2 ), k( R1 ).




f( R ) when is_record( R ) ->
    ok.

g( R ) when is_record( R, my_native_record ) ->
    ok.

h( R ) when is_record( R, ?MODULE, my_native_record ) ->
    ok.


i( #my_native_record{}=R ) ->
    R.

j( #?MODULE:my_native_record{}=R ) ->
    R.

k( #?MODULE:my_native_record{ x=?x_for_r1 }=R ) ->
    R.


-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    % To ensure that the Myriad parse transform is used (as throws in all cases
    % if not transformed):
    %
    cond_utils:if_defined( non_defined_token, ok ),

    test_native_records(),

    test_facilities:stop().
