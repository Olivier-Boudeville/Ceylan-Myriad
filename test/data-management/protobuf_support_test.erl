% Copyright (C) 2021-2021 Olivier Boudeville
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
% Creation date: Saturday, November 6, 2021.


% Unit tests for the <b>Protocol Buffer</b> facilities, a.k.a. Protobuf.
%
% See the protobuf_support.erl tested module.
%
% If running directly with the makefile system (i.e. not from an OTP/rebar3
% context), see, in GNUmakevars.inc, the USE_{PROTOBUF,GPB} variables to
% enable/disable Protobuff support and/or backend ones.
%
-module(protobuf_support_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-ifdef(myriad_uses_protobuf).


-include("myriad_example.hrl").

test_protobuf() ->

	% See thus example.proto and the generate example.[eh]rl:
	ProtobuffSpec = "myriad_example",

	test_facilities:display( "Generating Protobuf messages from the "
							 "'~ts' specification.", [ ProtobuffSpec ] ),

	BlankInstance = #myriad_protobuf_test_person{},

	test_facilities:display( "Blank instance of the "
		"#myriad_protobuf_test_person record: ~p", [ BlankInstance ] ),

	Name = <<"James Pond">>,
	EmailAddress = <<"james@mi6.org">>,

	SetInstance = BlankInstance#myriad_protobuf_test_person{
						name=Name, id=7, email=EmailAddress },

	TermSize = system_utils:get_size( SetInstance ),

	MinSize = lists:sum( [ system_utils:get_size( T )
		|| T <- [ myriad_protobuf_test_person, Name, 7, EmailAddress ] ] ),

	test_facilities:display( "Set instance: ~p, whose overall size is ~ts, "
		"its elements being of total size of ~ts.",
		[ SetInstance, system_utils:interpret_byte_size( TermSize ),
		  system_utils:interpret_byte_size( MinSize ) ] ),

	SetBin = myriad_example:encode_msg( SetInstance ),

	SetBinSize = system_utils:get_size( SetBin ),

	% Actually negative:
	Overhead = SetBinSize - TermSize,

	% As this serialised form is more compact:
	test_facilities:display( "Serialisation for this instance: ~p "
		"whose size is ~ts, for a gain (size decrease) of ~ts.",
		[ SetBin, system_utils:interpret_byte_size( SetBinSize ),
		  system_utils:interpret_byte_size( -Overhead ) ] ),

	DecodedInstance = myriad_example:decode_msg( SetBin,
		_MsgName=myriad_protobuf_test_person ),

	DecodedTermSize = system_utils:get_size( DecodedInstance ),

	test_facilities:display( "Deserialised instance: ~p, "
		"whose overall size is ~ts", [ DecodedInstance,
			system_utils:interpret_byte_size( DecodedTermSize ) ] ).


-else. % myriad_uses_protobuf


test_protobuf() ->

	test_facilities:display( "No Protocol Buffer support enabled (see USE_PROTOBUF in GNUmakevars.inc), hence no test thereof." ).


-endif. % myriad_uses_protobuf


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_protobuf(),

	test_facilities:stop().
