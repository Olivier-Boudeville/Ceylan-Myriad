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
% Creation date: Thursday, February 11, 2010.


% Gathering of various system convenient facilities.
% See system_utils_test.erl for the corresponding test.
-module(system_utils).



% User-related functions.
-export([ get_user_name/0, get_user_home_directory/0 ]).


% System-related functions.
-export([ get_interpreter_version/0, get_size_of_vm_word/0, get_size/1,
		 interpret_byte_size/1, interpret_byte_size_with_unit/1,
		 convert_byte_size_with_unit/1, display_memory_summary/0,
		 get_total_installed_memory/0 ]).



% User-related functions.


% Returns the name of the current user.
get_user_name() ->
	os:getenv("USER").


% Returns the home directory of the current user.
get_user_home_directory() ->
	os:getenv("HOME").



% Returns the total install memory (RAM) of the computer being used, in bytes.
get_total_installed_memory() ->

	% First check the expected unit is returned, by pattern-matching:
	"kB\n" = os:cmd("cat /proc/meminfo|grep 'MemTotal:'|awk '{print $3}'"),

	ValueCommand = "cat /proc/meminfo|grep 'MemTotal:'|awk '{print $2}'",

	% The returned value of following command is like "12345\n", in bytes:
	MemorySizeString = text_utils:remove_ending_carriage_return(
								os:cmd( ValueCommand ) ),

	% They were kB (not kiB):
	list_to_integer(MemorySizeString) * 1000.



% Erlang System-related functions.


% Returns the version informations of the current Erlang interpreter
% being used.
%
% Returns a full version name (ex: "R13B04") or, if not available, a shorter one
% (ex: "R11B").
%
get_interpreter_version() ->
	% Older versions (pre-R13A?) did not support the otp_release tag:
	try erlang:system_info(otp_release) of

		V ->
			% Ex: V="R13B04"
			V

	catch

		_:_ ->
			% Here we revert to another (older) solution:
			{_OTPInfos,V} = init:script_id(),
			% Ex: "R11B"
			V

	end.




% Returns the size, in bytes, of a word of this Virtual Machine.
get_size_of_vm_word() ->
	erlang:system_info(wordsize).


% Returns the size of specified term, in bytes.
get_size( Term ) ->
	erts_debug:flat_size(Term) * get_size_of_vm_word().



% Returns a string containing a user-friendly description of the specified size
% expressed in bytes, using GiB (Gibibytes, not Gigabytes), MiB (Mebibytes, not
% Megabytes), KiB (Kibibytes, not Kilobytes) and bytes.
%
% See http://en.wikipedia.org/wiki/Kibibyte
interpret_byte_size( SizeInBytes ) ->

	Kilo = 1024,
	Mega = Kilo*Kilo,
	Giga = Kilo*Mega,

	ListWithGiga = case SizeInBytes div Giga of

					 0 ->
						 [];

					 GigaNonNull->
						 [io_lib:format( "~B GiB", [GigaNonNull] )]

				   end,
	SizeAfterGiga = SizeInBytes rem Giga,
	%io:format( "SizeAfterGiga = ~B.~n", [SizeAfterGiga] ),

	ListWithMega = case SizeAfterGiga div Mega of

					 0 ->
						 ListWithGiga;

					 MegaNonNull->
						 [io_lib:format( "~B MiB", [MegaNonNull] )|ListWithGiga]

				   end,
	SizeAfterMega = SizeAfterGiga rem Mega,
	%io:format( "SizeAfterMega = ~B.~n", [SizeAfterMega] ),

	ListWithKilo = case SizeAfterMega div Kilo of

					 0 ->
						 ListWithMega;

					 KiloNonNull->
						 [io_lib:format( "~B KiB", [KiloNonNull] )|ListWithMega]

				   end,
	SizeAfterKilo = SizeAfterMega rem Kilo,
	%io:format( "SizeAfterKilo = ~B.~n", [SizeAfterKilo] ),

	ListWithByte = case SizeAfterKilo rem Kilo of

					 0 ->
						ListWithKilo ;

					 1->
						 [ "1 byte" | ListWithKilo ];

					 AtLeastTwoBytes ->
						 [ io_lib:format( "~B bytes", [AtLeastTwoBytes] )
						   | ListWithKilo ]

				   end,

	%io:format( "Unit list is: ~w.~n", [ListWithByte] ),

	case ListWithByte of

		[] ->
			"0 byte";

		[OneElement] ->
			OneElement;

		[Smaller|Bigger] ->
			text_utils:join( ", ", lists:reverse(Bigger) ) ++ " and " ++ Smaller

	end.



% Returns a string containing a user-friendly description of the specified size
% expressed in bytes), using the most appropriate unit among GiB (Gibibytes, not
% Gigabytes), MiB (Mebibytes, not Megabytes), KiB (Kibibytes, not Kilobytes) and
% bytes, rounding that value to 1 figure after the comma (this is thus an
% approximate value).
%
% See http://en.wikipedia.org/wiki/Kibibyte
interpret_byte_size_with_unit( Size ) ->

	{Unit,Value} = convert_byte_size_with_unit( Size ),

	case Unit of

		byte ->

			case Value of

				0 ->
					"0 byte";

				1 ->
					"1 byte";

				Other ->
					io_lib:format( "~B bytes", [Other] )

			end;

		kib ->
			io_lib:format( "~.1f KiB", [Value] );

		mib ->
			io_lib:format( "~.1f MiB", [Value] );

		gib ->
			io_lib:format( "~.1f GiB", [Value] )

	end.



% Converts the specified size, in bytes, as a value expressed in an appropriate
% size unit.
%
% Returns a {Unit,Value} pair, in which:
%
% - Unit is the largest size unit that can be selected so that the specified
% size if worth at least 1 unit of it (ex: we do not want a value 0.9, at least
% 1.0 is wanted); Unit can be 'gib', for GiB (Gibibytes), 'mib', for MiB
% (Mebibytes), 'kib' for KiB (Kibibytes), or 'byte', for Byte
%
% - Value is the converted byte size, in the specified returned unit, expressed
% either as an integer (for bytes) or as a float
%
% Ex: 1023 (bytes) translates to {byte,1023}, 1025 translates to
% {kib,1.0009765625}.
%
% Note that the returned value cannot be expected to be exact (rounded),
% therefore this function is mostly useful for user output.
%
convert_byte_size_with_unit( SizeInBytes ) ->

	Kilo = 1024,
	Mega = Kilo*Kilo,
	Giga = Kilo*Mega,

	case SizeInBytes div Giga of

		0 ->

			case SizeInBytes div Mega of

				0 ->

					case SizeInBytes div Kilo of

						0 ->
							%{byte,float(SizeInBytes)};
							{byte,SizeInBytes};

						_ ->
							{kib,SizeInBytes/Kilo}

					end;

				_ ->
					{mib,SizeInBytes/Mega}

			end;

		_ ->
			{gib,SizeInBytes/Giga}

	end.



% Returns a summary of the dynamically allocated memory currently being
% used by the Erlang emulator.
display_memory_summary() ->
	SysSize  = erlang:memory( system ),
	ProcSize = erlang:memory( processes ),
	Sum = SysSize + ProcSize,
	io:format( "  - system size: ~s (~s)~n",
			  [ interpret_byte_size_with_unit(SysSize),
			   text_utils:percent_to_string(SysSize/Sum) ] ),
	io:format( "  - process size: ~s (~s)~n",
			  [ interpret_byte_size_with_unit(ProcSize),
			   text_utils:percent_to_string(ProcSize/Sum) ] ).
