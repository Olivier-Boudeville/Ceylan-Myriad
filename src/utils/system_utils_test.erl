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


% Unit tests for the system utils toolbox.
% See the system_utils.erl tested module.
-module(system_utils_test).


-export([ run/0 ]).


-define( Tested_module, system_utils ).



print_sizes( [] ) ->
	ok;

print_sizes( [H|T] ) ->
	
	Size = system_utils:get_size(H),
	
	io:format( "     - exact size of ~p is ~s~n", 
		[ H, system_utils:interpret_byte_size( Size ) ] ),
		
	print_sizes( T ).
	


run() ->

	io:format( "--> Testing module ~s.~n", [ ?Tested_module ] ),
	
	
	% User-related functions.	
	
	io:format( "   Determining what is the name of the current user: ~s.~n",
		[ system_utils:get_user_name() ] ),
				
	io:format( "   Determining what is the home directory of the current user:"
		" ~s.~n", [ system_utils:get_user_home_directory() ] ),
	
	TotalRAM = system_utils:get_total_installed_memory(),
	io:format( "   Determining the total memory installed on this computer:"
		" ~B bytes, which is ~s.~n", 
			  [ TotalRAM, system_utils:interpret_byte_size(TotalRAM) ] ),

	
	% System-related functions.
	
	io:format( "   Determining the current version of the interpreter:"
		" ~s.~n", [ system_utils:get_interpreter_version() ] ),

	io:format( "   Determining the size of a VM word: ~B bytes.~n", 
			  [ system_utils:get_size_of_vm_word() ] ),

	Kilo = 1024,
	Mega = Kilo*Kilo,
	Giga = Kilo*Mega,
	
	SizesToInterpret = [ 0, 1, Kilo-1, Kilo, Kilo+1, 2*Kilo - 1, 2*Kilo,
						2*Kilo + 1, 10000, Mega-1, Mega, Mega+1, 
						10000000, Giga - 1, Giga, Giga + 1, 
						1140328500, 
						Giga + Kilo, Giga + Mega, Giga + Mega + Kilo, 
						2* Giga,
						1234567890123],
		
	io:format( "   Testing size-describing facilities:~n" ),
	
	[ io:format( "    + '~B bytes' translates to: '~s', or "
			  "'~s', in terms of units~n", 
      [X,system_utils:interpret_byte_size(X),
	   system_utils:interpret_byte_size_with_unit(X)] ) 
	 || X <- SizesToInterpret ],
	
  	io:format( "   Evaluating the size in memory of a few terms:~n" ),
	
	AFullSentence = "All human beings are born free and equal in dignity "
		"and rights. They are endowed with reason and conscience and "
		"should act towards one another in a spirit of brotherhood.",
	
	BinaryVersion = text_utils:string_to_binary( AFullSentence ),
										  
	TermsForSize = [ an_atom, 5, "aaa", "aaaa", [], [1], [1,2], {}, {1}, {1,2},
		self(), dict:new(), orddict:new(), hashtable:new(), AFullSentence, 
					BinaryVersion ],
	
	print_sizes( TermsForSize ),
			
	io:format( "  Plain string-binary size ratio for sentence '~s': "
			  "factor x~f.~n",
			  [ AFullSentence, system_utils:get_size(AFullSentence) 
			      / system_utils:get_size(BinaryVersion) ] ),
	
			   
 	io:format( "   Getting memory summary:~n" ),
	system_utils:display_memory_summary(),
						
						
	io:format( "--> End of test for module ~s.~n", [ ?Tested_module ] ),
	erlang:halt().

