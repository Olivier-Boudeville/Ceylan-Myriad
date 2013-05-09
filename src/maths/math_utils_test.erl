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


% Unit tests for the math basic toolbox facilities.
%
% See the math_utils tested module.
-module(math_utils_test).


-define(Tested_modules,[math_utils]).


% For test_finished/0 and al:
-include("test_facilities.hrl").



run() ->

	io:format( "--> Testing modules ~p.~n", [ ?Tested_modules ] ),

	Roundings = [ -1.1, -1.0, -0.9, 0.0, 0.9, 1.0, 1.1 ],

	[ io:format( "    Floor for ~p is ~p.~n", [ V,
			math_utils:floor(V) ] ) || V <- Roundings ],

	[ io:format( "    Ceiling for ~p is ~p.~n", [ V,
			math_utils:ceiling(V) ] ) || V <- Roundings ],


	Modulo = 3,
	[ io:format( "    ~p modulo ~p is ~p.~n", [ X, Modulo,
			math_utils:modulo(X,Modulo) ] ) || X <- lists:seq(-7,7) ],

	[ io:format( "    Canonical form for ~p degrees is ~p degrees.~n", [ A,
			math_utils:canonify(A) ] ) ||
		A <- [ -721, -721.0, -720, -720.0, -719, -719.0, -100, -100.0,
			   0, 0.0, 100, 100.0, 359, 359.0, 360, 360.0, 361, 361.0,
			   400, 400.0 ] ],

	X1 = 3.0,
	X2 = 3.1,
	X3 = 3.0000000000001,
	Y  = 3.0,

	true  = math_utils:are_close(X1,Y),
	false = math_utils:are_close(X2,Y),
	true  = math_utils:are_close(X3,Y),

	% ° does not output well on the console (ex: "90.000000 Â°."):
	[ io:format( "    Angle ~p rad is ~f degrees.~n", [ Angle,
			math_utils:radian_to_degree( Angle ) ] ) || Angle <-
				   [0,math:pi()/2,1.0,math:pi(), 2*math:pi() ] ],

	test_finished().
