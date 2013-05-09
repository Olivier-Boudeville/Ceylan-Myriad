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
% Creation date: Monday, February 15, 2010.


% Gathering of various general purpose basic math facilities.
%
% See math_utils_test.erl for the corresponding test.
-module(math_utils).


% General operations.
-export([ floor/1, ceiling/1, modulo/2 ]).


% Operations on floating-point values:
-export([ are_close/2, is_null/1 ]).


% Operations with angles:
-export([ radian_to_degree/1, canonify/1 ]).


% For epsilon define:
-include("math_utils.hrl").



% General section.


% Floors returns the biggest integer smaller than the specified floating-point
% value.
%
% Inspired from http://schemecookbook.org/Erlang/NumberRounding.
floor(X) ->
	T = erlang:trunc(X),
	case (X - T) of

		Neg when Neg < 0 ->
			T - 1;

		%Pos when Pos > 0 ->
		%	T;

		_PositiveOrNull ->
			T

	end.



% Ceiling returns the smallest integer bigger than the specified floating-point
% value.
%
% Inspired from http://schemecookbook.org/Erlang/NumberRounding.
ceiling(X) ->
	T = erlang:trunc(X),
	case (X - T) of

		Pos when Pos > 0 ->
			T + 1;

		%Neg when Neg < 0 ->
		%	T;

		_NegativeOrNull ->
			T

	end.



% Returns the positive remainder of the division of X by Y, in [0;Y[.
%
% In Erlang, -5 rem 3 is -2, whereas this function will return 1,
% since -5 = -2 * 3 + 1.
modulo(X,Y) when X > 0 ->
	X rem Y;

modulo(X,Y) when X < 0 ->
	K = (-X div Y)+1,
	PositiveX = X + K*Y,
	%io:format( "K=~B, PositiveX=~B~n.", [K,PositiveX] ),
	PositiveX rem Y;

modulo(0,_Y) ->
	0.




% Floating-point section.


% Returns true iff the two specified floating-point numbers are deemed close
% enough to be equal.
are_close( X, Y ) ->
	erlang:abs(X-Y) < ?epsilon.


% Returns true iff the specified floating-point number is deemed close enough to
% zero to be null.
is_null( X ) ->
	erlang:abs(X) < ?epsilon.



% Angle section.

% As we try to remain as much as possible with integer computations, for angle
% we tend to prefer expressing them in degrees rather than in radians.

% Angles in degrees are preferably kept in the [0;360[ interval, i.e. as
% positive integers.


% Converts specified angle in radian into the same angle expressed in degrees.
radian_to_degree( AngleInRadians ) ->
	AngleInRadians * 180 / math:pi().


% Canonifies specified angle in degrees, i.e. ensures the returned value that
% corresponds to the specified angle is in the [0;360[ interval.
canonify( AngleInDegrees ) when is_integer(AngleInDegrees) ->
	modulo( AngleInDegrees, 360 );

% Here we assume it is a floating-point value, positive or not.
canonify( AngleInDegrees ) ->
	AngleInDegrees - 360 * floor(AngleInDegrees/360).
