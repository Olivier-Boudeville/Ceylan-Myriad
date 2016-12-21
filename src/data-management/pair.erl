% Copyright (C) 2015-2017 Olivier Boudeville
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
% Creation date: Thursday, April 30, 2015
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% Minor utilities to manage pairs (2-element tuples).
%
-module(pair).

-export([ first/1, second/1, swap/1 ]).

-compile( { inline, [ first/1, second/1, swap/1 ] } ).



-type pair() :: { any(), any() }.

-export_type([ pair/0 ]).


% Returns the first element of specified pair.
%
-spec first( pair() ) -> pair().
first( { X, _Y } ) ->
	X.



% Returns the second element of specified pair.
%
-spec second( pair() ) -> pair().
second( { _X, Y } ) ->
	Y.



% Returns a pair whose elements have been swapped compared to specified one.
%
-spec swap( pair() ) -> pair().
swap( { X, Y } ) ->
	{ Y, X }.
