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
% Creation date: Saturday, February 20, 2010.
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)


% Generic option list implementation, loosely based on proplist.
% See option_list_test.erl for the corresponding test.

% An option list is basically a list containing key/value pairs, keys being
% generally atoms, values being any Erlang term.
%
% In an option list, usually no duplicate keys are expected to exist.
% Operations on option list tend to preserve the order of their entries.

-module(option_list).

-export([ set/2, get/2, lookup/2, update_with/2 ]).



% Note: our option lists only have {Key,Value} pairs (ex: no entry is made of a
% single atom.



% Sets in specified option list the specified entry.
% Returns an updated option list.

% The first previously existing entry found with Key (if any) is replaced 'in
% place' by this entry.
%
% If none is found, the specified entry is put as first element.
set( Entry, OptionList ) ->
	set( Entry, OptionList, _Acc=[] ).


set( Entry, _OptionList=[], Acc ) ->
	% Here no key matched:
	[Entry|lists:reverse(Acc)];

set( Entry = {Key,_Value}, [{Key,_AnyValue}|T], Acc ) ->
	% Same key found, recursion is over:
	lists:reverse(Acc) ++ [Entry|T];

set( Entry, [NonMatchingEntry|T], Acc ) ->
	% Different key found:
	set( Entry, T, [NonMatchingEntry|Acc] ).


% Returns the value associated to the specified key in specified option list.
%
% Throws an exception if an entry with that key could not be found.
get( Key, OptionList ) ->

	case proplists:get_value( Key, OptionList ) of

		undefined ->
			throw( {key_not_found,Key,OptionList} );

		Value ->
			Value

	end.


% Returns the value associated to the specified key in specified option list, if
% found, otherwise (key not found), returns 'undefined'.
lookup( Key, OptionList ) ->
	proplists:get_value( Key, OptionList ).


% Updates BaseOptionList with the entries of UpdatingOptionList.
%
% Merges the two specified option lists into the returned one, knowing that all
% entries found with the same key in both option lists will end up with the
% value defined in the second, UpdatingOptionList.
update_with( BaseOptionList, _UpdatingOptionList=[] ) ->
	BaseOptionList;

update_with( BaseOptionList, [H|T] ) ->
	update_with( set(H,BaseOptionList), T ).
