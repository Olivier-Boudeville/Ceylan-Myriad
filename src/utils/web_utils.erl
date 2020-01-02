% Copyright (C) 2019-2020 Olivier Boudeville
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
% Creation date: Tuesday, June 25, 2019



% Gathering of services related to web content, notably for HTML generation.
%
% See web_utils_test.erl for the corresponding test.
%
-module(web_utils).


-export([ get_unordered_list/1 ]).

% Ex: "<p>Hello!</p>":
-type html_element() :: text_utils:any_string().


-export_type([ html_element/0 ]).


% Returns the HTML code of an unordered list corresponding to specified list of
% elements.
%
-spec get_unordered_list( [ html_element() ] ) -> html_element().
get_unordered_list( Elements ) ->

	HTMLElems = [ text_utils:format( "    <li>~s</li>~n", [ E ] )
				  || E <- Elements ],

	text_utils:format( "  <ul>~n~s  </ul>~n", [ lists:flatten( HTMLElems ) ] ).
