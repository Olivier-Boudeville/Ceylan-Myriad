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


% All tests should export such a function:
-export([ run/0 ]).


% Comment out to be able to use the interpreter after the test:
-define(ExitAfterTest,).


-ifdef(ExitAfterTest).

test_finished() ->
	io:format( "--> End of test for modules ~p.~n", [ ?Tested_modules ] ),
	% Cannot be here as we are not in WOOPER: check_pending_wooper_results(),
	io:format( "(test finished, interpreter halted)~n" ),
	% To ensure all outputs are indeed performed before the VM is halted:
	timer:sleep(500),
	erlang:halt().

-else.

test_finished() ->
	io:format( "--> End of test for modules ~p.~n", [ ?Tested_modules ] ),
	% Cannot be here as we are not in WOOPER: check_pending_wooper_results(),
	io:format( "(test finished, interpreter still running)~n" ),
	test_success.

-endif.
