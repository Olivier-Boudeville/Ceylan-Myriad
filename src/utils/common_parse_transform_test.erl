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
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)



% This module allows to test the 'common_parse_transform' parse transform as a
% standalone unit, hence with proper error and warning messages.
%
% See the common_parse_transform.erl tested module.
%
-module(common_parse_transform_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	%TargetSourceFile = "graph_utils.erl",
	TargetSourceFile = "../data-management/simple_parse_transform_target.erl",

	test_facilities:display( "Applying the common parse transform to the "
							 "'~s' source file.~n", [ TargetSourceFile ] ),

	TransformedAST = common_parse_transform:run_standalone( TargetSourceFile ),

	test_facilities:display( "Transformed AST:~n~p", [ TransformedAST ] ),

	test_facilities:stop().
