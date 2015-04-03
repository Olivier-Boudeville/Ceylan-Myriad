% Copyright (C) 2014 Olivier Boudeville
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
% Creation date: Tuesday, December 30, 2014



% Describes a function.
%
-record( function_info, {

		   % The name of that function:
		   name = undefined :: meta_utils:function_name(),

		   % The arity of that function:
		   arity = undefined :: arity(),

		   % The abstract form of its definition:
		   definition = undefined :: [ meta_utils:clause_def() ],

		   % The type definition (if any) of that function, as an abstract form:
		   spec = undefined :: meta_utils:function_spec()

} ).
