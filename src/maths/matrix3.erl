% Copyright (C) 2021-2021 Olivier Boudeville
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
% Creation date: Friday, October 8, 2021.


% @doc Module implementing the support for <b>3x3 matrices</b>.
%
% See also:
% - the corresponding (3D) vectors, in vector3.erl
% - the (unspecialised) matrices of arbitrary dimensions, in matrix.erl
%
-module(matrix3).




% Implementation notes:
%


% For printout_*, inline_size, etc.:
-include("linear.hrl").

-compile( inline ).
-compile( { inline_size, ?inline_size } ).


% For records like matrix3:
-include("matrix3.hrl").


% Alias for 3x3 canonical matrices:
-type matrix3() :: #matrix3{}.
-type canonical_matrix() :: matrix3().


% Aliases for 3x3 compact matrices:
-type compact_matrix3() :: #compact_matrix3{}.
-type compact_matrix() :: compact_matrix3().


-type matrix() :: 'identity_3' | canonical_matrix() | compact_matrix().


-export_type([ matrix3/0, canonical_matrix/0,
			   compact_matrix3/0, compact_matrix/0, matrix/0 ]).


%-export_type([ matrix3/0 ]).


-export([
		  %new/1, null/0, identity/1,
		  %dimensions/1, row/2, column/2,
		  %get_element/3, set_element/4,
		  %transpose/1,
		  %add/2,
		  %unspecialise/1,
		  %to_string/1, to_basic_string/1, to_user_string/1
 ] ).


% Shorthands:

%-type ustring() :: text_utils:ustring().

%-type coordinate() :: linear:coordinate().

%-type vector3() :: vector3:vector3().
