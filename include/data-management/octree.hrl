% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Saturday, June 4, 2022.

-record( octants, { o1 :: maybe( octree:octree() ),
					o2 :: maybe( octree:octree() ),
					o3 :: maybe( octree:octree() ),
					o4 :: maybe( octree:octree() ),
					o5 :: maybe( octree:octree() ),
					o6 :: maybe( octree:octree() ),
					o7 :: maybe( octree:octree() ),
					o8 :: maybe( octree:octree() ) } ).
% The 8 sub-octrees (children cells) that any octree node may reference.
%
% In a conventional MyriadGUI absolute referential, space is divided that way:
%
%
%  Z
%  ^
%  |    Y
%  |  /
%  | /
%  |/
%  + ---------> X
%  O
%
% Octants are numbered according to two layers, the top one (Z>=0, child O1 to
% O4) and the bottom one (Z<0, child O5 to O8). In each layer, the children are
% listed in countercloocwise order, when considering the Z axis, starting from
% the one whose left side is then along the +X axis.
%
% So, looking from the axis Z downward to the origin, when the obserser is above
% plane OXY (upper half-space):
%
%        ^ Y
%        |
%        |     . Z
%        |
%     O3 | O4
% -------O-------> X
%     O2 | O1
%        |
%        |
%
%
% Looking from the axis Z downward to the origin, when the obserser is lower,
% below plane OXY (lower half-space):
%
%        ^ Y
%        |
%        |     . Z
%        |
%     O7 | O8
% -------O-------> X
%     O6 | O5
%        |
%        |


-record( concurrent_octants, {
	o1 :: maybe( octree:octree_pid() ),
	o2 :: maybe( octree:octree_pid() ),
	o3 :: maybe( octree:octree_pid() ),
	o4 :: maybe( octree:octree_pid() ),
	o5 :: maybe( octree:octree_pid() ),
	o6 :: maybe( octree:octree_pid() ),
	o7 :: maybe( octree:octree_pid() ),
	o8 :: maybe( octree:octree_pid() ) } ).
% The 8 sub-concurrent octrees (children cell processes) that any octree node
% may reference.
%
% See the octants record for more details.
