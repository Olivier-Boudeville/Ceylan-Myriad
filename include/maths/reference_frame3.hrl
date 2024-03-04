% Copyright (C) 2024-2024 Olivier Boudeville
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
% Creation date: Saturday, March 2, 2024.


% A 3D frame of reference, defining notably a coordinate system.
%
% A shorthand for this record and corresponding type is "ref3".
%
% A parent frame of reference is either explicitly defined (note that it relies
% then on an implicit reference table) or, if not defined, is the global, root,
% absolute one.
%
-record( reference_frame3, {

	% The parent reference frame (if any) of this one, to be found in an
	% (implicit) reference table.
	%
	% A reference frame having no parent defined shall be considered as
	% absolutely defined, as opposed to one having a parent, to which it is then
	% relative.
	%
	parent :: maybe( reference_frame3:ref3_id() ),


	% The 3D transformation (based on 4x4 matrices) from the parent reference
	% frame (if any, otherwise from the root, absolute reference frame), to this
	% one.
	%
	% The reference matrix held by this transformation is the transition matrix
	% from to this reference frame to its parent (if any, otherwise to the
	% global reference frame).
	%
	% Its associated inverse matrix is therefore the transition matrix from the
	% parent reference frame (if any, otherwise from the root, absolute
	% reference frame) to this frame of reference.
	%
	transform :: transform4:transform4() } ).
